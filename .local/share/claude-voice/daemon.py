#!/usr/bin/env python3
"""Claude-voice daemon: wake word detection + voice capture + query pipeline.

Listens for the wake word (or an external trigger via the control socket),
captures speech, transcribes it with Whisper, and runs queries directly
via the query pipeline helpers in claude-ask/query.py.
"""

import asyncio
import json
import logging
import os
import re
import sys
import threading
import time
from pathlib import Path

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    datefmt="%H:%M:%S",
)
log = logging.getLogger("claude-voice")


# ---------------------------------------------------------------------------
# Module imports (local)
# ---------------------------------------------------------------------------

from audio import RATE, VAD_FRAME_MS, AudioPipeline
from config import load_config, load_whisper_config
from notify import notify_ack, notify_dismiss, notify_listening, notify_sending, notify_transcription
from speech_detector import SpeechEndDetector
from stt import transcribe
from wake_word import WakeWordListener

# Add claude-ask helpers to import path
sys.path.insert(0, str(Path.home() / ".local" / "share" / "claude-ask"))
from query import send_query, NEW_CONVERSATION


# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

# Strip wake word remnants ("ok computer", "computer") from transcription start
_WAKE_WORD_RE = re.compile(r'^(ok\s+)?computer[.,!?\s:]*', re.IGNORECASE)


def get_control_socket_path():
    """Return path to the claude-voice control socket."""
    runtime_dir = os.environ.get("XDG_RUNTIME_DIR", f"/run/user/{os.getuid()}")
    return os.path.join(runtime_dir, "claude-voice.sock")


# ---------------------------------------------------------------------------
# Daemon
# ---------------------------------------------------------------------------

class Daemon:
    """Main daemon: wake word detection loop with speech capture and STT."""

    def __init__(self):
        self.config = load_config()
        self.whisper_config = load_whisper_config()

        ww_cfg = self.config["wake_word"]
        sp_cfg = self.config["speech"]

        self.wake_word = WakeWordListener(
            model_path=ww_cfg["model_path"],
            threshold=ww_cfg["threshold"],
        )
        self.audio = AudioPipeline(
            pre_roll_seconds=ww_cfg["pre_roll_seconds"],
            input_device=self.whisper_config.get("input_device"),
        )
        self.detector = SpeechEndDetector(
            silence_timeout=sp_cfg["silence_timeout"],
            smart_silence=sp_cfg["smart_silence"],
            force_send_phrases=sp_cfg["force_send_phrases"],
        )

        # External listen requests (set by control socket)
        self._listen_request = None  # dict or None
        self._muted = False  # When True, wake word detections are ignored
        self._lock = threading.Lock()
        self._cancel_event: threading.Event | None = None
        self._cancel_lock = threading.Lock()

    # ------------------------------------------------------------------
    # Control socket interface
    # ------------------------------------------------------------------

    def request_listen(self, conversation_id=None):
        """Queue a listen request (called from control socket handler)."""
        with self._lock:
            self._listen_request = {"conversation_id": conversation_id}
        log.info("Listen request queued (conv=%s)", conversation_id or "new")

    def set_muted(self, muted: bool):
        """Mute or unmute wake word detection."""
        with self._lock:
            was_muted = self._muted
            self._muted = muted
        if was_muted and not muted:
            # Reset wake word model state so it doesn't carry over
            # activations from before the mute. No audio.flush() here —
            # the main loop reads continuously while muted so there's no
            # stale data, and flush() from this thread would race with
            # the main thread's blocking reads on the same pipe fd.
            self.wake_word.reset()
        log.info("Wake word %s", "muted" if muted else "unmuted")

    def _check_listen_request(self):
        """Check and consume a pending listen request. Returns dict or None."""
        with self._lock:
            req = self._listen_request
            self._listen_request = None
            return req

    # ------------------------------------------------------------------
    # Query dispatch
    # ------------------------------------------------------------------

    def _start_query_thread(self, text, conversation_id=None, image=None, file=None):
        """Spawn a query in a background thread with a cancel_event."""
        cancel_event = threading.Event()
        with self._cancel_lock:
            # Cancel any existing query first
            if self._cancel_event is not None:
                self._cancel_event.set()
            self._cancel_event = cancel_event

        def _run():
            try:
                send_query(text, conversation_id=conversation_id,
                           cancel_event=cancel_event, image=image, file=file)
            finally:
                with self._cancel_lock:
                    if self._cancel_event is cancel_event:
                        self._cancel_event = None

        thread = threading.Thread(target=_run, daemon=True)
        thread.start()
        conv_label = "new" if conversation_id is NEW_CONVERSATION else (conversation_id or "auto")
        log.info("Query thread started (conv=%s)", conv_label)

    def cancel_query(self):
        """Cancel the currently running query."""
        with self._cancel_lock:
            if self._cancel_event is not None:
                self._cancel_event.set()
                log.info("Query cancelled")
            else:
                log.info("No query to cancel")

    # ------------------------------------------------------------------
    # Post-capture cleanup
    # ------------------------------------------------------------------

    def _post_capture_reset(self):
        """Flush stale pipe audio and reset the wake word model.

        During transcription + sleep, pw-record keeps writing to the
        pipe. This stale audio would be fed to the wake word model on
        the next loop iteration, contaminating its state. Flushing the
        pipe and resetting the model ensures immediate wake word
        responsiveness after a capture cycle.
        """
        self.audio.flush()
        self.wake_word.reset()
        log.debug("Post-capture reset: flushed audio, reset wake word model")

    # ------------------------------------------------------------------
    # Main loop
    # ------------------------------------------------------------------

    def run_wake_word_loop(self):
        """Main loop: detect wake word or external trigger, then capture speech."""
        self.audio.start()
        log.info("Wake word loop started")
        try:
            while True:
                # Check for external listen request first
                req = self._check_listen_request()
                if req is not None:
                    notify_ack()
                    self._capture_and_send(conversation_id=req.get("conversation_id"))
                    continue

                # Read audio and check for wake word
                chunk = self.audio.read_oww_chunk()
                if self._muted:
                    continue
                if self.wake_word.detect(chunk):
                    notify_ack()
                    self._capture_and_send(skip_pre_roll=True)
        except KeyboardInterrupt:
            log.info("Interrupted")
        finally:
            self.audio.stop()
            log.info("Audio pipeline stopped")

    # ------------------------------------------------------------------
    # Speech capture
    # ------------------------------------------------------------------

    def _capture_and_send(self, conversation_id=None, skip_pre_roll=False):
        """Capture speech, transcribe, and send query."""
        notify_listening()
        self.audio.begin_capture(skip_pre_roll=skip_pre_roll)
        self.detector.on_speech_start()
        # Track silence in audio time (frame count) instead of wall-clock
        # time.  Transcription blocks the frame-reading loop for ~1s, so
        # wall-clock elapsed time after a reset is near-zero when reading
        # buffered frames — making silence detection impossible.
        silence_frames = 0
        consecutive_speech = 0  # debounce: require 2+ to reset silence
        last_interim_time = 0
        capture_start = time.monotonic()
        heard_speech = False
        no_speech_timeout = self.config["speech"]["no_speech_timeout"]
        max_capture_seconds = self.config["speech"].get("max_capture_seconds", 60)
        last_transcript = ""
        transcript_stall_count = 0

        try:
            while True:
                raw, is_speech = self.audio.read_vad_frame()
                self.detector.on_voice_activity(is_speech)

                # Debounced speech detection: require 2+ consecutive speech
                # frames to reset silence counter.  A single noisy frame in
                # ambient audio should not restart the timer.
                if is_speech:
                    consecutive_speech += 1
                    if consecutive_speech >= 2:
                        silence_frames = 0
                else:
                    consecutive_speech = 0
                    silence_frames += 1

                # Interim transcription every ~2 seconds
                now = time.monotonic()
                if now - last_interim_time >= 2.0:
                    full_audio = self.audio.get_captured_audio()
                    if len(full_audio) > 0:
                        interim_text = transcribe(full_audio, self.whisper_config)
                        self.detector.update_transcript(interim_text)
                        notify_transcription(interim_text)
                        if interim_text.strip():
                            heard_speech = True

                        # Transcript stall detection: if the transcript is
                        # unchanged for 3+ consecutive transcriptions (~6s),
                        # the user has stopped speaking even if VAD still
                        # sees ambient noise.
                        if heard_speech and interim_text.strip() == last_transcript:
                            transcript_stall_count += 1
                            if transcript_stall_count >= 3:
                                log.info("Transcript unchanged for %d iterations, ending capture",
                                         transcript_stall_count)
                                break
                        else:
                            transcript_stall_count = 0
                        last_transcript = interim_text.strip()
                    last_interim_time = now

                    # Bail if no words detected after timeout
                    if not heard_speech and (now - capture_start) >= no_speech_timeout:
                        log.info("No speech detected after %.1fs, discarding",
                                 now - capture_start)
                        self.audio.end_capture()
                        self._post_capture_reset()
                        notify_dismiss()
                        return

                    # Check for force-send phrase
                    phrase = self.detector.check_force_send()
                    if phrase:
                        log.info("Force-send phrase detected: %r", phrase)
                        final_audio = self.audio.end_capture()
                        final_text = transcribe(final_audio, self.whisper_config)
                        final_text = SpeechEndDetector.strip_force_phrase(final_text, phrase)
                        if skip_pre_roll:
                            final_text = _WAKE_WORD_RE.sub('', final_text).strip()
                        if final_text.strip():
                            notify_sending()
                            self._start_query_thread(final_text.strip(), conversation_id)
                        else:
                            notify_dismiss()
                        self._post_capture_reset()
                        return

                # Hard limit on capture duration
                if (now - capture_start) >= max_capture_seconds:
                    log.info("Max capture duration reached (%.0fs)", max_capture_seconds)
                    break

                # Check silence timeout (using audio time, not wall-clock)
                silence_seconds = silence_frames * (VAD_FRAME_MS / 1000)
                if silence_seconds > 0 and self.detector.is_done(silence_seconds):
                    log.info("Silence timeout reached (%.1fs audio time)", silence_seconds)
                    break

        except Exception:
            log.exception("Error during speech capture")
            self.audio.end_capture()
            self._post_capture_reset()
            notify_dismiss()
            return

        # Final transcription
        final_audio = self.audio.end_capture()
        if len(final_audio) < RATE * 0.3:  # < 0.3s = probably false trigger
            log.info("Audio too short (%.2fs), discarding", len(final_audio) / RATE)
            self._post_capture_reset()
            notify_dismiss()
            return

        final_text = transcribe(final_audio, self.whisper_config)
        # Strip wake word remnants (e.g. "computer.") from transcription
        if skip_pre_roll:
            final_text = _WAKE_WORD_RE.sub('', final_text).strip()
        if final_text.strip():
            notify_transcription(final_text.strip())
            time.sleep(3)
            notify_sending()
            self._start_query_thread(final_text.strip(), conversation_id)
        else:
            log.info("Empty transcription, discarding")
            notify_dismiss()
        self._post_capture_reset()


# ---------------------------------------------------------------------------
# Control socket server (asyncio, runs in background thread)
# ---------------------------------------------------------------------------

async def handle_control_client(daemon, reader, writer):
    """Handle a single control socket connection."""
    try:
        chunks = []
        while True:
            chunk = await reader.read(65536)
            if not chunk:
                break
            chunks.append(chunk)
        data = b"".join(chunks)
        if not data:
            return

        msg = json.loads(data.decode("utf-8"))
        action = msg.get("action")

        if action == "listen":
            daemon.request_listen(conversation_id=msg.get("conversation_id"))
            log.info("Control socket: listen (conv=%s)", msg.get("conversation_id") or "new")
        elif action == "mute":
            daemon.set_muted(True)
        elif action == "unmute":
            daemon.set_muted(False)
        elif action == "toggle-mute":
            with daemon._lock:
                new_state = not daemon._muted
            daemon.set_muted(new_state)
            # Send back the new state
            try:
                writer.write(json.dumps({"muted": new_state}).encode("utf-8"))
                await writer.drain()
            except Exception:
                pass
        elif action == "get-mute":
            with daemon._lock:
                muted = daemon._muted
            try:
                writer.write(json.dumps({"muted": muted}).encode("utf-8"))
                await writer.drain()
            except Exception:
                pass
        elif action == "query":
            text = msg.get("text", "").strip()
            if not text:
                log.warning("Control socket: empty query text")
            else:
                raw_conv = msg.get("conversation_id")
                if raw_conv == "__new__":
                    conv_id = NEW_CONVERSATION
                else:
                    conv_id = raw_conv  # str or None
                image = msg.get("image")
                file = msg.get("file")
                daemon._start_query_thread(text, conversation_id=conv_id,
                                            image=image, file=file)
                log.info("Control socket: query (conv=%s)", raw_conv or "auto")
        elif action == "cancel":
            daemon.cancel_query()
            log.info("Control socket: cancel")
        elif action == "stop_tts":
            from query import _get_tts, _get_waybar, _voice_control
            _get_tts().stop()
            _voice_control("unmute")
            with daemon._cancel_lock:
                query_active = daemon._cancel_event is not None
            if query_active:
                _get_waybar().set_status("thinking")
            else:
                _get_waybar().set_status("idle")
            log.info("Control socket: stop_tts (query_active=%s)", query_active)
        else:
            log.warning("Control socket: unknown action %r", action)

    except json.JSONDecodeError as e:
        log.error("Control socket: invalid JSON: %s", e)
    except Exception:
        log.exception("Control socket: error handling client")
    finally:
        writer.close()
        await writer.wait_closed()


async def run_control_socket(daemon):
    """Start the asyncio control socket server."""
    sock_path = get_control_socket_path()

    # Clean up stale socket
    try:
        os.unlink(sock_path)
    except FileNotFoundError:
        pass

    server = await asyncio.start_unix_server(
        lambda r, w: handle_control_client(daemon, r, w),
        path=sock_path,
    )
    os.chmod(sock_path, 0o600)
    log.info("Control socket listening on %s", sock_path)

    async with server:
        await server.serve_forever()


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

def main():
    daemon = Daemon()

    # Start control socket server in a background thread
    control_thread = threading.Thread(
        target=lambda: asyncio.run(run_control_socket(daemon)),
        daemon=True,
    )
    control_thread.start()

    # Run the wake word detection loop on the main thread (blocking)
    daemon.run_wake_word_loop()


if __name__ == "__main__":
    main()
