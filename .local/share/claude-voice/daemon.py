#!/usr/bin/env python3
"""Claude-voice daemon: wake word detection + voice capture + STT.

Listens for the wake word (or an external trigger via the control socket),
captures speech, transcribes it with Whisper, and sends the text to the
claude-ask daemon over its Unix socket.
"""

import asyncio
import json
import logging
import os
import socket
import threading
import time

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    datefmt="%H:%M:%S",
)
log = logging.getLogger("claude-voice")


# ---------------------------------------------------------------------------
# Module imports (local)
# ---------------------------------------------------------------------------

from audio import RATE, AudioPipeline
from config import load_config, load_whisper_config
from notify import notify_dismiss, notify_listening, notify_sending, notify_transcription
from speech_detector import SpeechEndDetector
from stt import transcribe
from wake_word import WakeWordListener


# ---------------------------------------------------------------------------
# Socket path helpers
# ---------------------------------------------------------------------------

def get_control_socket_path():
    """Return path to the claude-voice control socket."""
    runtime_dir = os.environ.get("XDG_RUNTIME_DIR", f"/run/user/{os.getuid()}")
    return os.path.join(runtime_dir, "claude-voice.sock")


def get_claude_ask_socket_path():
    """Return path to the claude-ask daemon socket."""
    runtime_dir = os.environ.get("XDG_RUNTIME_DIR", f"/run/user/{os.getuid()}")
    return os.path.join(runtime_dir, "claude-ask.sock")


# ---------------------------------------------------------------------------
# Send transcribed text to claude-ask
# ---------------------------------------------------------------------------

def send_to_claude_ask(text, conversation_id=None):
    """Send transcribed text to the claude-ask daemon via its Unix socket."""
    sock_path = get_claude_ask_socket_path()
    payload = {"text": text}
    if conversation_id:
        payload["conversation_id"] = conversation_id

    log.info("Sending to claude-ask: %r (conv=%s)", text[:80], conversation_id or "new")
    try:
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        sock.connect(sock_path)
        sock.sendall(json.dumps(payload).encode("utf-8"))
        sock.close()
        log.info("Sent successfully")
    except Exception:
        log.exception("Failed to send to claude-ask at %s", sock_path)


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
        )
        self.detector = SpeechEndDetector(
            silence_timeout=sp_cfg["silence_timeout"],
            smart_silence=sp_cfg["smart_silence"],
            force_send_phrases=sp_cfg["force_send_phrases"],
        )

        # External listen requests (set by control socket)
        self._listen_request = None  # dict or None
        self._lock = threading.Lock()

    # ------------------------------------------------------------------
    # Control socket interface
    # ------------------------------------------------------------------

    def request_listen(self, conversation_id=None):
        """Queue a listen request (called from control socket handler)."""
        with self._lock:
            self._listen_request = {"conversation_id": conversation_id}
        log.info("Listen request queued (conv=%s)", conversation_id or "new")

    def _check_listen_request(self):
        """Check and consume a pending listen request. Returns dict or None."""
        with self._lock:
            req = self._listen_request
            self._listen_request = None
            return req

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
                    self._capture_and_send(conversation_id=req.get("conversation_id"))
                    continue

                # Read audio and check for wake word
                chunk = self.audio.read_oww_chunk()
                if self.wake_word.detect(chunk):
                    self._capture_and_send()
        except KeyboardInterrupt:
            log.info("Interrupted")
        finally:
            self.audio.stop()
            log.info("Audio pipeline stopped")

    # ------------------------------------------------------------------
    # Speech capture
    # ------------------------------------------------------------------

    def _capture_and_send(self, conversation_id=None):
        """Capture speech, transcribe, and send to claude-ask."""
        notify_listening()
        self.audio.begin_capture()
        self.detector.on_speech_start()
        silence_start = None
        last_interim_time = 0

        try:
            while True:
                raw, is_speech = self.audio.read_vad_frame()
                self.detector.on_voice_activity(is_speech)

                if is_speech:
                    silence_start = None
                elif silence_start is None:
                    silence_start = time.monotonic()

                # Interim transcription every ~2 seconds
                now = time.monotonic()
                if now - last_interim_time >= 2.0:
                    full_audio = self.audio.get_captured_audio()
                    if len(full_audio) > 0:
                        interim_text = transcribe(full_audio, self.whisper_config)
                        self.detector.update_transcript(interim_text)
                        notify_transcription(interim_text)
                    last_interim_time = now

                    # Check for force-send phrase
                    phrase = self.detector.check_force_send()
                    if phrase:
                        log.info("Force-send phrase detected: %r", phrase)
                        final_audio = self.audio.end_capture()
                        final_text = transcribe(final_audio, self.whisper_config)
                        final_text = SpeechEndDetector.strip_force_phrase(final_text, phrase)
                        if final_text.strip():
                            notify_sending()
                            send_to_claude_ask(final_text.strip(), conversation_id)
                        else:
                            notify_dismiss()
                        return

                # Check silence timeout
                if silence_start is not None:
                    elapsed = time.monotonic() - silence_start
                    if self.detector.is_done(elapsed):
                        log.info("Silence timeout reached (%.1fs)", elapsed)
                        break

        except Exception:
            log.exception("Error during speech capture")
            self.audio.end_capture()
            notify_dismiss()
            return

        # Final transcription
        final_audio = self.audio.end_capture()
        if len(final_audio) < RATE * 0.3:  # < 0.3s = probably false trigger
            log.info("Audio too short (%.2fs), discarding", len(final_audio) / RATE)
            notify_dismiss()
            return

        notify_sending()
        final_text = transcribe(final_audio, self.whisper_config)
        if final_text.strip():
            send_to_claude_ask(final_text.strip(), conversation_id)
        else:
            log.info("Empty transcription, discarding")
            notify_dismiss()


# ---------------------------------------------------------------------------
# Control socket server (asyncio, runs in background thread)
# ---------------------------------------------------------------------------

async def handle_control_client(daemon, reader, writer):
    """Handle a single control socket connection."""
    try:
        data = await reader.read(4096)
        if not data:
            return

        msg = json.loads(data.decode("utf-8"))
        action = msg.get("action")

        if action == "listen":
            daemon.request_listen(conversation_id=msg.get("conversation_id"))
            log.info("Control socket: listen (conv=%s)", msg.get("conversation_id") or "new")
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
