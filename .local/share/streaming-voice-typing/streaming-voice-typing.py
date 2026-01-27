#!/usr/bin/env python3
"""
Streaming voice typing using Vosk.

Key design: partial results are shown in a notification overlay for visual
feedback (like Android's gray tentative text), but ONLY final results are
typed via ydotool.  This avoids the backspace-and-rewrite problem that
nerd-dictation had — no stray keystrokes if you switch windows mid-dictation.

Usage:
    python streaming-voice-typing.py [--model-path PATH] [--sample-rate N]

Toggle pause/resume by sending SIGUSR1 to this process.
"""

import argparse
import json
import os
import re
import signal
import subprocess
import sys
import time

import vosk
import pyaudio

# ---------------------------------------------------------------------------
# Defaults
# ---------------------------------------------------------------------------

SAMPLE_RATE = 16000
CHUNK_SIZE = 4000  # ~250 ms at 16 kHz

RUNTIME_DIR = os.environ.get("XDG_RUNTIME_DIR", "/tmp")
STATE_FILE = os.path.join(RUNTIME_DIR, "voice-typing-stream-state")
YDOTOOL_SOCKET = os.environ.get(
    "YDOTOOL_SOCKET", f"/run/user/{os.getuid()}/.ydotool_socket"
)

DEFAULT_MODEL = os.path.expanduser(
    os.environ.get(
        "VOSK_MODEL_PATH",
        "~/.local/share/vosk-models/vosk-model-en-us-0.22",
    )
)

# ---------------------------------------------------------------------------
# Global state
# ---------------------------------------------------------------------------

paused: bool = True  # start paused; user activates with keybinding
running: bool = True
recognizer: vosk.KaldiRecognizer | None = None

# ---------------------------------------------------------------------------
# Spoken-punctuation table
# ---------------------------------------------------------------------------

SPOKEN_PUNCTUATION = [
    ("period", "."),
    ("full stop", "."),
    ("comma", ","),
    ("question mark", "?"),
    ("exclamation mark", "!"),
    ("exclamation point", "!"),
    ("colon", ":"),
    ("semicolon", ";"),
    ("dash", " —"),
    ("hyphen", "-"),
    ("new line", "\n"),
    ("new paragraph", "\n\n"),
    ("open paren", "("),
    ("close paren", ")"),
    ("open quote", '"'),
    ("close quote", '"'),
    ("ellipsis", "..."),
]

# Pre-compile a single regex that matches any spoken-punctuation phrase.
_punct_pattern = re.compile(
    "|".join(re.escape(word) for word, _ in SPOKEN_PUNCTUATION),
    flags=re.IGNORECASE,
)
_punct_map = {word.lower(): symbol for word, symbol in SPOKEN_PUNCTUATION}


def punctuate(text: str) -> str:
    """Basic auto-punctuation for Vosk final results."""
    if not text:
        return text

    # Spoken punctuation  →  symbols
    def _replace(m: re.Match) -> str:
        return _punct_map[m.group(0).lower()]

    text = _punct_pattern.sub(_replace, text)

    # Remove stray spaces before punctuation marks
    text = re.sub(r"\s+([.!?,;:)\"])", r"\1", text)

    # Capitalize first character
    text = text[0].upper() + text[1:]

    # Capitalize standalone "i"
    text = re.sub(r"\bi\b", "I", text)

    # Capitalize after sentence-ending punctuation
    text = re.sub(
        r"([.!?])\s+(\w)",
        lambda m: m.group(1) + " " + m.group(2).upper(),
        text,
    )

    return text


# ---------------------------------------------------------------------------
# Output helpers
# ---------------------------------------------------------------------------


def type_text(text: str) -> None:
    """Inject text via ydotool."""
    if not text:
        return
    env = os.environ.copy()
    env["YDOTOOL_SOCKET"] = YDOTOOL_SOCKET
    try:
        subprocess.run(
            ["ydotool", "type", "--", text],
            env=env,
            timeout=5,
            capture_output=True,
        )
    except (subprocess.TimeoutExpired, FileNotFoundError) as exc:
        print(f"ydotool error: {exc}", file=sys.stderr)


def notify(title: str, body: str, timeout: int = 2000) -> None:
    try:
        subprocess.Popen(
            ["notify-send", "-t", str(timeout), title, body],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
    except FileNotFoundError:
        pass


def show_partial(text: str) -> None:
    """Update the notification overlay with current partial result."""
    if not text:
        return
    try:
        subprocess.Popen(
            [
                "notify-send",
                "-t", "3000",
                "-h", "string:x-canonical-private-synchronous:vosk-partial",
                "hearing...", text,
            ],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
    except FileNotFoundError:
        pass


def clear_partial() -> None:
    try:
        subprocess.Popen(
            [
                "notify-send",
                "-t", "1",
                "-h", "string:x-canonical-private-synchronous:vosk-partial",
                "", "",
            ],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
    except FileNotFoundError:
        pass


def write_state(state: str) -> None:
    try:
        with open(STATE_FILE, "w") as fh:
            fh.write(state)
    except OSError:
        pass


# ---------------------------------------------------------------------------
# Signal handlers
# ---------------------------------------------------------------------------


def _flush_recognizer() -> None:
    """Flush pending audio and type whatever Vosk has buffered."""
    global recognizer
    if recognizer is None:
        return
    result = json.loads(recognizer.FinalResult())
    text = result.get("text", "").strip()
    if text:
        type_text(punctuate(text) + " ")
    clear_partial()


def _handle_toggle(signum, frame):
    global paused
    paused = not paused

    if paused:
        # Flush remaining audio before pausing
        _flush_recognizer()
        write_state("paused")
        notify("Stream Typing", "Paused", timeout=500)
    else:
        write_state("listening")
        notify("Stream Typing", "Listening", timeout=500)


def _handle_shutdown(signum=None, frame=None):
    global running
    running = False


signal.signal(signal.SIGUSR1, _handle_toggle)
signal.signal(signal.SIGTERM, _handle_shutdown)
signal.signal(signal.SIGINT, _handle_shutdown)

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------


def main() -> None:
    global running, recognizer

    parser = argparse.ArgumentParser(description="Streaming voice typing (Vosk)")
    parser.add_argument(
        "--model-path",
        default=DEFAULT_MODEL,
        help="Path to Vosk model directory",
    )
    parser.add_argument(
        "--sample-rate",
        type=int,
        default=SAMPLE_RATE,
        help="Audio sample rate (default: 16000)",
    )
    args = parser.parse_args()

    model_path = os.path.expanduser(args.model_path)

    if not os.path.isdir(model_path):
        print(f"Vosk model not found at {model_path}", file=sys.stderr)
        print(
            "Download the large English model (~1.8 GB):\n"
            f"  mkdir -p {os.path.dirname(model_path)}\n"
            f"  cd {os.path.dirname(model_path)}\n"
            "  wget https://alphacephei.com/vosk/models/vosk-model-en-us-0.22.zip\n"
            "  unzip vosk-model-en-us-0.22.zip",
            file=sys.stderr,
        )
        notify("Stream Typing", "Vosk model not found — see terminal", timeout=5000)
        sys.exit(1)

    # ---- Load model ----
    print(f"Loading Vosk model from {model_path} ...")
    vosk.SetLogLevel(-1)
    model = vosk.Model(model_path)
    recognizer = vosk.KaldiRecognizer(model, args.sample_rate)
    recognizer.SetWords(True)
    print("Model loaded.")

    # ---- Open audio ----
    pa = pyaudio.PyAudio()
    stream = pa.open(
        format=pyaudio.paInt16,
        channels=1,
        rate=args.sample_rate,
        input=True,
        frames_per_buffer=CHUNK_SIZE,
    )

    # Start paused — user activates with keybinding
    write_state("paused")
    notify("Stream Typing", "Ready (paused)", timeout=1500)
    print("Streaming voice typing daemon ready (paused). SIGUSR1 to toggle.")

    last_partial = ""

    try:
        while running:
            if paused:
                time.sleep(0.05)
                continue

            data = stream.read(CHUNK_SIZE, exception_on_overflow=False)

            if recognizer.AcceptWaveform(data):
                # ---- Final result: type it ----
                result = json.loads(recognizer.Result())
                text = result.get("text", "").strip()
                if text:
                    type_text(punctuate(text) + " ")
                clear_partial()
                last_partial = ""
            else:
                # ---- Partial result: overlay only ----
                partial = json.loads(recognizer.PartialResult())
                text = partial.get("partial", "").strip()
                if text and text != last_partial:
                    show_partial(text)
                    last_partial = text
    except Exception as exc:
        print(f"Fatal: {exc}", file=sys.stderr)
    finally:
        _flush_recognizer()
        stream.stop_stream()
        stream.close()
        pa.terminate()
        try:
            os.remove(STATE_FILE)
        except OSError:
            pass
        print("Streaming voice typing stopped.")


if __name__ == "__main__":
    main()
