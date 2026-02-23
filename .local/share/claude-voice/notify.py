"""Desktop notification helpers for the claude-voice daemon.

Thin wrappers around notify-send that use a synchronous hint tag so
notifications update in-place rather than stacking.
"""

import logging
import subprocess

log = logging.getLogger("claude-voice")

TAG = "claude-voice"


def notify(text: str, title: str = "Claude Voice") -> None:
    """Show or update a persistent notification (replaces previous with same tag)."""
    log.debug("notify: %s — %s", title, text)
    subprocess.Popen(
        [
            "notify-send",
            "-t", "0",
            "-h", f"string:x-canonical-private-synchronous:{TAG}",
            "-a", "Claude Voice",
            title, text,
        ],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )


def notify_listening() -> None:
    """Show a 'Listening...' notification."""
    notify("Listening...")


def notify_transcription(text: str | None) -> None:
    """Show live transcription text, or fall back to 'Listening...'."""
    notify(text or "Listening...")


def notify_sending() -> None:
    """Show a 'Sending to Claude...' notification."""
    notify("Sending to Claude...")


def notify_dismiss() -> None:
    """Dismiss the notification by replacing it with a near-instant timeout."""
    log.debug("notify: dismiss")
    subprocess.Popen(
        [
            "notify-send",
            "-t", "1",
            "-h", f"string:x-canonical-private-synchronous:{TAG}",
            "-a", "Claude Voice",
            "Claude Voice", "",
        ],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
