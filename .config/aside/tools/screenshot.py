"""Capture a screenshot of all monitors for visual context."""

import base64
import os
import subprocess
import tempfile
import time

TOOL_SPEC = {
    "name": "screenshot",
    "description": (
        "Capture a screenshot of the user's screen. The image will include all "
        "monitors. After receiving the image, determine which monitor is relevant "
        "to the conversation and focus your answer on that monitor's content.\n\n"
        "IMPORTANT: Always ask the user for permission before calling this tool. "
        "When the user seems to be referring to something on their screen without "
        "providing visual context (e.g. 'help me with this', 'what am I doing wrong', "
        "'I give up'), ask 'Want me to look at your screen?' and only call this "
        "tool if they confirm."
    ),
    "parameters": {
        "type": "object",
        "properties": {},
    },
}


def run() -> dict:
    """Capture full screen via grim, return base64 JPEG."""
    fd, tmp = tempfile.mkstemp(suffix=".jpg")
    os.close(fd)
    try:
        # Run grim via hyprctl dispatch exec so it runs detached from
        # the daemon's process tree.  grim hangs when spawned directly
        # as a child of the aside daemon (Wayland screencopy contention).
        subprocess.run(
            ["hyprctl", "dispatch", "exec",
             f"bash -c 'grim -t jpeg -q 80 {tmp}'"],
            capture_output=True, timeout=5,
        )
        # Wait for grim to write the file (it runs asynchronously).
        for _ in range(40):
            try:
                if os.path.getsize(tmp) > 1000:
                    time.sleep(0.1)  # let grim finish writing
                    break
            except OSError:
                pass
            time.sleep(0.25)
        else:
            return "Error: grim did not produce output within 10 seconds"
        with open(tmp, "rb") as f:
            encoded = base64.b64encode(f.read()).decode()
        return {"type": "image", "media_type": "image/jpeg", "base64": encoded}
    finally:
        try:
            os.unlink(tmp)
        except OSError:
            pass
