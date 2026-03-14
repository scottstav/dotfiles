"""Capture a screenshot of the focused monitor for visual context."""

import base64
import json
import os
import subprocess
import tempfile

TOOL_SPEC = {
    "name": "screenshot",
    "description": (
        "Capture a screenshot of the user's currently focused monitor. "
        "After receiving the image, analyze what's on screen and help "
        "the user with what they're looking at. "
        "Call this tool immediately whenever the user seems to be referring "
        "to something on their screen without providing visual context "
        "(e.g. 'help me with this', 'what am I doing wrong', 'I give up')."
    ),
    "parameters": {
        "type": "object",
        "properties": {},
    },
}


def _focused_output() -> str:
    """Return the wlr output name (e.g. 'eDP-1') of the focused monitor."""
    active = json.loads(subprocess.check_output(
        ["hyprctl", "activewindow", "-j"]))
    mon_id = active.get("monitor", 0)
    monitors = json.loads(subprocess.check_output(
        ["hyprctl", "monitors", "-j"]))
    for m in monitors:
        if m["id"] == mon_id:
            return m["name"]
    # Fallback: first monitor
    return monitors[0]["name"] if monitors else "eDP-1"


def run() -> dict:
    """Capture focused monitor via grim, return base64 JPEG."""
    output = _focused_output()
    fd, tmp = tempfile.mkstemp(suffix=".jpg")
    os.close(fd)
    os.unlink(tmp)  # grim needs to create the file fresh
    try:
        result = subprocess.run(
            ["grim", "-o", output, "-t", "jpeg", "-q", "80", tmp],
            capture_output=True, timeout=10,
            start_new_session=True,
        )
        if result.returncode != 0:
            return f"Error capturing screenshot: {result.stderr.decode()}"
        with open(tmp, "rb") as f:
            encoded = base64.b64encode(f.read()).decode()
        return {"type": "image", "media_type": "image/jpeg", "base64": encoded}
    except subprocess.TimeoutExpired:
        subprocess.run(["pkill", "-9", "grim"], capture_output=True)
        return "Error: grim timed out"
    finally:
        try:
            os.unlink(tmp)
        except OSError:
            pass
