"""Capture a screenshot of all monitors for visual context."""

import base64
import subprocess

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
    """Capture full screen via grim, return base64 PNG."""
    result = subprocess.run(
        ["grim", "-t", "png", "-"],
        capture_output=True,
    )
    if result.returncode != 0:
        return f"Error capturing screenshot: {result.stderr.decode()}"
    encoded = base64.b64encode(result.stdout).decode()
    return {"type": "image", "base64": encoded}
