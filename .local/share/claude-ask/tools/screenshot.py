"""Capture a screenshot and return it as an image."""

import base64
import subprocess
import tempfile
import os

name = "screenshot"
description = "Capture a screenshot of the screen or a selected region. Returns the image for analysis. Use when the user asks about something on their screen."
input_schema = {
    "type": "object",
    "properties": {
        "region": {
            "type": "string",
            "enum": ["full", "select"],
            "description": "'select' lets the user pick a region with their cursor, 'full' captures the entire screen",
        },
    },
    "required": ["region"],
}


def run(input):
    region = input["region"]
    with tempfile.NamedTemporaryFile(suffix=".png", delete=False) as f:
        tmp_path = f.name

    try:
        if region == "select":
            slurp = subprocess.run(
                ["slurp"], capture_output=True, text=True, check=True,
            )
            geometry = slurp.stdout.strip()
            subprocess.run(
                ["grim", "-g", geometry, tmp_path], check=True,
            )
        else:
            subprocess.run(["grim", tmp_path], check=True)

        with open(tmp_path, "rb") as f:
            data = base64.b64encode(f.read()).decode("ascii")

        return {"type": "image", "data": data, "media_type": "image/png"}
    finally:
        if os.path.exists(tmp_path):
            os.unlink(tmp_path)
