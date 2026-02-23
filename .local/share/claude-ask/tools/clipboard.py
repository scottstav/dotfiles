"""Copy text to the Wayland clipboard."""

import subprocess

name = "clipboard"
description = "Copy text to the user's clipboard via wl-copy. Use when the user asks you to copy something or when providing code/text they'll want to paste."
input_schema = {
    "type": "object",
    "properties": {
        "text": {
            "type": "string",
            "description": "The text to copy to clipboard",
        },
    },
    "required": ["text"],
}


def run(input):
    text = input["text"]
    subprocess.run(["wl-copy", text], check=True)
    return f"Copied {len(text)} characters to clipboard."
