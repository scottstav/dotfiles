"""Copy text or a file to the Wayland clipboard."""

import subprocess

name = "clipboard"
description = "Copy text or a file to the user's clipboard via wl-copy. Use when the user asks you to copy something, when providing code/text they'll want to paste, or when you produce an output file the user will want to paste somewhere."
input_schema = {
    "type": "object",
    "properties": {
        "text": {
            "type": "string",
            "description": "The text to copy to clipboard",
        },
        "file": {
            "type": "string",
            "description": "Absolute path to a file to copy to clipboard as a file URI (for pasting into apps like Slack/Discord)",
        },
    },
}


def run(input):
    file = input.get("file")
    text = input.get("text")

    if file:
        uri = f"file://{file}"
        subprocess.run(["wl-copy", "-t", "text/uri-list", uri], check=True)
        return f"Copied file {file} to clipboard."
    elif text:
        subprocess.run(["wl-copy", text], check=True)
        return f"Copied {len(text)} characters to clipboard."
    else:
        return "Error: provide either 'text' or 'file'."
