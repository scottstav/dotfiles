#!/usr/bin/env python3
"""Minimal TUI for typing a Claude query. Runs inside a floating Foot terminal."""

import argparse
import json
import os
import socket
import sys

from prompt_toolkit import Application
from prompt_toolkit.buffer import Buffer
from prompt_toolkit.input.vt100_parser import ANSI_SEQUENCES
from prompt_toolkit.key_binding import KeyBindings
from prompt_toolkit.keys import Keys
from prompt_toolkit.layout.containers import HSplit, Window
from prompt_toolkit.layout.controls import BufferControl, FormattedTextControl
from prompt_toolkit.layout.layout import Layout

# Map Shift+Enter (Kitty keyboard protocol: CSI 13;2u) to Escape+Enter so
# prompt_toolkit treats it the same as Alt+Enter. Foot supports this protocol.
ANSI_SEQUENCES["\x1b[13;2u"] = (Keys.Escape, Keys.ControlJ)


def get_socket_path():
    runtime_dir = os.environ.get("XDG_RUNTIME_DIR", f"/run/user/{os.getuid()}")
    return os.path.join(runtime_dir, "claude-ask.sock")


def send_message(text, conversation_id):
    """Send the query as JSON over the Unix socket."""
    sock_path = get_socket_path()
    msg = json.dumps({"text": text, "conversation_id": conversation_id})

    try:
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        sock.connect(sock_path)
        sock.sendall(msg.encode("utf-8"))
        sock.close()
    except (ConnectionRefusedError, FileNotFoundError) as e:
        print(f"Could not connect to daemon: {e}", file=sys.stderr)
        sys.exit(1)


def main():
    parser = argparse.ArgumentParser(description="Claude Ask input TUI")
    parser.add_argument("--reply", metavar="CONV_ID", default=None,
                        help="Continue an existing conversation")
    args = parser.parse_args()

    result = {"submitted": False, "text": ""}

    buf = Buffer(multiline=True)
    kb = KeyBindings()

    @kb.add("enter")
    def submit(event):
        text = buf.text.strip()
        if text:
            result["submitted"] = True
            result["text"] = text
        event.app.exit()

    @kb.add("escape", "enter")
    def newline(event):
        """Shift+Enter (via Kitty protocol) or Alt+Enter inserts a newline."""
        buf.insert_text("\n")

    @kb.add("escape")
    def cancel(event):
        event.app.exit()

    mode = "reply" if args.reply else "new"
    header_text = f" [{mode}]  Enter: send | Shift+Enter: newline | Esc: cancel"

    header = Window(
        content=FormattedTextControl(header_text),
        height=1,
        style="reverse",
    )

    body = Window(
        content=BufferControl(buffer=buf),
        wrap_lines=True,
    )

    layout = Layout(HSplit([header, body]))

    app = Application(
        layout=layout,
        key_bindings=kb,
        full_screen=True,
        mouse_support=False,
    )

    app.run()

    if result["submitted"]:
        send_message(result["text"], args.reply)


if __name__ == "__main__":
    main()
