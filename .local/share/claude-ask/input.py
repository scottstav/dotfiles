#!/usr/bin/env python3
"""Minimal TUI for typing a Claude query with conversation picker.

Runs inside a floating Foot terminal. Two modes:
  - Picker mode (default when >30s since last query): browse recent conversations
  - Auto-reply mode (<30s since last query): pre-selects the most recent conversation
"""

import argparse
import base64
import json
import os
import socket
import subprocess
import sys
import threading
import time
from pathlib import Path

from format import archive_path
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


CONVERSATIONS_DIR = Path.home() / ".local" / "state" / "claude-ask" / "conversations"
LAST_STATE_FILE = Path.home() / ".local" / "state" / "claude-ask" / "last.json"
AUTO_REPLY_THRESHOLD_SECS = 30


def get_socket_path():
    runtime_dir = os.environ.get("XDG_RUNTIME_DIR", f"/run/user/{os.getuid()}")
    return os.path.join(runtime_dir, "claude-ask.sock")


def get_clipboard_image():
    """Check if clipboard has an image and return base64-encoded PNG, or None."""
    try:
        types = subprocess.run(
            ["wl-paste", "--list-types"],
            capture_output=True, text=True, timeout=2,
        )
        if "image/png" not in types.stdout:
            return None
        img = subprocess.run(
            ["wl-paste", "--type", "image/png"],
            capture_output=True, timeout=5,
        )
        if img.returncode != 0 or not img.stdout:
            return None
        return base64.b64encode(img.stdout).decode("ascii")
    except Exception:
        return None


def send_message(text, conversation_id, image=None):
    """Send the query as JSON over the Unix socket."""
    sock_path = get_socket_path()
    payload = {"text": text, "conversation_id": conversation_id}
    if image:
        payload["image"] = image
    msg = json.dumps(payload)

    try:
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        sock.connect(sock_path)
        sock.sendall(msg.encode("utf-8"))
        sock.close()
    except (ConnectionRefusedError, FileNotFoundError) as e:
        print(f"Could not connect to daemon: {e}", file=sys.stderr)
        sys.exit(1)


# ---------------------------------------------------------------------------
# Conversation loading
# ---------------------------------------------------------------------------

def _extract_text(content):
    """Extract plain text from a message content field (string or list of blocks)."""
    if isinstance(content, str):
        return content
    if isinstance(content, list):
        parts = []
        for block in content:
            if isinstance(block, dict):
                if block.get("type") == "text":
                    parts.append(block["text"])
                elif block.get("type") == "tool_result":
                    # skip tool results
                    continue
            elif isinstance(block, str):
                parts.append(block)
        return " ".join(parts)
    return ""


def load_conversations():
    """Load all conversations, return sorted list (newest first).

    Each entry: {"id": str, "user_preview": str, "assistant_preview": str, "mtime": float}
    """
    if not CONVERSATIONS_DIR.is_dir():
        return []

    convs = []
    for path in CONVERSATIONS_DIR.glob("*.json"):
        try:
            with open(path) as f:
                data = json.load(f)
        except (json.JSONDecodeError, OSError):
            continue

        messages = data.get("messages", [])
        if not messages:
            continue

        # Find last user message (skip tool_result messages)
        last_user = ""
        for msg in reversed(messages):
            if msg["role"] == "user":
                text = _extract_text(msg["content"])
                # Skip if it looks like tool results only
                if text.strip():
                    last_user = text.strip()
                    break

        # Find last assistant text
        last_assistant = ""
        for msg in reversed(messages):
            if msg["role"] == "assistant":
                last_assistant = _extract_text(msg["content"]).strip()
                if last_assistant:
                    break

        if not last_user:
            continue

        convs.append({
            "id": data["id"],
            "user_preview": last_user,
            "assistant_preview": last_assistant,
            "mtime": path.stat().st_mtime,
        })

    convs.sort(key=lambda c: c["mtime"], reverse=True)
    return convs


def read_last_state():
    """Read the daemon's last-query state file. Returns (conv_id, timestamp) or (None, 0)."""
    try:
        with open(LAST_STATE_FILE) as f:
            data = json.load(f)
        return data.get("conversation_id"), data.get("timestamp", 0)
    except (FileNotFoundError, json.JSONDecodeError, OSError):
        return None, 0


def truncate(text, max_len):
    """Truncate text to max_len chars, adding ellipsis if needed."""
    text = text.replace("\n", " ").strip()
    if len(text) <= max_len:
        return text
    return text[: max_len - 1] + "\u2026"


# ---------------------------------------------------------------------------
# TUI
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="Claude Ask input TUI")
    parser.add_argument("--reply", metavar="CONV_ID", default=None,
                        help="Continue an existing conversation")
    args = parser.parse_args()

    conversations = load_conversations()

    # Determine initial mode
    selected_conv_id = None  # The conversation selected for reply (via Tab)
    picker_index = [0]       # Which conversation is highlighted in the picker (mutable)

    # Determine which conversation to pre-select (if any)
    pre_select_id = None
    if args.reply:
        pre_select_id = args.reply
    else:
        last_conv_id, last_ts = read_last_state()
        if last_conv_id and (time.time() - last_ts) < AUTO_REPLY_THRESHOLD_SECS:
            pre_select_id = last_conv_id

    if pre_select_id:
        selected_conv_id = pre_select_id
        for i, c in enumerate(conversations):
            if c["id"] == pre_select_id:
                picker_index[0] = i
                break

    # Mutable state for the app
    state = {
        "selected_conv_id": selected_conv_id,
        "image": None,  # base64 PNG data, set by explicit Ctrl+V paste
    }

    result = {"submitted": False, "text": ""}

    buf = Buffer(multiline=True)
    kb = KeyBindings()

    # -- Key bindings -------------------------------------------------------

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

    @kb.add("c-g")
    def attach_image(event):
        """Ctrl+G: attach clipboard image."""
        def fetch():
            image = get_clipboard_image()
            if image:
                state["image"] = image
            event.app.invalidate()
        threading.Thread(target=fetch, daemon=True).start()

    @kb.add("tab")
    def toggle_reply(event):
        if not conversations:
            return

        if state["selected_conv_id"] is not None:
            # Deselect: go back to new mode
            state["selected_conv_id"] = None
        else:
            # Select the highlighted conversation
            idx = picker_index[0]
            if 0 <= idx < len(conversations):
                state["selected_conv_id"] = conversations[idx]["id"]

        # Force redraw
        event.app.invalidate()

    @kb.add("c-n")
    def next_conv(event):
        if not conversations:
            return
        if picker_index[0] < len(conversations) - 1:
            picker_index[0] += 1
            event.app.invalidate()

    @kb.add("c-p")
    def prev_conv(event):
        if not conversations:
            return
        if picker_index[0] > 0:
            picker_index[0] -= 1
            event.app.invalidate()

    @kb.add("c-o")
    def open_in_emacs(event):
        if not conversations:
            return
        idx = picker_index[0]
        if 0 <= idx < len(conversations):
            conv_id = conversations[idx]["id"]
            conv_path = CONVERSATIONS_DIR / f"{conv_id}.json"
            if conv_path.exists():
                with open(conv_path) as f:
                    conv = json.load(f)
                arch = archive_path(conv)
                if arch.exists():
                    subprocess.Popen(
                        ["emacsclient", "-c", str(arch)],
                        stdout=subprocess.DEVNULL,
                        stderr=subprocess.DEVNULL,
                    )
                    event.app.exit()

    # -- Header -------------------------------------------------------------

    def get_header():
        img = " [image] " if state["image"] else " "
        if state["selected_conv_id"]:
            # Find the preview for the selected conversation
            preview = ""
            for c in conversations:
                if c["id"] == state["selected_conv_id"]:
                    preview = truncate(c["user_preview"], 40)
                    break
            return [("class:header",
                     f" [reply: {preview}]{img}Enter:send  S-Enter:newline  C-g:image  Tab:deselect  Esc:cancel")]
        return [("class:header",
                 f" [new]{img}Enter:send  S-Enter:newline  C-g:image  Tab:select  Esc:cancel")]

    header = Window(
        content=FormattedTextControl(get_header),
        height=1,
        style="reverse",
    )

    # -- Conversation list --------------------------------------------------

    PREVIEW_WIDTH = 60

    def get_conv_list():
        if not conversations:
            return []

        fragments = []
        for i, conv in enumerate(conversations):
            is_highlighted = (i == picker_index[0])
            is_selected = (conv["id"] == state["selected_conv_id"])

            # Pointer indicator
            if is_selected:
                pointer = " \u25b6 "  # filled triangle for selected
            elif is_highlighted:
                pointer = " \u25b7 "  # outline triangle for highlighted
            else:
                pointer = "   "

            user_text = truncate(conv["user_preview"], PREVIEW_WIDTH)
            asst_text = truncate(conv["assistant_preview"], PREVIEW_WIDTH) if conv["assistant_preview"] else "(no response)"

            # Style based on state
            if is_highlighted or is_selected:
                user_style = "bold"
                asst_style = ""
            else:
                user_style = "class:conv-user"
                asst_style = "class:conv-asst"

            fragments.append((user_style, f'{pointer}"{user_text}"'))
            fragments.append(("", "\n"))
            fragments.append((asst_style, f"   {asst_text}"))
            fragments.append(("", "\n"))
            fragments.append(("", "\n"))

        return fragments

    conv_list_window = Window(
        content=FormattedTextControl(get_conv_list),
        wrap_lines=True,
    )

    # -- Input area ---------------------------------------------------------

    body = Window(
        content=BufferControl(buffer=buf),
        height=3,
        wrap_lines=True,
    )

    separator = Window(height=1, char="\u2500", style="class:separator")

    # -- Layout -------------------------------------------------------------

    layout = Layout(HSplit([header, body, separator, conv_list_window]))

    style_defs = {
        "conv-user": "#888888",
        "conv-asst": "#666666",
        "separator": "#444444",
    }

    from prompt_toolkit.styles import Style
    style = Style.from_dict(style_defs)

    app = Application(
        layout=layout,
        key_bindings=kb,
        full_screen=True,
        mouse_support=False,
        style=style,
    )

    app.run()

    if result["submitted"]:
        conv_id = state["selected_conv_id"]
        send_message(result["text"], conv_id, image=state["image"])


if __name__ == "__main__":
    main()
