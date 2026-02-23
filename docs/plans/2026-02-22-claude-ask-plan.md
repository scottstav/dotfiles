# Claude Ask Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build a desktop Claude chat overlay — keybind opens floating input, response streams as a live-updating notification.

**Architecture:** Two components: a Bash launcher (`claude-ask`) spawns a floating Foot terminal running a `prompt_toolkit` TUI for input, which sends queries via Unix socket to a long-running Python daemon (`claude-ask-daemon`). The daemon streams Claude API responses to SwayNC notifications, handles tool calls, and persists conversation history.

**Tech Stack:** Python 3 (`anthropic`, `prompt_toolkit`), Bash, Foot terminal, `notify-send` (SwayNC), Hyprland window rules, systemd user service.

---

### Task 1: Create project directory and venv

**Files:**
- Create: `~/.local/share/claude-ask/` (directory)
- Create: `~/.local/share/claude-ask/requirements.txt`

**Step 1: Create directory structure**

```bash
mkdir -p ~/.local/share/claude-ask/tools
mkdir -p ~/.local/state/claude-ask/conversations
```

**Step 2: Create requirements.txt**

Create `.local/share/claude-ask/requirements.txt`:
```
anthropic
prompt_toolkit
```

**Step 3: Create venv and install deps**

```bash
cd ~/.local/share/claude-ask
python3 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```

**Step 4: Verify installation**

```bash
source ~/.local/share/claude-ask/.venv/bin/activate
python3 -c "import anthropic; import prompt_toolkit; print('OK')"
```

Expected: `OK`

**Step 5: Commit**

```bash
cd ~/dotfiles
git add .local/share/claude-ask/requirements.txt
git commit -m "feat(claude-ask): add project skeleton and requirements"
```

---

### Task 2: Build the input TUI

**Files:**
- Create: `.local/share/claude-ask/input.py`

**Step 1: Write the input TUI**

Create `.local/share/claude-ask/input.py`:

```python
#!/usr/bin/env python3
"""Minimal TUI for typing a Claude query. Shift+Enter = newline, Enter = submit."""

import argparse
import json
import os
import socket
import sys

from prompt_toolkit import Application
from prompt_toolkit.buffer import Buffer
from prompt_toolkit.key_binding import KeyBindings
from prompt_toolkit.layout.containers import HSplit, Window
from prompt_toolkit.layout.controls import BufferControl, FormattedTextControl
from prompt_toolkit.layout.layout import Layout


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--reply", help="Conversation ID to continue")
    args = parser.parse_args()

    submitted_text = None

    kb = KeyBindings()

    @kb.add("enter")
    def submit(event):
        nonlocal submitted_text
        submitted_text = event.app.current_buffer.text
        event.app.exit()

    @kb.add("escape")
    def cancel(event):
        event.app.exit()

    @kb.add("s-enter")
    def newline(event):
        event.app.current_buffer.insert_text("\n")

    buf = Buffer(multiline=True)
    header = Window(
        content=FormattedTextControl("Ask Claude (Enter=send, Shift+Enter=newline, Esc=cancel)"),
        height=1,
    )
    body = Window(content=BufferControl(buffer=buf), wrap_lines=True)
    layout = Layout(HSplit([header, body]))

    app = Application(layout=layout, key_bindings=kb, full_screen=True)
    app.run()

    if not submitted_text or not submitted_text.strip():
        sys.exit(0)

    sock_path = os.path.join(
        os.environ.get("XDG_RUNTIME_DIR", f"/run/user/{os.getuid()}"),
        "claude-ask.sock",
    )
    msg = json.dumps({
        "text": submitted_text.strip(),
        "conversation_id": args.reply,
    })

    try:
        s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        s.connect(sock_path)
        s.sendall(msg.encode() + b"\n")
        s.close()
    except ConnectionRefusedError:
        print("claude-ask-daemon is not running", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
```

**Step 2: Test the TUI manually**

```bash
cd ~/.local/share/claude-ask
source .venv/bin/activate
python3 input.py
```

Expected: Full-screen TUI appears. Type text, press Enter — it exits. Press Escape — it exits. Shift+Enter inserts newline. (Socket send will fail since daemon isn't running yet — that's fine.)

**Step 3: Commit**

```bash
cd ~/dotfiles
git add .local/share/claude-ask/input.py
git commit -m "feat(claude-ask): add prompt_toolkit input TUI"
```

---

### Task 3: Build the daemon — socket listener and credential loading

**Files:**
- Create: `.local/share/claude-ask/daemon.py`

**Step 1: Write the daemon skeleton**

Create `.local/share/claude-ask/daemon.py` with:
- GPG credential parsing from `~/.authinfo.gpg`
- Unix socket listener accepting JSON messages
- Conversation state management (load/save JSON files)
- No Claude API calls yet — just log received messages

```python
#!/usr/bin/env python3
"""Claude Ask daemon: receives queries via Unix socket, streams responses as notifications."""

import asyncio
import json
import os
import subprocess
import sys
import uuid
from datetime import datetime, timezone
from pathlib import Path


SOCK_DIR = os.environ.get("XDG_RUNTIME_DIR", f"/run/user/{os.getuid()}")
SOCK_PATH = os.path.join(SOCK_DIR, "claude-ask.sock")
CONV_DIR = Path.home() / ".local" / "state" / "claude-ask" / "conversations"
AUTHINFO = Path.home() / ".authinfo.gpg"


def load_api_key() -> str:
    """Parse API key from ~/.authinfo.gpg."""
    try:
        result = subprocess.run(
            ["gpg", "--batch", "--quiet", "--decrypt", str(AUTHINFO)],
            capture_output=True, text=True, check=True,
        )
    except (subprocess.CalledProcessError, FileNotFoundError) as e:
        print(f"Failed to decrypt {AUTHINFO}: {e}", file=sys.stderr)
        print("Add a line: machine api.anthropic.com login apikey password sk-ant-...", file=sys.stderr)
        sys.exit(1)

    for line in result.stdout.splitlines():
        parts = line.split()
        if "api.anthropic.com" in parts:
            try:
                pw_idx = parts.index("password") + 1
                return parts[pw_idx]
            except (ValueError, IndexError):
                pass

    print(f"No api.anthropic.com entry in {AUTHINFO}", file=sys.stderr)
    print("Add a line: machine api.anthropic.com login apikey password sk-ant-...", file=sys.stderr)
    sys.exit(1)


class ConversationStore:
    """Load and save conversation JSON files."""

    def __init__(self, directory: Path):
        self.directory = directory
        self.directory.mkdir(parents=True, exist_ok=True)
        self.conversations: dict[str, dict] = {}
        self._load_all()

    def _load_all(self):
        for f in self.directory.glob("*.json"):
            try:
                data = json.loads(f.read_text())
                self.conversations[data["id"]] = data
            except (json.JSONDecodeError, KeyError):
                pass

    def get_or_create(self, conv_id: str | None) -> dict:
        if conv_id and conv_id in self.conversations:
            return self.conversations[conv_id]
        new_id = conv_id or str(uuid.uuid4())
        conv = {
            "id": new_id,
            "created": datetime.now(timezone.utc).isoformat(),
            "messages": [],
        }
        self.conversations[new_id] = conv
        return conv

    def save(self, conv: dict):
        path = self.directory / f"{conv['id']}.json"
        path.write_text(json.dumps(conv, indent=2))


class Daemon:
    def __init__(self, api_key: str):
        self.api_key = api_key
        self.store = ConversationStore(CONV_DIR)

    async def handle_client(self, reader: asyncio.StreamReader, writer: asyncio.StreamWriter):
        try:
            data = await reader.readline()
            if not data:
                return
            msg = json.loads(data.decode())
            text = msg.get("text", "").strip()
            conv_id = msg.get("conversation_id")
            if not text:
                return
            print(f"Received: {text[:80]}... (conv={conv_id})")
            await self.handle_query(text, conv_id)
        except Exception as e:
            print(f"Client error: {e}", file=sys.stderr)
        finally:
            writer.close()
            await writer.wait_closed()

    async def handle_query(self, text: str, conv_id: str | None):
        """Process a query — streaming will be added in the next task."""
        conv = self.store.get_or_create(conv_id)
        conv["messages"].append({"role": "user", "content": text})
        # Placeholder: actual streaming in next task
        print(f"Would call Claude API with {len(conv['messages'])} messages")
        self.store.save(conv)

    async def run(self):
        # Clean up stale socket
        try:
            os.unlink(SOCK_PATH)
        except FileNotFoundError:
            pass

        server = await asyncio.start_unix_server(self.handle_client, SOCK_PATH)
        os.chmod(SOCK_PATH, 0o600)
        print(f"Listening on {SOCK_PATH}")

        async with server:
            await server.serve_forever()


def main():
    api_key = load_api_key()
    print(f"API key loaded ({api_key[:12]}...)")
    daemon = Daemon(api_key)
    asyncio.run(daemon.run())


if __name__ == "__main__":
    main()
```

**Step 2: Test credential loading**

First, ensure `~/.authinfo.gpg` has an Anthropic entry. If not, add one:
```bash
# Decrypt, add line, re-encrypt
gpg --batch --quiet --decrypt ~/.authinfo.gpg > /tmp/authinfo.tmp
echo "machine api.anthropic.com login apikey password YOUR_KEY_HERE" >> /tmp/authinfo.tmp
gpg --batch --yes --default-recipient-self -e -o ~/.authinfo.gpg /tmp/authinfo.tmp
rm /tmp/authinfo.tmp
```

Then test:
```bash
cd ~/.local/share/claude-ask
source .venv/bin/activate
python3 daemon.py
```

Expected: `API key loaded (sk-ant-api0...)` then `Listening on /run/user/1001/claude-ask.sock`

**Step 3: Test socket communication end-to-end**

In another terminal:
```bash
cd ~/.local/share/claude-ask
source .venv/bin/activate
python3 input.py
```

Type something, press Enter.

Expected: daemon terminal prints `Received: <your text>... (conv=None)` and a conversation file appears in `~/.local/state/claude-ask/conversations/`.

**Step 4: Commit**

```bash
cd ~/dotfiles
git add .local/share/claude-ask/daemon.py
git commit -m "feat(claude-ask): add daemon with socket listener and credential loading"
```

---

### Task 4: Add Claude API streaming and notification output

**Files:**
- Modify: `.local/share/claude-ask/daemon.py` — replace `handle_query` placeholder with real streaming

**Step 1: Implement streaming in handle_query**

Replace the `handle_query` method in `daemon.py` with:

```python
async def handle_query(self, text: str, conv_id: str | None):
    """Stream a Claude response and display via notifications."""
    conv = self.store.get_or_create(conv_id)
    conv["messages"].append({"role": "user", "content": text})

    import anthropic

    client = anthropic.Anthropic(api_key=self.api_key)

    # Collect tool definitions
    tools = self._load_tools()

    api_kwargs = dict(
        model="claude-sonnet-4-6",
        max_tokens=4096,
        messages=conv["messages"],
    )
    if tools:
        api_kwargs["tools"] = tools

    full_response = ""
    notify_tag = f"claude-ask-{conv['id'][:8]}"
    last_notify_time = 0

    try:
        with client.messages.stream(**api_kwargs) as stream:
            for event in stream:
                if hasattr(event, "type"):
                    if event.type == "content_block_delta":
                        if hasattr(event.delta, "text"):
                            full_response += event.delta.text
                            now = asyncio.get_event_loop().time()
                            if now - last_notify_time > 0.2:
                                last_notify_time = now
                                self._notify(notify_tag, full_response)

        # Handle tool use in response
        response_msg = stream.get_final_message()
        tool_uses = [b for b in response_msg.content if b.type == "tool_use"]

        if tool_uses:
            # Add assistant message with all content blocks
            conv["messages"].append({
                "role": "assistant",
                "content": [
                    {"type": b.type, **({"text": b.text} if b.type == "text" else {"id": b.id, "name": b.name, "input": b.input})}
                    for b in response_msg.content
                ],
            })

            # Execute each tool and collect results
            tool_results = []
            for tu in tool_uses:
                self._notify(notify_tag, f"Running tool: {tu.name}...")
                result = await self._run_tool(tu.name, tu.input)
                tool_results.append({
                    "type": "tool_result",
                    "tool_use_id": tu.id,
                    "content": result,
                })

            conv["messages"].append({"role": "user", "content": tool_results})
            self.store.save(conv)

            # Continue conversation with tool results
            await self.handle_query_continue(conv, notify_tag)
            return

        # No tool use — finalize
        conv["messages"].append({"role": "assistant", "content": full_response})
        self.store.save(conv)

        # Final notification with actions
        self._final_notify(notify_tag, full_response, conv["id"])

    except anthropic.APIError as e:
        self._notify(notify_tag, f"API error: {e.message}")
        self.store.save(conv)

async def handle_query_continue(self, conv: dict, notify_tag: str):
    """Continue streaming after tool results."""
    import anthropic

    client = anthropic.Anthropic(api_key=self.api_key)
    tools = self._load_tools()

    api_kwargs = dict(
        model="claude-sonnet-4-6",
        max_tokens=4096,
        messages=conv["messages"],
    )
    if tools:
        api_kwargs["tools"] = tools

    full_response = ""
    last_notify_time = 0

    with client.messages.stream(**api_kwargs) as stream:
        for event in stream:
            if hasattr(event, "type") and event.type == "content_block_delta":
                if hasattr(event.delta, "text"):
                    full_response += event.delta.text
                    now = asyncio.get_event_loop().time()
                    if now - last_notify_time > 0.2:
                        last_notify_time = now
                        self._notify(notify_tag, full_response)

    response_msg = stream.get_final_message()
    tool_uses = [b for b in response_msg.content if b.type == "tool_use"]

    if tool_uses:
        conv["messages"].append({
            "role": "assistant",
            "content": [
                {"type": b.type, **({"text": b.text} if b.type == "text" else {"id": b.id, "name": b.name, "input": b.input})}
                for b in response_msg.content
            ],
        })
        tool_results = []
        for tu in tool_uses:
            self._notify(notify_tag, f"Running tool: {tu.name}...")
            result = await self._run_tool(tu.name, tu.input)
            tool_results.append({
                "type": "tool_result",
                "tool_use_id": tu.id,
                "content": result,
            })
        conv["messages"].append({"role": "user", "content": tool_results})
        self.store.save(conv)
        await self.handle_query_continue(conv, notify_tag)
        return

    conv["messages"].append({"role": "assistant", "content": full_response})
    self.store.save(conv)
    self._final_notify(notify_tag, full_response, conv["id"])
```

Also add these helper methods to the `Daemon` class:

```python
def _notify(self, tag: str, text: str):
    """Update in-place notification with current response text."""
    truncated = text[:300] + ("..." if len(text) > 300 else "")
    # Use x-canonical-private-synchronous for in-place updates (SwayNC pattern)
    subprocess.Popen(
        [
            "notify-send", "-t", "0",
            "-h", f"string:x-canonical-private-synchronous:{tag}",
            "-a", "Claude",
            "Claude", truncated,
        ],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )

def _final_notify(self, tag: str, text: str, conv_id: str):
    """Show final notification with Reply and Open actions."""
    truncated = text[:300] + ("..." if len(text) > 300 else "")

    # Run in background thread since --wait blocks
    import threading

    def _wait_for_action():
        try:
            result = subprocess.run(
                [
                    "notify-send", "-t", "0",
                    "-h", f"string:x-canonical-private-synchronous:{tag}",
                    "-a", "Claude",
                    "Claude", truncated,
                    "-A", "reply=Reply",
                    "-A", "open=Open in Emacs",
                    "--wait",
                ],
                capture_output=True, text=True,
            )
            action = result.stdout.strip()
            if action == "reply":
                subprocess.Popen(["claude-ask", "--reply", conv_id])
            elif action == "open":
                # Escape for elisp string
                escaped = text.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n")
                subprocess.Popen([
                    "emacsclient", "-c", "--eval",
                    f'(with-current-buffer (get-buffer-create "*claude-response*") '
                    f'(let ((inhibit-read-only t)) (erase-buffer) (insert "{escaped}")) '
                    f'(markdown-mode) (goto-char (point-min)))',
                ])
        except Exception as e:
            print(f"Notification action error: {e}")

    threading.Thread(target=_wait_for_action, daemon=True).start()

def _load_tools(self) -> list[dict]:
    """Auto-discover tools from ~/.local/share/claude-ask/tools/."""
    tools_dir = Path.home() / ".local" / "share" / "claude-ask" / "tools"
    tools = []
    if not tools_dir.exists():
        return tools
    for f in tools_dir.glob("*.py"):
        if f.name.startswith("_"):
            continue
        try:
            import importlib.util
            spec = importlib.util.spec_from_file_location(f.stem, f)
            mod = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(mod)
            tools.append({
                "name": mod.name,
                "description": mod.description,
                "input_schema": mod.input_schema,
            })
        except Exception as e:
            print(f"Failed to load tool {f}: {e}")
    return tools

async def _run_tool(self, name: str, input_data: dict) -> str:
    """Execute a tool by name and return its result."""
    tools_dir = Path.home() / ".local" / "share" / "claude-ask" / "tools"
    for f in tools_dir.glob("*.py"):
        if f.name.startswith("_"):
            continue
        try:
            import importlib.util
            spec = importlib.util.spec_from_file_location(f.stem, f)
            mod = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(mod)
            if mod.name == name:
                return str(mod.run(input_data))
        except Exception as e:
            return f"Tool execution error: {e}"
    return f"Unknown tool: {name}"
```

**Step 2: Test streaming end-to-end**

Start daemon:
```bash
cd ~/.local/share/claude-ask && source .venv/bin/activate && python3 daemon.py
```

In another terminal, send a test query:
```bash
cd ~/.local/share/claude-ask && source .venv/bin/activate && python3 input.py
```

Type "What is 2+2?" and press Enter.

Expected: A notification appears and updates in real-time as Claude streams the response. Final notification has Reply and Open buttons.

**Step 3: Test Reply action**

Click "Reply" on the notification. A new Foot window should appear with the same conversation context.

**Step 4: Test Open in Emacs action**

Send another query. Click "Open in Emacs" on the notification.

Expected: An Emacs frame opens with the response in markdown-mode.

**Step 5: Commit**

```bash
cd ~/dotfiles
git add .local/share/claude-ask/daemon.py
git commit -m "feat(claude-ask): add Claude API streaming and notification output"
```

---

### Task 5: Create the Bash launcher script

**Files:**
- Create: `.local/bin/claude-ask`

**Step 1: Write the launcher**

Create `.local/bin/claude-ask`:

```bash
#!/bin/bash
# Open floating input for Claude Ask. Passes args through to input.py.
CLAUDE_ASK_DIR="$HOME/.local/share/claude-ask"
exec foot --app-id=claude-ask -e "$CLAUDE_ASK_DIR/.venv/bin/python3" "$CLAUDE_ASK_DIR/input.py" "$@"
```

**Step 2: Make it executable**

```bash
chmod +x ~/.local/bin/claude-ask
```

**Step 3: Test it**

```bash
claude-ask
```

Expected: A Foot window opens running the TUI. (Floating behavior depends on Hyprland rules added in next task.)

**Step 4: Test reply mode**

```bash
claude-ask --reply some-uuid-here
```

Expected: Same TUI opens. After submit, the conversation_id is sent via socket.

**Step 5: Commit**

```bash
cd ~/dotfiles
git add .local/bin/claude-ask
git commit -m "feat(claude-ask): add bash launcher script"
```

---

### Task 6: Add Hyprland window rules and keybind

**Files:**
- Modify: `.config/hypr/hyprland.conf`

**Step 1: Add window rules for claude-ask**

Add after the existing `fzf-picker` rules (around line 193):

```
# Claude Ask floating input
windowrule = match:initial_class ^(claude-ask)$, float on
windowrule = match:initial_class ^(claude-ask)$, size (monitor_w*0.4) (monitor_h*0.15)
windowrule = match:initial_class ^(claude-ask)$, center on
```

**Step 2: Replace gptel-agent-prompt keybind with claude-ask**

Change line 249 from:
```
bind = $mainMod SHIFT, A, exec, gptel-agent-prompt
```
to:
```
bind = $mainMod SHIFT, A, exec, claude-ask
```

**Step 3: Test the keybind**

Reload Hyprland config (it auto-reloads on save, or `hyprctl reload`), then press `Super+Shift+A`.

Expected: Small floating Foot window appears centered, ~40% width and ~15% height.

**Step 4: Commit**

```bash
cd ~/dotfiles
git add .config/hypr/hyprland.conf
git commit -m "feat(claude-ask): add hyprland window rules and keybind"
```

---

### Task 7: Create systemd user service

**Files:**
- Create: `.config/systemd/user/claude-ask.service`

**Step 1: Write the service file**

Create `.config/systemd/user/claude-ask.service`:

```ini
[Unit]
Description=Claude Ask Daemon
After=gpg-agent.service

[Service]
ExecStart=%h/.local/share/claude-ask/.venv/bin/python3 %h/.local/share/claude-ask/daemon.py
Restart=on-failure
RestartSec=5

[Install]
WantedBy=default.target
```

**Step 2: Stow and enable**

```bash
cd ~/dotfiles && stow .
systemctl --user daemon-reload
systemctl --user enable --now claude-ask
```

**Step 3: Verify it's running**

```bash
systemctl --user status claude-ask
```

Expected: Active (running), log shows `API key loaded` and `Listening on /run/user/1001/claude-ask.sock`.

**Step 4: Full end-to-end test**

Press `Super+Shift+A`, type a question, press Enter. Verify:
1. Foot window closes
2. Notification appears and streams Claude's response
3. Final notification has Reply and Open buttons
4. Reply opens a new input with the same conversation
5. Open shows full response in Emacs

**Step 5: Commit**

```bash
cd ~/dotfiles
git add .config/systemd/user/claude-ask.service
git commit -m "feat(claude-ask): add systemd user service"
```

---

### Task 8: Final integration testing and polish

**Files:**
- Possibly modify: `.local/share/claude-ask/daemon.py` (bug fixes from testing)
- Possibly modify: `.local/share/claude-ask/input.py` (bug fixes from testing)

**Step 1: Test new conversation flow**

1. `Super+Shift+A` → type "What's the capital of France?" → Enter
2. Verify notification streams and shows "Paris" in response
3. Verify conversation file created in `~/.local/state/claude-ask/conversations/`

**Step 2: Test reply/follow-up flow**

1. Click "Reply" on the notification
2. Type "And what about Germany?" → Enter
3. Verify response uses conversation context (knows you were asking about capitals)
4. Verify conversation file has 4 messages (2 user, 2 assistant)

**Step 3: Test Open in Emacs**

1. Send a longer query that produces a multi-paragraph response
2. Click "Open in Emacs"
3. Verify Emacs frame opens with full response in markdown-mode

**Step 4: Test daemon restart resilience**

```bash
systemctl --user restart claude-ask
```

Then send a reply to an old conversation — verify history is loaded from disk.

**Step 5: Test escape/cancel**

1. `Super+Shift+A` → press Escape
2. Verify no notification appears, no socket message sent

**Step 6: Fix any issues found, commit**

```bash
cd ~/dotfiles
git add -A .local/share/claude-ask/ .config/
git commit -m "fix(claude-ask): polish from integration testing"
```
