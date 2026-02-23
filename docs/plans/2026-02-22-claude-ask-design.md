# Claude Ask: Desktop Claude Chat Overlay

## Overview

A minimal, unobtrusive desktop UI for talking to Claude. Press a keybind, type in a floating box, press Enter — the box disappears and Claude's response streams as a live-updating notification.

## Architecture

Two components connected via Unix socket:

1. **`claude-ask`** (Bash) — Keybind target. Launches a floating Foot terminal running a Python TUI for text input.
2. **`claude-ask-daemon`** (Python) — Long-running daemon. Receives queries, streams Claude API responses to notifications, manages conversation history, handles tool calls.

### Flow

```
Keybind (Super+Shift+A)
  → claude-ask (bash)
  → foot --app-id=claude-ask -- python3 input.py
  → user types (Shift+Enter for newlines, Enter to submit, Escape to cancel)
  → sends JSON query via Unix socket → foot closes

claude-ask-daemon (python, systemd user service)
  → receives query on /run/user/$UID/claude-ask.sock
  → reads API key from ~/.authinfo.gpg (cached in memory)
  → streams Claude API response (claude-sonnet-4-6)
  → notify-send --replace-id for live-updating notification
  → if tool_use: execute tool, feed result back, continue streaming
  → final notification with [Reply] and [Open] actions
  → saves conversation to ~/.local/state/claude-ask/conversations/
```

## Components

### Input: `claude-ask` + `input.py`

**`~/.local/bin/claude-ask`** (Bash):
- Launches `foot --app-id=claude-ask -e python3 ~/.local/share/claude-ask/input.py "$@"`
- Accepts `--reply <conv_id>` to continue a conversation

**`~/.local/share/claude-ask/input.py`** (Python, uses `prompt_toolkit`):
- Text area with Shift+Enter = newline, Enter = submit
- On submit: sends JSON to Unix socket `{"text": "...", "conversation_id": "..."}`
- On Escape: closes without sending
- Exits immediately after sending

**Hyprland rules:**
```
windowrule = match:initial_class ^(claude-ask)$, float on
windowrule = match:initial_class ^(claude-ask)$, size (monitor_w*0.4) (monitor_h*0.15)
windowrule = match:initial_class ^(claude-ask)$, center on
```

**Keybind:**
```
bind = $mainMod SHIFT, A, exec, claude-ask
```

### Daemon: `claude-ask-daemon`

**`~/.local/share/claude-ask/daemon.py`** (Python):

**Startup:**
- Parses `~/.authinfo.gpg` via `gpg --batch --quiet --decrypt` for `machine api.anthropic.com password sk-ant-...`
- Caches API key in memory
- Creates Unix socket at `/run/user/$UID/claude-ask.sock`
- Loads existing conversations from disk

**On query received:**
1. Look up or create conversation by ID
2. Append user message to conversation history
3. Call Claude Messages API with `stream=True`
4. As text deltas arrive, debounce (~200ms) and update notification via `notify-send -r <id>`
5. Truncate notification body at ~300 chars
6. If `tool_use` block: execute tool, feed result back, continue streaming
7. Final notification with actions:
   - **Reply** → `claude-ask --reply <conv_id>`
   - **Open** → `emacsclient -c` with response in markdown-mode buffer
8. Save conversation to `~/.local/state/claude-ask/conversations/<conv_id>.json`

**Conversation format:**
```json
{
  "id": "uuid",
  "created": "2026-02-22T...",
  "messages": [
    {"role": "user", "content": "..."},
    {"role": "assistant", "content": "..."}
  ]
}
```

**Systemd service** (`~/.config/systemd/user/claude-ask.service`):
```ini
[Unit]
Description=Claude Ask Daemon
After=gpg-agent.service

[Service]
ExecStart=python3 %h/.local/share/claude-ask/daemon.py
Restart=on-failure

[Install]
WantedBy=default.target
```

### Tool System

Tools live in `~/.local/share/claude-ask/tools/`. Each is a Python file:

```python
# tools/example.py
name = "example_tool"
description = "What this tool does"
input_schema = {
    "type": "object",
    "properties": {"param": {"type": "string"}},
    "required": ["param"]
}

def run(input: dict) -> str:
    return "result"
```

Auto-discovered at startup. V1 ships with no tools — just the plugin directory.

### Credentials

- Format: `machine api.anthropic.com login apikey password sk-ant-api03-...`
- Stored in `~/.authinfo.gpg`
- Parsed once at daemon startup

### Error Handling

- **API errors:** Notification with error text, conversation preserved for retry
- **Socket errors:** Logged to journald, daemon restarts via systemd
- **GPG failure:** Daemon exits with setup instructions
- **Tool failure:** Error sent as tool_result to Claude, brief error in notification

## File Layout

```
~/.local/bin/claude-ask                          # Keybind entry point (bash)
~/.local/share/claude-ask/input.py               # TUI input (prompt_toolkit)
~/.local/share/claude-ask/daemon.py              # Streaming daemon
~/.local/share/claude-ask/tools/                 # Tool plugins directory
~/.local/state/claude-ask/conversations/         # Persistent conversation history
~/.config/systemd/user/claude-ask.service        # Systemd user service
```

## Future Enhancements

- **Image paste:** Ctrl+V in TUI detects image on Wayland clipboard via `wl-paste`, sends as base64 image content block
- **Waybar module:** Show conversation status / last response snippet in status bar
- **System prompt customization:** Config file for default system prompt

## Dependencies

- `python3`, `prompt_toolkit`, `anthropic` (Python SDK)
- `foot` (terminal), `notify-send` (notifications), `gpg` (credentials)
- `emacsclient` (for Open action)
