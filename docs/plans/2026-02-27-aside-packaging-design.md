# aside — Wayland-Native LLM Desktop Assistant

**Date:** 2026-02-27
**Status:** Design approved

## Overview

Extract the claude-ask/claude-voice/claude-overlay system from the dotfiles repo into a standalone, shareable package called **aside**. The project becomes LLM-agnostic (via LiteLLM) and Wayland-native, targeting Linux desktop users who want a conversational AI assistant integrated into their compositor.

## Architecture

```
┌──────────────────────────────────────────────────────┐
│  Input Methods                                       │
├──────────────┬──────────────┬────────────────────────┤
│  GTK4 Popup  │  Voice/STT   │  CLI (aside query ...) │
│  (default)   │  (optional)  │  (scriptable)          │
└──────┬───────┴──────┬───────┴──────────┬─────────────┘
       │              │                  │
       │    JSON over Unix socket        │
       ▼              ▼                  ▼
┌──────────────────────────────────────────────────────┐
│  aside-daemon (Python)                               │
│  ┌─────────────┐ ┌──────────┐ ┌────────────────────┐│
│  │ LiteLLM     │ │ Tool     │ │ State Manager      ││
│  │ query engine│ │ executor │ │ (conversations,    ││
│  │ (streaming) │ │ (plugins)│ │  memory, usage)    ││
│  └──────┬──────┘ └────┬─────┘ └────────────────────┘│
└─────────┼─────────────┼─────────────────────────────┘
          │             │
    ┌─────┴────┐   ┌────┴─────┐
    ▼          ▼   ▼          ▼
┌────────┐ ┌──────────┐ ┌──────────┐
│Overlay │ │ TTS      │ │ notify-  │
│(C/Way- │ │ (Kokoro, │ │ send     │
│ land)  │ │ optional)│ │          │
└────────┘ └──────────┘ └──────────┘
```

### Components

1. **aside-daemon** (Python, long-running systemd service) — the brain. Listens on `$XDG_RUNTIME_DIR/aside.sock`, routes queries through LiteLLM, executes tools, manages state, streams text to the overlay.

2. **aside-overlay** (C, separate systemd service) — streaming text display. Wayland layer-shell surface with Cairo/Pango rendering, scroll/fade animations, click actions. Listens on `$XDG_RUNTIME_DIR/aside-overlay.sock`.

3. **aside-input** (Python/GTK4) — popup input window. Conversation picker + text entry + file attachment. Launched by keybind, sends JSON to daemon socket. Replaces the current foot TUI.

4. **aside-voice** (Python, optional) — wake word detection (openwakeword) + speech-to-text (faster-whisper). Runs as part of the daemon when enabled. Sends transcribed text through the same query pipeline.

5. **aside-status** (Python script) — optional waybar/status bar module. Reads state files, outputs JSON for waybar custom module format.

6. **aside CLI** — `aside query "..."` for scripting, `aside status`, `aside cancel`, etc.

## LLM Backend: LiteLLM

Replace the `anthropic` SDK with `litellm` for LLM-agnostic support:

- Model format: `provider/model` (e.g. `anthropic/claude-sonnet-4-6`, `openai/gpt-4o`, `ollama/llama3`)
- API keys via environment variables (LiteLLM convention)
- Tool/function calling via OpenAI function calling schema (LiteLLM normalizes across providers)
- Streaming via OpenAI-style deltas

## Configuration

Single file at `$XDG_CONFIG_HOME/aside/config.toml`:

```toml
[model]
name = "anthropic/claude-sonnet-4-6"
system_prompt = ""

[input]
terminal = "foot -e"  # fallback if GTK not available

[voice]
enabled = false
wake_word = "hey aside"
stt_model = "base.en"

[tts]
enabled = false
model = "af_heart"
speed = 1.0
lang = "a"

[tts.filter]
skip_code_blocks = true
skip_urls = true

[overlay]
font = "Sans 13"
width = 600
max_lines = 40
margin_top = 10
padding_x = 20
padding_y = 16
corner_radius = 12
border_width = 2
accent_height = 3
scroll_duration = 200
fade_duration = 400

[overlay.colors]
background = "#1a1b26e6"
foreground = "#c0caf5ff"
border = "#414868ff"
accent = "#7aa2f7ff"

[storage]
conversations_dir = ""  # default: $XDG_STATE_HOME/aside/conversations/
archive_dir = ""        # empty = no archiving

[plugins]
dirs = []  # additional tool plugin directories
```

## Plugin/Tool System

Tools are Python files with a simple contract:

```python
TOOL_SPEC = {
    "name": "my_tool",
    "description": "Does something useful",
    "parameters": {
        "type": "object",
        "properties": {
            "query": {"type": "string", "description": "The search query"}
        },
        "required": ["query"]
    }
}

def run(query: str) -> str:
    return f"Result for {query}"
```

`TOOL_SPEC` uses the OpenAI function calling schema. `run()` receives the parameters and returns a string.

**Built-in tools:** clipboard, shell, memory
**Example plugins (documented, not installed):** screenshot, web_search, fetch_url

Plugin loading: scan built-in + user dirs on startup. Hot-reload on SIGHUP.

## Notifications

Uses `notify-send` (freedesktop standard). Action buttons (reply, listen) work on mako, dunst, swaync, and any spec-compliant daemon. Graceful degradation if the daemon doesn't support actions.

Notification action commands are configurable (e.g. what "reply" launches).

## Repo Layout

```
aside/
├── aside/                      # Python package
│   ├── __init__.py
│   ├── daemon.py               # Main daemon
│   ├── query.py                # LiteLLM streaming query engine
│   ├── tts.py                  # Kokoro TTS
│   ├── sentence_buffer.py      # Stream → sentence chunking
│   ├── voice/                  # Optional voice input
│   │   ├── listener.py
│   │   ├── audio.py
│   │   └── speech_detector.py
│   ├── input/                  # GTK4 input popup
│   │   └── window.py
│   ├── tools/                  # Built-in tools
│   │   ├── clipboard.py
│   │   ├── shell.py
│   │   └── memory.py
│   ├── plugins.py              # Plugin loader
│   ├── state.py                # Conversation/memory/usage state
│   ├── status.py               # Status bar output
│   └── cli.py                  # CLI entry points
├── overlay/                    # C Wayland overlay
│   ├── src/
│   │   ├── main.c, wayland.c/h, render.c/h
│   │   ├── socket.c/h, config.c/h
│   │   ├── animation.c/h, shm.c/h
│   ├── protocols/
│   │   └── wlr-layer-shell-unstable-v1.xml
│   └── meson.build
├── data/
│   ├── aside.desktop
│   ├── aside-daemon.service
│   ├── aside-overlay.service
│   ├── config.toml.example
│   └── waybar/
├── plugins/                    # Example plugins (not installed)
│   ├── screenshot.py
│   ├── web_search.py
│   └── fetch_url.py
├── Makefile
├── PKGBUILD
├── pyproject.toml
├── LICENSE
└── README.md
```

## Installation

### make install
1. Build C overlay (meson + ninja)
2. Install overlay binary to `$PREFIX/bin/aside-overlay`
3. Create Python venv at `$PREFIX/lib/aside/venv/`
4. Install Python package + deps into venv
5. Install wrapper scripts to `$PREFIX/bin/` (aside, aside-input, aside-status)
6. Install systemd user units
7. Install .desktop file
8. Install example config (if none exists)

### AUR PKGBUILD
- Depends: python>=3.11, wayland, cairo, pango, json-c, gtk4, python-gobject, gtk4-layer-shell
- Optional: python-kokoro (TTS), python-faster-whisper (voice)

### Python extras
```toml
[project.optional-dependencies]
tts = ["kokoro>=0.9.4", "soundfile", "sounddevice"]
voice = ["openwakeword==0.6.0", "faster-whisper==1.2.1", "webrtcvad==2.0.10"]
```

## Delta From Current Code

### Changes required
1. **Replace anthropic SDK with litellm** — query.py swap, tool schema translation, streaming format change
2. **New GTK4 input window** — replaces foot TUI (~300-400 lines Python/PyGObject)
3. **Decouple personal integrations** — remove emacsclient, Dropbox, hyprctl, authinfo.gpg, waybar SIGRTMIN+12
4. **Config consolidation** — merge claude-voice config.py + claude-ask config.toml + overlay config into single config.toml
5. **Rename** — claude-ask→aside, claude-voice→aside-voice, claude-overlay→aside-overlay, all socket/state/config paths
6. **Merge into single daemon** — voice listener + query dispatch in one long-running process
7. **Packaging boilerplate** — pyproject.toml, Makefile, PKGBUILD, .desktop, systemd units, README, LICENSE

### Unchanged
- Overlay C code (rename + config path change only)
- TTS pipeline (kokoro + sentence buffer)
- Audio system (PipeWire via sounddevice)
- Conversation state format (JSON files)
- Socket IPC protocol (JSON messages)
- Tool execution model

## Python Version

Python 3.11+ (tomllib built-in, wide availability on current distros).

## Out of Scope
- claude-hid (USB button listener) — personal hardware, not shared
- claude-worker system — separate package, may be shared later
- Theme engine integration — users manage overlay colors directly
