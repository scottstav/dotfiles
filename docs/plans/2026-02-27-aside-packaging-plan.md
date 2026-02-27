# aside — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Extract claude-ask/claude-voice/claude-overlay from dotfiles into a standalone, LLM-agnostic (LiteLLM) Wayland desktop assistant called "aside".

**Architecture:** Single Python daemon (aside-daemon) handles query dispatch, tool execution, TTS, and optional voice input. C overlay (aside-overlay) renders streaming text via Wayland layer-shell. GTK4 popup (aside-input) provides text input. All components communicate via Unix domain sockets with JSON messages.

**Tech Stack:** Python 3.11+, LiteLLM, GTK4/PyGObject, Kokoro TTS, OpenWakeWord, faster-whisper, C11/Cairo/Pango/Wayland, meson/ninja

**Source reference:** All existing code lives in the dotfiles repo at `/home/ifit/dotfiles/`. The new project will be created at `~/projects/aside/`.

---

### Task 1: Project Scaffold

**Files:**
- Create: `~/projects/aside/` (full directory tree)
- Create: `aside/pyproject.toml`
- Create: `aside/Makefile`
- Create: `aside/aside/__init__.py`
- Create: `aside/aside/config.py`

**Step 1: Create directory structure**

```bash
mkdir -p ~/projects/aside/{aside/{tools,voice,input},overlay/{src,protocols},data/waybar,plugins,tests}
```

**Step 2: Create pyproject.toml**

Create `~/projects/aside/pyproject.toml`:

```toml
[build-system]
requires = ["setuptools>=68.0"]
build-backend = "setuptools.backends._legacy:_Backend"

[project]
name = "aside"
version = "0.1.0"
description = "Wayland-native LLM desktop assistant"
readme = "README.md"
license = "MIT"
requires-python = ">=3.11"
dependencies = [
    "litellm>=1.30.0",
]

[project.optional-dependencies]
tts = ["kokoro>=0.9.4", "soundfile", "sounddevice"]
voice = ["openwakeword==0.6.0", "faster-whisper==1.2.1", "webrtcvad==2.0.10", "numpy>=2.0", "pyyaml>=6.0"]
gtk = ["PyGObject>=3.50"]

[project.scripts]
aside = "aside.cli:main"
aside-input = "aside.input.window:main"
aside-status = "aside.status:main"
```

**Step 3: Create `aside/__init__.py`**

```python
"""aside — Wayland-native LLM desktop assistant."""
__version__ = "0.1.0"
```

**Step 4: Initialize git repo**

```bash
cd ~/projects/aside && git init && git add -A && git commit -m "scaffold: initial project structure"
```

---

### Task 2: Configuration System

**Files:**
- Create: `aside/aside/config.py`
- Create: `aside/data/config.toml.example`
- Create: `aside/tests/test_config.py`

**Step 1: Write test for config loading**

Create `~/projects/aside/tests/test_config.py`:

```python
import tomllib
from pathlib import Path
from aside.config import load_config, DEFAULT_CONFIG

def test_default_config_has_required_keys():
    cfg = DEFAULT_CONFIG
    assert "model" in cfg
    assert cfg["model"]["name"] == "anthropic/claude-sonnet-4-6"
    assert "tts" in cfg
    assert "voice" in cfg
    assert "overlay" in cfg
    assert "storage" in cfg
    assert "plugins" in cfg

def test_load_config_returns_defaults_when_no_file(tmp_path):
    cfg = load_config(tmp_path / "nonexistent.toml")
    assert cfg["model"]["name"] == "anthropic/claude-sonnet-4-6"

def test_load_config_merges_user_overrides(tmp_path):
    user_cfg = tmp_path / "config.toml"
    user_cfg.write_text('[model]\nname = "openai/gpt-4o"\n')
    cfg = load_config(user_cfg)
    assert cfg["model"]["name"] == "openai/gpt-4o"
    # Other defaults preserved
    assert cfg["tts"]["model"] == "af_heart"

def test_load_config_deep_merges(tmp_path):
    user_cfg = tmp_path / "config.toml"
    user_cfg.write_text('[tts]\nspeed = 1.5\n')
    cfg = load_config(user_cfg)
    assert cfg["tts"]["speed"] == 1.5
    assert cfg["tts"]["model"] == "af_heart"  # preserved from defaults

def test_xdg_paths_resolve():
    cfg = DEFAULT_CONFIG
    # Storage paths should be empty strings (use defaults)
    assert cfg["storage"]["conversations_dir"] == ""
    assert cfg["storage"]["archive_dir"] == ""
```

**Step 2: Run test to verify it fails**

```bash
cd ~/projects/aside && python -m pytest tests/test_config.py -v
```

Expected: FAIL — `aside.config` doesn't exist yet.

**Step 3: Implement config.py**

Create `~/projects/aside/aside/config.py`:

```python
"""Configuration loading and defaults for aside."""
import tomllib
from copy import deepcopy
from pathlib import Path

DEFAULT_CONFIG = {
    "model": {
        "name": "anthropic/claude-sonnet-4-6",
        "system_prompt": "",
    },
    "input": {
        "terminal": "foot -e",
    },
    "voice": {
        "enabled": False,
        "wake_word_model": "",
        "wake_word_threshold": 0.5,
        "pre_roll_seconds": 0.5,
        "stt_model": "base",
        "stt_device": "cpu",
        "smart_silence": True,
        "silence_timeout": 2.5,
        "no_speech_timeout": 3.0,
        "force_send_phrases": ["send it", "that's it"],
    },
    "tts": {
        "enabled": False,
        "model": "af_heart",
        "speed": 1.0,
        "lang": "a",
        "filter": {
            "skip_code_blocks": True,
            "skip_urls": True,
        },
    },
    "overlay": {
        "font": "Sans 13",
        "width": 600,
        "max_lines": 40,
        "margin_top": 10,
        "padding_x": 20,
        "padding_y": 16,
        "corner_radius": 12,
        "border_width": 2,
        "accent_height": 3,
        "scroll_duration": 200,
        "fade_duration": 400,
        "colors": {
            "background": "#1a1b26e6",
            "foreground": "#c0caf5ff",
            "border": "#414868ff",
            "accent": "#7aa2f7ff",
        },
    },
    "storage": {
        "conversations_dir": "",
        "archive_dir": "",
    },
    "plugins": {
        "dirs": [],
    },
    "notifications": {
        "reply_command": "",
        "listen_command": "",
    },
    "status": {
        "signal": 12,
    },
}


def _deep_merge(base: dict, override: dict) -> dict:
    """Recursively merge override into base. Override wins on conflicts."""
    result = base.copy()
    for key, val in override.items():
        if key in result and isinstance(result[key], dict) and isinstance(val, dict):
            result[key] = _deep_merge(result[key], val)
        else:
            result[key] = val
    return result


def load_config(path: Path | None = None) -> dict:
    """Load config from TOML file, merging over defaults."""
    if path is None:
        xdg = Path.home() / ".config"
        config_home = Path(os.environ.get("XDG_CONFIG_HOME", str(xdg)))
        path = config_home / "aside" / "config.toml"
    path = Path(path)
    defaults = deepcopy(DEFAULT_CONFIG)
    if not path.exists():
        return defaults
    with open(path, "rb") as f:
        user = tomllib.load(f)
    return _deep_merge(defaults, user)


def resolve_state_dir(cfg: dict) -> Path:
    """Return the state directory, respecting XDG."""
    xdg = Path.home() / ".local" / "state"
    return Path(os.environ.get("XDG_STATE_HOME", str(xdg))) / "aside"


def resolve_conversations_dir(cfg: dict) -> Path:
    """Return the conversations directory."""
    d = cfg["storage"]["conversations_dir"]
    if d:
        return Path(d)
    return resolve_state_dir(cfg) / "conversations"


def resolve_socket_path(name: str = "aside.sock") -> Path:
    """Return the socket path under XDG_RUNTIME_DIR."""
    runtime = os.environ.get("XDG_RUNTIME_DIR", f"/run/user/{os.getuid()}")
    return Path(runtime) / name


import os  # noqa: E402 — keep import at end to avoid circular issues
```

**Step 4: Run tests**

```bash
cd ~/projects/aside && python -m pytest tests/test_config.py -v
```

Expected: PASS

**Step 5: Create example config file**

Create `~/projects/aside/data/config.toml.example` — copy the `[model]` through `[plugins]` section from the design doc verbatim.

**Step 6: Commit**

```bash
cd ~/projects/aside && git add -A && git commit -m "feat: config system with TOML loading and XDG paths"
```

---

### Task 3: State Management

**Files:**
- Create: `aside/aside/state.py`
- Create: `aside/tests/test_state.py`

Port conversation store, memory, and usage tracking from `dotfiles/.local/share/claude-ask/query.py` (lines 234-265, 422-439) and `waybar_state.py`.

**Step 1: Write test for ConversationStore**

```python
# tests/test_state.py
from aside.state import ConversationStore

def test_create_new_conversation(tmp_path):
    store = ConversationStore(tmp_path)
    conv = store.get_or_create()
    assert "id" in conv
    assert "created" in conv
    assert conv["messages"] == []

def test_save_and_load(tmp_path):
    store = ConversationStore(tmp_path)
    conv = store.get_or_create()
    conv["messages"].append({"role": "user", "content": "hello"})
    store.save(conv)
    loaded = store.get_or_create(conv["id"])
    assert loaded["messages"][0]["content"] == "hello"

def test_auto_resolve_recent(tmp_path):
    store = ConversationStore(tmp_path)
    conv = store.get_or_create()
    store.save(conv)
    store.save_last(conv["id"])
    resolved = store.auto_resolve(max_age_seconds=60)
    assert resolved == conv["id"]

def test_auto_resolve_stale(tmp_path):
    store = ConversationStore(tmp_path)
    conv = store.get_or_create()
    store.save(conv)
    store.save_last(conv["id"])
    resolved = store.auto_resolve(max_age_seconds=0)
    assert resolved is None
```

**Step 2: Run test to verify failure**

```bash
cd ~/projects/aside && python -m pytest tests/test_state.py -v
```

**Step 3: Implement state.py**

Port from `dotfiles/.local/share/claude-ask/query.py` (ConversationStore class, lines 234-265) and the auto-resolution logic (lines 457-467). Key changes:
- Constructor takes a directory path (instead of hardcoding `~/.local/state/claude-ask/conversations/`)
- `save_last()` and `auto_resolve()` work with a `last.json` file in the parent state dir
- Add `UsageLog` class ported from query.py lines 422-439 (append-only JSONL)
- Add `StatusState` class ported from `waybar_state.py` (write JSON status, signal bar)

Reference files:
- `dotfiles/.local/share/claude-ask/query.py:234-265` — ConversationStore
- `dotfiles/.local/share/claude-ask/query.py:422-439` — usage logging
- `dotfiles/.local/share/claude-ask/query.py:457-467` — auto-resolution
- `dotfiles/.local/share/claude-ask/waybar_state.py` — StatusState

Changes from original:
- No hardcoded paths — all paths passed as arguments or resolved from config
- StatusState: configurable signal number (from `config["status"]["signal"]`), not hardcoded SIGRTMIN+12
- StatusState: remove `reload_speak_enabled()` dependency on external waybar.json — maintain speak_enabled internally

**Step 4: Run tests**

```bash
cd ~/projects/aside && python -m pytest tests/test_state.py -v
```

**Step 5: Commit**

```bash
cd ~/projects/aside && git add -A && git commit -m "feat: conversation store, usage log, and status state"
```

---

### Task 4: Plugin System

**Files:**
- Create: `aside/aside/plugins.py`
- Create: `aside/aside/tools/clipboard.py`
- Create: `aside/aside/tools/shell.py`
- Create: `aside/aside/tools/memory.py`
- Create: `aside/tests/test_plugins.py`

**Step 1: Write test for plugin loader**

```python
# tests/test_plugins.py
from aside.plugins import load_tools, run_tool

def test_load_tools_from_directory(tmp_path):
    tool_file = tmp_path / "hello.py"
    tool_file.write_text('''
TOOL_SPEC = {
    "name": "hello",
    "description": "Says hello",
    "parameters": {"type": "object", "properties": {"name": {"type": "string"}}, "required": ["name"]}
}

def run(name: str) -> str:
    return f"Hello, {name}!"
''')
    tools = load_tools([tmp_path])
    assert len(tools) == 1
    assert tools[0]["function"]["name"] == "hello"

def test_run_tool(tmp_path):
    tool_file = tmp_path / "hello.py"
    tool_file.write_text('''
TOOL_SPEC = {
    "name": "hello",
    "description": "Says hello",
    "parameters": {"type": "object", "properties": {"name": {"type": "string"}}, "required": ["name"]}
}

def run(name: str) -> str:
    return f"Hello, {name}!"
''')
    result = run_tool("hello", {"name": "World"}, [tmp_path])
    assert result == "Hello, World!"

def test_skip_broken_plugin(tmp_path):
    bad = tmp_path / "broken.py"
    bad.write_text("raise RuntimeError('oops')")
    tools = load_tools([tmp_path])
    assert len(tools) == 0
```

**Step 2: Run test to verify failure**

```bash
cd ~/projects/aside && python -m pytest tests/test_plugins.py -v
```

**Step 3: Implement plugins.py**

The plugin contract from the design doc:
- Each `.py` file has `TOOL_SPEC` (OpenAI function calling schema) and `run(**kwargs) -> str`
- `load_tools(dirs)` scans directories, imports modules, returns list of OpenAI-format tool dicts
- `run_tool(name, input_data, dirs)` finds and executes the matching tool

Key difference from current code (`query.py` lines 271-312):
- Current code uses `name`, `description`, `input_schema` as module-level globals (Anthropic format)
- New code uses `TOOL_SPEC` dict (OpenAI format: `{"name", "description", "parameters"}`)
- `load_tools()` returns OpenAI-format tool specs: `[{"type": "function", "function": {"name": ..., "description": ..., "parameters": ...}}]`
- `run()` receives keyword arguments unpacked from the input dict, not a single dict argument

**Step 4: Port built-in tools**

Port from `dotfiles/.local/share/claude-ask/tools/`:

**clipboard.py** — copy from `dotfiles/.local/share/claude-ask/tools/clipboard.py`. Change: use `TOOL_SPEC` format, `run()` takes keyword args. Keep `wl-copy`/`wl-paste` dependency (Wayland-native).

**shell.py** — copy from `dotfiles/.local/share/claude-ask/tools/shell.py`. Change: `TOOL_SPEC` format, keyword args. Keep 30s timeout, 4000 char limit.

**memory.py** — copy from `dotfiles/.local/share/claude-ask/tools/memory.py`. Change: `TOOL_SPEC` format, keyword args. Critical change: memory file path should be passed as an environment variable or discovered from XDG state dir, NOT hardcoded to `~/.local/state/claude-ask/memory.md`. Use `Path(os.environ.get("XDG_STATE_HOME", Path.home() / ".local" / "state")) / "aside" / "memory.md"`.

**Step 5: Run tests**

```bash
cd ~/projects/aside && python -m pytest tests/test_plugins.py -v
```

**Step 6: Port example plugins**

Create `~/projects/aside/plugins/` with screenshot.py, web_search.py, fetch_url.py. These are NOT installed — they live in the `plugins/` directory as examples. Copy from dotfiles and adapt to `TOOL_SPEC` format.

**Step 7: Commit**

```bash
cd ~/projects/aside && git add -A && git commit -m "feat: plugin system with built-in tools (clipboard, shell, memory)"
```

---

### Task 5: Sentence Buffer (Pure Logic)

**Files:**
- Create: `aside/aside/sentence_buffer.py`
- Copy: `aside/tests/test_sentence_buffer.py`

**Step 1: Copy existing test**

Copy `dotfiles/.local/share/claude-ask/test_sentence_buffer.py` to `~/projects/aside/tests/test_sentence_buffer.py`. Adjust imports from `sentence_buffer` to `aside.sentence_buffer`.

**Step 2: Copy implementation**

Copy `dotfiles/.local/share/claude-ask/sentence_buffer.py` to `~/projects/aside/aside/sentence_buffer.py`. No changes needed — this is pure logic with no external dependencies.

**Step 3: Run tests**

```bash
cd ~/projects/aside && python -m pytest tests/test_sentence_buffer.py -v
```

**Step 4: Commit**

```bash
cd ~/projects/aside && git add -A && git commit -m "feat: sentence buffer for streaming text-to-sentence chunking"
```

---

### Task 6: TTS Pipeline

**Files:**
- Create: `aside/aside/tts.py`
- Create: `aside/tests/test_tts.py`

**Step 1: Write test for TTS config handling**

```python
# tests/test_tts.py
from aside.tts import TTSPipeline

def test_init_without_starting():
    """Pipeline should be creatable without loading kokoro."""
    tts = TTSPipeline(model="af_heart", speed=1.0, lang="a")
    assert not tts._running

def test_update_config():
    tts = TTSPipeline()
    tts.update_config(model="af_sky", speed=1.5, lang="a")
    assert tts._model == "af_sky"
    assert tts._speed == 1.5
```

**Step 2: Copy and adapt tts.py**

Copy `dotfiles/.local/share/claude-ask/tts.py` to `~/projects/aside/aside/tts.py`. Changes:
- Remove hardcoded `sd.default.device[1] = "pipewire"` — let sounddevice use system default (PipeWire is typically the default on Wayland systems anyway)
- Constructor takes config values, no hardcoded defaults
- Keep lazy-loading of kokoro (only imported when `start()` is called)

**Step 3: Run tests**

```bash
cd ~/projects/aside && python -m pytest tests/test_tts.py -v
```

**Step 4: Commit**

```bash
cd ~/projects/aside && git add -A && git commit -m "feat: TTS pipeline with Kokoro synthesis and PipeWire playback"
```

---

### Task 7: LiteLLM Query Engine

**Files:**
- Create: `aside/aside/query.py`
- Create: `aside/tests/test_query.py`

This is the core — port from `dotfiles/.local/share/claude-ask/query.py` but replace the Anthropic SDK with LiteLLM.

**Step 1: Write test for tool schema conversion**

```python
# tests/test_query.py
from aside.query import stream_response, _build_messages

def test_build_messages_text_only():
    msgs = _build_messages("Hello", [])
    assert msgs[-1] == {"role": "user", "content": "Hello"}

def test_build_messages_with_history():
    history = [
        {"role": "user", "content": "Hi"},
        {"role": "assistant", "content": "Hello!"},
    ]
    msgs = _build_messages("How are you?", history)
    assert len(msgs) == 3
    assert msgs[-1]["content"] == "How are you?"
```

**Step 2: Implement query.py**

Port from `dotfiles/.local/share/claude-ask/query.py`. This is the biggest single file change. Key transformations:

**API call (lines 474-527 in original):**
```python
# BEFORE (Anthropic):
with client.messages.stream(model=model, max_tokens=4096, system=system, messages=msgs, tools=tools) as stream:
    for event in stream:
        if event.type == "content_block_delta" and event.delta.type == "text_delta":
            text = event.delta.text

# AFTER (LiteLLM):
response = litellm.completion(model=model, messages=msgs, tools=tools, stream=True, max_tokens=4096)
for chunk in response:
    delta = chunk.choices[0].delta
    if delta.content:
        text = delta.content
    if delta.tool_calls:
        # accumulate tool call arguments (streamed incrementally)
```

**Tool handling:**
- Anthropic returns `tool_use` content blocks with `name` and `input` complete
- LiteLLM/OpenAI streams `tool_calls` incrementally — need to accumulate `function.arguments` across chunks then JSON-parse at end
- Tool results sent back as `{"role": "tool", "tool_call_id": ..., "content": ...}` (OpenAI format) instead of `{"role": "user", "content": [{"type": "tool_result", ...}]}` (Anthropic format)

**System prompt:**
- Anthropic: separate `system` parameter
- LiteLLM/OpenAI: `{"role": "system", "content": ...}` as first message

**API key:**
- Remove `_load_api_key()` (GPG decrypt from authinfo)
- LiteLLM reads from environment variables automatically (`ANTHROPIC_API_KEY`, `OPENAI_API_KEY`, etc.)

**Overlay/TTS integration:**
- Keep overlay socket protocol unchanged (`{"cmd": "open"}`, `{"cmd": "text", "data": "..."}`, etc.)
- Keep TTS sentence buffer integration unchanged
- Remove `_voice_control()` — daemon handles mute/unmute internally

**Notification:**
- Keep `notify-send` calls
- Make action commands configurable (no hardcoded `emacsclient`, `claude-ask`)
- Action "reply" command comes from `config["notifications"]["reply_command"]`

**Functions to port:**
- `stream_response()` — core streaming loop (REWRITE for LiteLLM)
- `send_query()` — orchestration (port with config-driven paths)
- `notify()`, `notify_final()` — notifications (port, make action commands configurable)
- `_overlay_send()`, `_connect_overlay()` — overlay IPC (port unchanged)
- `_build_system_prompt()` — system prompt (port, use config)

**Functions to remove:**
- `_load_api_key()` — LiteLLM handles API keys
- `_get_client()` — LiteLLM is stateless
- `auto_resolve_conversation()` — moved to state.py
- `ConversationStore` — moved to state.py

**Step 3: Run tests**

```bash
cd ~/projects/aside && python -m pytest tests/test_query.py -v
```

**Step 4: Commit**

```bash
cd ~/projects/aside && git add -A && git commit -m "feat: LiteLLM query engine with streaming and tool execution"
```

---

### Task 8: C Overlay Port

**Files:**
- Copy: all `overlay/src/*.c`, `overlay/src/*.h`
- Copy: `overlay/protocols/wlr-layer-shell-unstable-v1.xml`
- Copy: `overlay/meson.build`

**Step 1: Copy overlay source**

Copy from `dotfiles/.local/share/claude-overlay/` to `~/projects/aside/overlay/`:
- `src/main.c`, `wayland.c/h`, `render.c/h`, `socket.c/h`, `config.c/h`, `animation.c/h`, `shm.c/h`
- `protocols/wlr-layer-shell-unstable-v1.xml`
- `meson.build`

**Step 2: Rename paths in main.c**

In `overlay/src/main.c`:
- Line 70-78: Change config path from `claude-overlay` to `aside`:
  ```c
  // BEFORE:
  "%s/claude-overlay/config"
  // AFTER:
  "%s/aside/overlay.conf"
  ```
- Lines 23-41 (`send_daemon_action()`): Change daemon socket from `claude-voice.sock` to `aside.sock`
- Line 99-111: Change default socket path from `claude-overlay.sock` to `aside-overlay.sock`

In `overlay/src/shm.c`:
- Line 14: Change SHM name from `claude-overlay` to `aside-overlay`

In `overlay/src/config.c`:
- No format changes needed — KEY=VALUE parsing stays the same. The overlay reads a separate config file (not the TOML). The Makefile install step will generate `~/.config/aside/overlay.conf` from the TOML `[overlay]` section, OR the daemon can write it on startup. Simplest: overlay reads its own KEY=VALUE file at `$XDG_CONFIG_HOME/aside/overlay.conf`, and the daemon writes that file from `config.toml` on startup.

**Step 3: Verify build**

```bash
cd ~/projects/aside/overlay && meson setup build && ninja -C build
```

Expected: Builds successfully, binary at `overlay/build/aside-overlay` (update `meson.build` line 55 to change binary name from `claude-overlay` to `aside-overlay`).

**Step 4: Commit**

```bash
cd ~/projects/aside && git add -A && git commit -m "feat: port Wayland overlay (rename paths, build verified)"
```

---

### Task 9: Daemon (Socket Server + Orchestration)

**Files:**
- Create: `aside/aside/daemon.py`
- Create: `aside/tests/test_daemon.py`

Port and merge from `dotfiles/.local/share/claude-voice/daemon.py` and the query dispatch logic.

**Step 1: Write test for socket protocol**

```python
# tests/test_daemon.py
import json

def test_parse_query_command():
    from aside.daemon import parse_command
    msg = json.dumps({"action": "query", "text": "hello", "conversation_id": None})
    cmd = parse_command(msg)
    assert cmd["action"] == "query"
    assert cmd["text"] == "hello"

def test_parse_cancel_command():
    from aside.daemon import parse_command
    msg = json.dumps({"action": "cancel"})
    cmd = parse_command(msg)
    assert cmd["action"] == "cancel"
```

**Step 2: Implement daemon.py**

The daemon merges two existing components:

1. **Control socket server** — from `dotfiles/.local/share/claude-voice/daemon.py` lines 342-442 (asyncio Unix socket server)
2. **Voice listener** — from `dotfiles/.local/share/claude-voice/daemon.py` lines 61-336 (wake word + capture + transcribe)
3. **Query dispatch** — calls `query.py:send_query()` in background thread

Key changes from original:
- Single daemon process (not separate claude-voice + claude-ask)
- Socket at `$XDG_RUNTIME_DIR/aside.sock` (was `claude-voice.sock`)
- Config from unified `config.toml` (was separate YAML files)
- Voice features imported conditionally (only if `voice.enabled` in config and deps available)
- Overlay config file generation on startup: read `[overlay]` from config.toml, write `$XDG_CONFIG_HOME/aside/overlay.conf` in KEY=VALUE format

**Socket protocol (unchanged from current):**

| Action | Payload | Effect |
|--------|---------|--------|
| `query` | `{text, conversation_id, image, file}` | Start query thread |
| `listen` | `{conversation_id}` | Queue voice capture |
| `cancel` | — | Cancel running query |
| `stop_tts` | — | Stop TTS playback only |
| `mute` | — | Mute wake word |
| `unmute` | — | Unmute wake word |
| `toggle-mute` | — | Toggle, return `{muted: bool}` |

**Structure:**
```python
class Daemon:
    def __init__(self, config: dict):
        self.config = config
        self.state = StatusState(...)
        self.store = ConversationStore(...)
        # Optional voice:
        if config["voice"]["enabled"]:
            from aside.voice.listener import VoiceListener
            self.voice = VoiceListener(config)

    async def handle_client(self, reader, writer):
        """Process JSON commands from socket clients."""

    def start_query(self, text, conversation_id=None, image=None, file=None):
        """Dispatch query to background thread."""

    def run(self):
        """Main entry point: start socket server + optional voice loop."""
```

**Step 3: Run tests**

```bash
cd ~/projects/aside && python -m pytest tests/test_daemon.py -v
```

**Step 4: Commit**

```bash
cd ~/projects/aside && git add -A && git commit -m "feat: daemon with socket server, query dispatch, optional voice"
```

---

### Task 10: Voice Input (Optional Module)

**Files:**
- Create: `aside/aside/voice/listener.py`
- Create: `aside/aside/voice/audio.py`
- Create: `aside/aside/voice/speech_detector.py`
- Create: `aside/aside/voice/stt.py`
- Create: `aside/aside/voice/__init__.py`

**Step 1: Port audio.py**

Copy `dotfiles/.local/share/claude-voice/audio.py` to `aside/aside/voice/audio.py`. No changes — it's already clean (uses PipeWire via `pw-record`, no hardcoded paths).

**Step 2: Port speech_detector.py**

Copy `dotfiles/.local/share/claude-voice/speech_detector.py` to `aside/aside/voice/speech_detector.py`. No changes — pure logic, no external deps beyond config values.

**Step 3: Port stt.py**

Copy `dotfiles/.local/share/claude-voice/stt.py` to `aside/aside/voice/stt.py`. No changes — already clean with lazy model loading.

**Step 4: Port wake_word.py**

Copy `dotfiles/.local/share/claude-voice/wake_word.py`. Change: model path comes from config instead of hardcoded `~/.local/share/claude-voice/models/ok_computer.onnx`. The model file is user-provided (documented in README).

**Step 5: Create listener.py**

Port the `Daemon` class logic from `dotfiles/.local/share/claude-voice/daemon.py` (lines 61-336) into `VoiceListener`. This class:
- Owns `AudioPipeline`, `WakeWordListener`, `SpeechEndDetector`
- Provides `run_wake_word_loop()` that the daemon calls in a thread
- Calls back to daemon's `start_query()` when speech is captured and transcribed

**Step 6: Port notify.py helpers**

Copy notification helper functions from `dotfiles/.local/share/claude-voice/notify.py` into `aside/aside/voice/listener.py` (they're small — 5 functions, ~40 lines total). Change notification tag from `claude-voice` to `aside`.

**Step 7: Run tests (smoke test the imports)**

```bash
cd ~/projects/aside && python -c "from aside.voice import listener; print('voice module loads')"
```

**Step 8: Commit**

```bash
cd ~/projects/aside && git add -A && git commit -m "feat: voice input module (wake word, STT, speech detection)"
```

---

### Task 11: GTK4 Input Window

**Files:**
- Create: `aside/aside/input/__init__.py`
- Create: `aside/aside/input/window.py`

This is new code — no equivalent in current system.

**Step 1: Design the GTK4 window**

The input window is a floating popup with:
- A conversation list (recent conversations, scrollable)
- A multiline text entry at the bottom
- File/image drag-and-drop support
- Submit on Ctrl+Enter (or configurable)
- Escape to close

It connects to the daemon socket and sends `{"action": "query", "text": "...", "conversation_id": "..."}`.

**Step 2: Implement window.py**

```python
"""GTK4 input popup for aside."""
import gi
gi.require_version("Gtk", "4.0")
gi.require_version("Adw", "1")
gi.require_version("Gtk4LayerShell", "1.0")
from gi.repository import Gtk, Adw, Gdk, Gio, GLib, Gtk4LayerShell
import json
import socket
from pathlib import Path
from aside.config import load_config, resolve_socket_path, resolve_conversations_dir


class AsideInput(Adw.Application):
    def __init__(self):
        super().__init__(application_id="dev.aside.input")
        self.config = load_config()
        self.connect("activate", self.on_activate)

    def on_activate(self, app):
        win = Adw.ApplicationWindow(application=app)
        win.set_default_size(500, 400)
        win.set_title("aside")

        # Layer shell setup (floating popup)
        Gtk4LayerShell.init_for_window(win)
        Gtk4LayerShell.set_layer(win, Gtk4LayerShell.Layer.TOP)
        Gtk4LayerShell.set_keyboard_mode(win, Gtk4LayerShell.KeyboardMode.ON_DEMAND)

        # Main layout
        box = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=8)
        box.set_margin_top(12)
        box.set_margin_bottom(12)
        box.set_margin_start(12)
        box.set_margin_end(12)

        # Conversation list
        self.conv_list = self._build_conversation_list()
        scrolled = Gtk.ScrolledWindow(vexpand=True)
        scrolled.set_child(self.conv_list)
        box.append(scrolled)

        # Text entry
        self.text_view = Gtk.TextView(wrap_mode=Gtk.WrapMode.WORD_CHAR, vexpand=False)
        self.text_view.set_size_request(-1, 80)
        # Ctrl+Enter to submit
        controller = Gtk.EventControllerKey()
        controller.connect("key-pressed", self.on_key_pressed)
        self.text_view.add_controller(controller)
        box.append(self.text_view)

        win.set_content(box)
        # Escape to close
        esc = Gtk.ShortcutController()
        esc.add_shortcut(Gtk.Shortcut(
            trigger=Gtk.ShortcutTrigger.parse_string("Escape"),
            action=Gtk.CallbackAction.new(lambda *a: win.close()),
        ))
        win.add_controller(esc)
        win.present()

    def on_key_pressed(self, controller, keyval, keycode, state):
        if keyval == Gdk.KEY_Return and (state & Gdk.ModifierType.CONTROL_MASK):
            self.submit()
            return True
        return False

    def submit(self):
        buf = self.text_view.get_buffer()
        text = buf.get_text(buf.get_start_iter(), buf.get_end_iter(), False).strip()
        if not text:
            return
        msg = {"action": "query", "text": text, "conversation_id": self.selected_conv_id}
        self._send_to_daemon(msg)
        self.quit()

    def _send_to_daemon(self, msg):
        sock_path = resolve_socket_path()
        try:
            s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            s.connect(str(sock_path))
            s.sendall((json.dumps(msg) + "\n").encode())
            s.close()
        except (ConnectionRefusedError, FileNotFoundError):
            pass  # daemon not running

    def _build_conversation_list(self):
        # Load recent conversations from state dir
        # Return a Gtk.ListBox with conversation titles/dates
        ...

def main():
    app = AsideInput()
    app.run()
```

This is ~200-300 lines for v1. The conversation list and drag-and-drop can be iterated on.

**Step 3: Test manually**

```bash
cd ~/projects/aside && python -m aside.input.window
```

Verify: Window appears, text entry works, Escape closes, Ctrl+Enter submits.

**Step 4: Commit**

```bash
cd ~/projects/aside && git add -A && git commit -m "feat: GTK4 input popup with conversation picker"
```

---

### Task 12: CLI Entry Points

**Files:**
- Create: `aside/aside/cli.py`

**Step 1: Implement cli.py**

```python
"""CLI entry points for aside."""
import argparse
import json
import socket
import sys
from aside.config import load_config, resolve_socket_path


def _send(msg: dict) -> str | None:
    """Send JSON to daemon, optionally read response."""
    sock_path = resolve_socket_path()
    try:
        s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        s.connect(str(sock_path))
        s.sendall((json.dumps(msg) + "\n").encode())
        # Try to read response (1s timeout)
        s.settimeout(1.0)
        try:
            data = s.recv(4096)
            return data.decode().strip()
        except socket.timeout:
            return None
        finally:
            s.close()
    except (ConnectionRefusedError, FileNotFoundError):
        print("aside: daemon not running", file=sys.stderr)
        sys.exit(1)


def main():
    parser = argparse.ArgumentParser(prog="aside")
    sub = parser.add_subparsers(dest="command")

    # aside query "..."
    q = sub.add_parser("query", help="Send a query")
    q.add_argument("text", help="Query text")
    q.add_argument("--conversation", "-c", help="Conversation ID")
    q.add_argument("--new", action="store_true", help="Start new conversation")

    # aside cancel
    sub.add_parser("cancel", help="Cancel running query")

    # aside stop-tts
    sub.add_parser("stop-tts", help="Stop TTS playback")

    # aside status
    sub.add_parser("status", help="Print current status JSON")

    args = parser.parse_args()

    if args.command == "query":
        conv_id = "__new__" if args.new else args.conversation
        _send({"action": "query", "text": args.text, "conversation_id": conv_id})
    elif args.command == "cancel":
        _send({"action": "cancel"})
    elif args.command == "stop-tts":
        _send({"action": "stop_tts"})
    elif args.command == "status":
        from aside.config import resolve_state_dir
        cfg = load_config()
        state_file = resolve_state_dir(cfg) / "status.json"
        if state_file.exists():
            print(state_file.read_text())
        else:
            print("{}")
    else:
        parser.print_help()
```

**Step 2: Test**

```bash
cd ~/projects/aside && python -m aside.cli --help
```

**Step 3: Commit**

```bash
cd ~/projects/aside && git add -A && git commit -m "feat: CLI entry points (query, cancel, stop-tts, status)"
```

---

### Task 13: Status Bar Module

**Files:**
- Create: `aside/aside/status.py`
- Create: `aside/data/waybar/aside.json`

**Step 1: Port status script**

Port from `dotfiles/.local/bin/claude-ask-status`. This becomes `aside/aside/status.py` with a `main()` entry point.

Changes from original:
- Read state from `$XDG_STATE_HOME/aside/status.json` (not `~/.local/state/claude-ask/waybar.json`)
- Model name display: support LiteLLM model format (`provider/model` → show just model part)
- Remove hardcoded color hex values — read from config or use sensible defaults
- Remove worker state reading (workers are out of scope)
- Output JSON compatible with waybar custom module format

**Step 2: Create waybar config snippet**

Create `~/projects/aside/data/waybar/aside.json`:

```json
{
    "custom/aside": {
        "exec": "aside-status",
        "return-type": "json",
        "signal": 12,
        "interval": "once",
        "on-click": "aside-input",
        "on-scroll-up": "aside model-cycle up",
        "on-scroll-down": "aside model-cycle down"
    }
}
```

**Step 3: Commit**

```bash
cd ~/projects/aside && git add -A && git commit -m "feat: waybar status module"
```

---

### Task 14: Systemd Units and Desktop Entry

**Files:**
- Create: `aside/data/aside-daemon.service`
- Create: `aside/data/aside-overlay.service`
- Create: `aside/data/aside.desktop`

**Step 1: Create systemd unit for daemon**

Create `~/projects/aside/data/aside-daemon.service`:

```ini
[Unit]
Description=aside - LLM desktop assistant daemon
After=graphical-session.target

[Service]
Type=simple
Environment=PYTHONUNBUFFERED=1
ExecStart=%h/.local/lib/aside/venv/bin/python3 -m aside.daemon
Restart=on-failure
RestartSec=5

[Install]
WantedBy=default.target
```

**Step 2: Create systemd unit for overlay**

Create `~/projects/aside/data/aside-overlay.service`:

```ini
[Unit]
Description=aside - Wayland text overlay
After=graphical-session.target

[Service]
Type=simple
ExecStart=%h/.local/bin/aside-overlay
Restart=on-failure
RestartSec=2

[Install]
WantedBy=default.target
```

**Step 3: Create .desktop file**

Create `~/projects/aside/data/aside.desktop`:

```ini
[Desktop Entry]
Type=Application
Name=aside
Comment=Wayland-native LLM desktop assistant
Exec=aside-input
Icon=utilities-terminal
Categories=Utility;
Keywords=ai;llm;assistant;voice;
```

**Step 4: Commit**

```bash
cd ~/projects/aside && git add -A && git commit -m "feat: systemd units and desktop entry"
```

---

### Task 15: Makefile and Installation

**Files:**
- Create: `aside/Makefile`

**Step 1: Write Makefile**

```makefile
PREFIX ?= $(HOME)/.local
BINDIR = $(PREFIX)/bin
LIBDIR = $(PREFIX)/lib/aside
SYSTEMD_DIR = $(PREFIX)/lib/systemd/user
DESKTOP_DIR = $(PREFIX)/share/applications
CONFIG_DIR = $(HOME)/.config/aside

PYTHON ?= python3

.PHONY: all overlay install uninstall clean

all: overlay

overlay:
	cd overlay && meson setup build --prefix=$(PREFIX) && ninja -C build

install: overlay
	# Overlay binary
	install -Dm755 overlay/build/aside-overlay $(BINDIR)/aside-overlay
	# Python venv
	$(PYTHON) -m venv $(LIBDIR)/venv
	$(LIBDIR)/venv/bin/pip install --upgrade pip
	$(LIBDIR)/venv/bin/pip install .
	# Wrapper scripts
	@echo '#!/bin/sh' > $(BINDIR)/aside
	@echo 'exec $(LIBDIR)/venv/bin/python3 -m aside.cli "$$@"' >> $(BINDIR)/aside
	chmod 755 $(BINDIR)/aside
	@echo '#!/bin/sh' > $(BINDIR)/aside-input
	@echo 'exec $(LIBDIR)/venv/bin/python3 -m aside.input.window "$$@"' >> $(BINDIR)/aside-input
	chmod 755 $(BINDIR)/aside-input
	@echo '#!/bin/sh' > $(BINDIR)/aside-status
	@echo 'exec $(LIBDIR)/venv/bin/python3 -m aside.status "$$@"' >> $(BINDIR)/aside-status
	chmod 755 $(BINDIR)/aside-status
	# Systemd units
	install -Dm644 data/aside-daemon.service $(SYSTEMD_DIR)/aside-daemon.service
	install -Dm644 data/aside-overlay.service $(SYSTEMD_DIR)/aside-overlay.service
	# Desktop entry
	install -Dm644 data/aside.desktop $(DESKTOP_DIR)/aside.desktop
	# Example config (don't overwrite existing)
	@if [ ! -f $(CONFIG_DIR)/config.toml ]; then \
		install -Dm644 data/config.toml.example $(CONFIG_DIR)/config.toml; \
	fi
	@echo "Installed. Run: systemctl --user enable --now aside-daemon aside-overlay"

install-extras-tts:
	$(LIBDIR)/venv/bin/pip install ".[tts]"

install-extras-voice:
	$(LIBDIR)/venv/bin/pip install ".[voice]"

install-extras-gtk:
	$(LIBDIR)/venv/bin/pip install ".[gtk]"

uninstall:
	rm -f $(BINDIR)/aside $(BINDIR)/aside-input $(BINDIR)/aside-status $(BINDIR)/aside-overlay
	rm -rf $(LIBDIR)
	rm -f $(SYSTEMD_DIR)/aside-daemon.service $(SYSTEMD_DIR)/aside-overlay.service
	rm -f $(DESKTOP_DIR)/aside.desktop
	systemctl --user disable aside-daemon aside-overlay 2>/dev/null || true
	@echo "Uninstalled. Config at $(CONFIG_DIR) preserved."

clean:
	rm -rf overlay/build
```

**Step 2: Test build**

```bash
cd ~/projects/aside && make all
```

**Step 3: Commit**

```bash
cd ~/projects/aside && git add -A && git commit -m "feat: Makefile for build, install, and uninstall"
```

---

### Task 16: PKGBUILD (AUR)

**Files:**
- Create: `aside/PKGBUILD`

**Step 1: Write PKGBUILD**

```bash
# Maintainer: Your Name <you@example.com>
pkgname=aside
pkgver=0.1.0
pkgrel=1
pkgdesc="Wayland-native LLM desktop assistant with voice, overlay, and plugin support"
arch=('x86_64')
url="https://github.com/you/aside"
license=('MIT')
depends=('python>=3.11' 'wayland' 'cairo' 'pango' 'json-c' 'python-litellm')
makedepends=('meson' 'ninja' 'wayland-protocols' 'python-setuptools')
optdepends=(
    'gtk4: GTK4 input popup'
    'python-gobject: GTK4 input popup'
    'gtk4-layer-shell: GTK4 layer shell support'
    'python-kokoro: Text-to-speech'
    'python-sounddevice: Audio playback for TTS'
    'python-soundfile: Audio file handling for TTS'
    'python-openwakeword: Wake word detection'
    'python-faster-whisper: Speech-to-text'
)
source=("$pkgname-$pkgver.tar.gz::$url/archive/v$pkgver.tar.gz")
sha256sums=('SKIP')

build() {
    cd "$pkgname-$pkgver"
    cd overlay && meson setup build --prefix=/usr && ninja -C build
}

package() {
    cd "$pkgname-$pkgver"
    # Overlay binary
    install -Dm755 overlay/build/aside-overlay "$pkgdir/usr/bin/aside-overlay"
    # Python package
    python -m installer --destdir="$pkgdir" dist/*.whl
    # Systemd units
    install -Dm644 data/aside-daemon.service "$pkgdir/usr/lib/systemd/user/aside-daemon.service"
    install -Dm644 data/aside-overlay.service "$pkgdir/usr/lib/systemd/user/aside-overlay.service"
    # Desktop entry
    install -Dm644 data/aside.desktop "$pkgdir/usr/share/applications/aside.desktop"
    # Example config
    install -Dm644 data/config.toml.example "$pkgdir/usr/share/aside/config.toml.example"
    # License
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
```

**Step 2: Commit**

```bash
cd ~/projects/aside && git add -A && git commit -m "feat: AUR PKGBUILD"
```

---

### Task 17: README and License

**Files:**
- Create: `aside/README.md`
- Create: `aside/LICENSE`

**Step 1: Write README**

Cover:
- What aside is (1-2 sentences)
- Screenshot/demo GIF placeholder
- Features list (LLM-agnostic, Wayland overlay, voice input, TTS, plugin system)
- Quick start (install, configure API key, start services, use)
- Configuration reference (link to config.toml.example)
- Plugin development guide (TOOL_SPEC + run contract)
- Dependencies (required vs optional)
- Architecture diagram (from design doc)

**Step 2: Add MIT license**

**Step 3: Commit**

```bash
cd ~/projects/aside && git add -A && git commit -m "docs: README and MIT license"
```

---

### Task 18: Integration Test — Full Stack Smoke Test

**Step 1: Build everything**

```bash
cd ~/projects/aside && make clean && make all
```

**Step 2: Install locally**

```bash
cd ~/projects/aside && make install
```

**Step 3: Configure**

```bash
# Set API key
export ANTHROPIC_API_KEY="sk-..."

# Start services
systemctl --user start aside-overlay
```

**Step 4: Test CLI query**

```bash
aside query "What is 2+2?"
```

Verify: overlay appears with streaming response, fades out after completion.

**Step 5: Test GTK4 input**

```bash
aside-input
```

Verify: popup window appears, can type and submit.

**Step 6: Test status**

```bash
aside status
```

Verify: JSON output with status, model, usage.

**Step 7: Verify notifications**

Check that `notify-send` fires on query completion.

**Step 8: Commit any fixes**

```bash
cd ~/projects/aside && git add -A && git commit -m "fix: integration test fixes"
```

---

## Task Dependency Graph

```
Task 1 (scaffold)
  ├── Task 2 (config)
  │     ├── Task 3 (state)
  │     ├── Task 4 (plugins)
  │     ├── Task 6 (TTS)
  │     └── Task 7 (query engine) ← depends on 3, 4, 5, 6
  │           └── Task 9 (daemon) ← depends on 7, 10
  │                 ├── Task 11 (GTK4 input)
  │                 ├── Task 12 (CLI)
  │                 └── Task 13 (status)
  ├── Task 5 (sentence buffer) — no deps
  ├── Task 8 (overlay C port) — no deps beyond scaffold
  └── Task 10 (voice) ← depends on 2
        └── Task 9 (daemon)

Task 14 (systemd) ← depends on 9, 8
Task 15 (Makefile) ← depends on all code tasks
Task 16 (PKGBUILD) ← depends on 15
Task 17 (README) ← depends on all
Task 18 (integration test) ← depends on all
```

## Parallelizable Groups

These tasks have no interdependencies and can be worked on simultaneously:

**Group A** (after Task 2): Tasks 3, 4, 5, 6, 8, 10
**Group B** (after Group A): Tasks 7, 11, 12, 13
**Group C** (after all code): Tasks 14, 15, 16, 17
