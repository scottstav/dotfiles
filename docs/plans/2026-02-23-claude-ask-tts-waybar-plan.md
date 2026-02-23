# Claude-Ask TTS & Waybar Module Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add Kokoro TTS voice output and a Waybar status module to claude-ask, so Claude can speak responses aloud when toggled on.

**Architecture:** TTS integrates directly into `daemon.py`. A sentence buffer detects boundaries in the streaming text, feeds sentences to Kokoro for synthesis in a thread, and plays audio via sounddevice. A JSON state file bridges the daemon and a new Waybar module that shows status and provides a speak toggle.

**Tech Stack:** Python 3 (kokoro, sounddevice, soundfile), Bash (Waybar scripts), TOML config, Waybar JSON module

---

### Task 1: Install Dependencies and Create Config

**Files:**
- Modify: `.local/share/claude-ask/requirements.txt`
- Create: `.config/claude-ask/config.toml`

**Step 1: Update requirements.txt**

Add to `.local/share/claude-ask/requirements.txt`:

```
kokoro>=0.9.4
soundfile
sounddevice
```

Note: `tomli` is not needed — Python 3.11+ has `tomllib` in stdlib. The daemon runs on Arch which ships Python 3.12+.

**Step 2: Install system dependency**

```bash
sudo pacman -S --needed espeak-ng
```

**Step 3: Install Python dependencies**

```bash
cd ~/.local/share/claude-ask && .venv/bin/pip install -r requirements.txt
```

**Step 4: Create config file**

Create `.config/claude-ask/config.toml`:

```toml
[voice]
enabled = false
model = "af_heart"
speed = 1.0
lang = "a"

[voice.filter]
skip_code_blocks = true
skip_urls = true
```

**Step 5: Stow the config**

```bash
cd ~/dotfiles && stow .
```

**Step 6: Commit**

```bash
git add .local/share/claude-ask/requirements.txt .config/claude-ask/config.toml
git commit -m "feat(claude-ask): add TTS dependencies and voice config"
```

---

### Task 2: Create the Sentence Buffer Module

This is pure logic with no external dependencies — easily testable.

**Files:**
- Create: `.local/share/claude-ask/sentence_buffer.py`
- Create: `.local/share/claude-ask/test_sentence_buffer.py`

**Step 1: Write the failing test**

Create `.local/share/claude-ask/test_sentence_buffer.py`:

```python
"""Tests for the sentence buffer."""

from sentence_buffer import SentenceBuffer


def test_basic_sentence_detection():
    buf = SentenceBuffer()
    sentences = buf.add("Hello world. How are you? ")
    assert sentences == ["Hello world.", "How are you?"]


def test_partial_sentence_buffered():
    buf = SentenceBuffer()
    assert buf.add("Hello wor") == []
    assert buf.add("ld. ") == ["Hello world."]


def test_newline_is_boundary():
    buf = SentenceBuffer()
    sentences = buf.add("First line.\nSecond line.\n")
    assert sentences == ["First line.", "Second line."]


def test_flush_returns_remainder():
    buf = SentenceBuffer()
    buf.add("Incomplete sentence")
    assert buf.flush() == ["Incomplete sentence"]
    assert buf.flush() == []


def test_code_block_skipped():
    buf = SentenceBuffer()
    sentences = buf.add("Here is code:\n```python\nprint('hello')\n```\nDone.\n")
    # "Here is code:" spoken, code block skipped, "Done." spoken
    assert "Here is code:" in sentences
    assert "Done." in sentences
    assert not any("print" in s for s in sentences)


def test_markdown_stripped():
    buf = SentenceBuffer()
    sentences = buf.add("This is **bold** and *italic*. ")
    assert sentences == ["This is bold and italic."]


def test_url_skipped():
    buf = SentenceBuffer()
    sentences = buf.add("Check https://example.com for details. ")
    assert sentences == ["Check  for details."]


def test_link_text_kept():
    buf = SentenceBuffer()
    sentences = buf.add("See [this article](https://example.com) for more. ")
    assert sentences == ["See this article for more."]


def test_image_reference_skipped():
    buf = SentenceBuffer()
    sentences = buf.add("Here: ![screenshot](image.png). And more. ")
    assert sentences == ["Here: .", "And more."]


def test_exclamation_boundary():
    buf = SentenceBuffer()
    sentences = buf.add("Wow! That's great. ")
    assert sentences == ["Wow!", "That's great."]


def test_empty_input():
    buf = SentenceBuffer()
    assert buf.add("") == []


def test_clear_resets():
    buf = SentenceBuffer()
    buf.add("Partial text")
    buf.clear()
    assert buf.flush() == []
```

**Step 2: Run test to verify it fails**

```bash
cd ~/.local/share/claude-ask && .venv/bin/python -m pytest test_sentence_buffer.py -v
```

Expected: FAIL — `sentence_buffer` module not found.

**Step 3: Write the implementation**

Create `.local/share/claude-ask/sentence_buffer.py`:

```python
"""Sentence buffer: accumulates streaming text, yields complete sentences.

Strips markdown formatting, skips code blocks and bare URLs,
and detects sentence boundaries for TTS consumption.
"""

import re

# Sentence boundary: period, question mark, or exclamation followed by
# whitespace (or end-of-string after flush).  We split on these but keep
# the punctuation attached to the sentence.
_SENTENCE_RE = re.compile(r'(?<=[.!?])\s+|(?<=\n)')

# Markdown patterns to strip
_BOLD_ITALIC = re.compile(r'\*{1,3}(.*?)\*{1,3}')
_INLINE_CODE = re.compile(r'`([^`]+)`')
_MD_IMAGE = re.compile(r'!\[.*?\]\(.*?\)')
_MD_LINK = re.compile(r'\[([^\]]+)\]\([^\)]+\)')
_BARE_URL = re.compile(r'https?://\S+')
_HEADING = re.compile(r'^#{1,6}\s+', re.MULTILINE)

# Fenced code block detection
_CODE_FENCE_OPEN = re.compile(r'^```', re.MULTILINE)
_CODE_FENCE_CLOSE = re.compile(r'^```\s*$', re.MULTILINE)


class SentenceBuffer:
    """Accumulate streaming text chunks and yield complete sentences."""

    def __init__(self):
        self._buffer = ""
        self._in_code_block = False

    def add(self, text: str) -> list[str]:
        """Add a text chunk. Returns list of complete sentences (may be empty)."""
        self._buffer += text
        return self._extract_sentences()

    def flush(self) -> list[str]:
        """Return any remaining buffered text as a sentence."""
        if not self._buffer.strip():
            self._buffer = ""
            return []
        text = self._buffer.strip()
        self._buffer = ""
        text = self._clean(text)
        return [text] if text.strip() else []

    def clear(self):
        """Discard all buffered text."""
        self._buffer = ""
        self._in_code_block = False

    def _extract_sentences(self) -> list[str]:
        """Split buffer on sentence boundaries, keep remainder buffered."""
        sentences = []

        # Process code fences line by line
        lines = self._buffer.split('\n')
        clean_lines = []
        i = 0
        while i < len(lines):
            line = lines[i]
            if _CODE_FENCE_OPEN.match(line.strip()):
                if self._in_code_block:
                    # Closing fence
                    self._in_code_block = False
                else:
                    # Opening fence
                    self._in_code_block = True
                i += 1
                continue
            if self._in_code_block:
                i += 1
                continue
            clean_lines.append(line)
            i += 1

        # Rejoin non-code text
        text = '\n'.join(clean_lines)

        # Split on sentence boundaries
        parts = _SENTENCE_RE.split(text)
        if not parts:
            self._buffer = ""
            return []

        # Last part might be incomplete — keep it buffered
        self._buffer = parts[-1] if parts else ""
        complete = parts[:-1]

        for part in complete:
            cleaned = self._clean(part.strip())
            if cleaned.strip():
                sentences.append(cleaned)

        return sentences

    def _clean(self, text: str) -> str:
        """Strip markdown formatting, URLs, image refs from text."""
        text = _MD_IMAGE.sub('', text)
        text = _MD_LINK.sub(r'\1', text)
        text = _BARE_URL.sub('', text)
        text = _BOLD_ITALIC.sub(r'\1', text)
        text = _INLINE_CODE.sub(r'\1', text)
        text = _HEADING.sub('', text)
        # Collapse multiple spaces
        text = re.sub(r'  +', ' ', text)
        return text.strip()
```

**Step 4: Run tests to verify they pass**

```bash
cd ~/.local/share/claude-ask && .venv/bin/python -m pytest test_sentence_buffer.py -v
```

Expected: All tests PASS. Some tests may need minor adjustments based on exact boundary behavior — iterate until green.

**Step 5: Commit**

```bash
cd ~/dotfiles
git add .local/share/claude-ask/sentence_buffer.py .local/share/claude-ask/test_sentence_buffer.py
git commit -m "feat(claude-ask): add sentence buffer for TTS streaming"
```

---

### Task 3: Create the Waybar State Writer Module

**Files:**
- Create: `.local/share/claude-ask/waybar_state.py`

**Step 1: Write the module**

Create `.local/share/claude-ask/waybar_state.py`:

```python
"""Waybar state file writer for claude-ask.

Writes a JSON state file that the Waybar module script reads.
Signals Waybar to refresh immediately after writing.
"""

import json
import logging
import subprocess
from pathlib import Path

log = logging.getLogger("claude-ask")

STATE_FILE = Path.home() / ".local" / "state" / "claude-ask" / "waybar.json"
WAYBAR_SIGNAL = 12  # SIGRTMIN+12


class WaybarState:
    """Manage the Waybar state file."""

    def __init__(self):
        self._state = {
            "status": "idle",
            "speak_enabled": False,
            "usage": {
                "session_cost": "$0.00",
                "last_query_cost": "$0.00",
                "total_tokens": 0,
            },
        }
        STATE_FILE.parent.mkdir(parents=True, exist_ok=True)
        self._write()

    @property
    def speak_enabled(self) -> bool:
        return self._state["speak_enabled"]

    @speak_enabled.setter
    def speak_enabled(self, val: bool):
        self._state["speak_enabled"] = val
        self._write()

    @property
    def status(self) -> str:
        return self._state["status"]

    def set_status(self, status: str):
        """Set status to idle, thinking, or speaking. Writes + signals."""
        self._state["status"] = status
        self._write()

    def update_usage(self, query_cost: float, total_tokens: int):
        """Update usage after an API call. Accumulates session cost."""
        current = float(self._state["usage"]["session_cost"].replace("$", ""))
        current += query_cost
        self._state["usage"]["session_cost"] = f"${current:.2f}"
        self._state["usage"]["last_query_cost"] = f"${query_cost:.4f}"
        self._state["usage"]["total_tokens"] = total_tokens
        self._write()

    def reload_speak_enabled(self):
        """Re-read the state file to pick up toggle changes from external scripts."""
        try:
            if STATE_FILE.exists():
                data = json.loads(STATE_FILE.read_text())
                self._state["speak_enabled"] = data.get("speak_enabled", False)
        except (json.JSONDecodeError, OSError):
            pass

    def _write(self):
        """Write state to disk and signal Waybar."""
        try:
            STATE_FILE.write_text(json.dumps(self._state, indent=2))
        except OSError:
            log.exception("Failed to write waybar state")
        self._signal_waybar()

    def _signal_waybar(self):
        """Send SIGRTMIN+12 to waybar for immediate refresh."""
        try:
            subprocess.Popen(
                ["pkill", f"-SIGRTMIN+{WAYBAR_SIGNAL}", "waybar"],
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
            )
        except OSError:
            pass
```

**Step 2: Verify it creates the state file**

```bash
cd ~/.local/share/claude-ask && .venv/bin/python -c "
from waybar_state import WaybarState
ws = WaybarState()
print('Created:', ws.status, ws.speak_enabled)
ws.set_status('thinking')
print('Updated:', ws.status)
" && cat ~/.local/state/claude-ask/waybar.json
```

Expected: JSON file with `status: "thinking"`, `speak_enabled: false`.

**Step 3: Commit**

```bash
cd ~/dotfiles
git add .local/share/claude-ask/waybar_state.py
git commit -m "feat(claude-ask): add Waybar state writer module"
```

---

### Task 4: Create TTS Pipeline Module

**Files:**
- Create: `.local/share/claude-ask/tts.py`

**Step 1: Write the TTS pipeline**

Create `.local/share/claude-ask/tts.py`:

```python
"""TTS pipeline: Kokoro synthesis + sounddevice playback.

Manages two threads: one for synthesis (sentence → audio), one for
playback (audio → speakers). Supports interruption and lazy model loading.
"""

import logging
import queue
import threading

log = logging.getLogger("claude-ask")

# Sentinel to signal threads to stop
_STOP = object()
_DONE = object()


class TTSPipeline:
    """Kokoro TTS synthesis and audio playback pipeline."""

    def __init__(self, model="af_heart", speed=1.0, lang="a"):
        self._model_name = model
        self._speed = speed
        self._lang = lang
        self._pipeline = None  # Lazy-loaded KPipeline
        self._sentence_q: queue.Queue = queue.Queue()
        self._audio_q: queue.Queue = queue.Queue()
        self._synth_thread: threading.Thread | None = None
        self._play_thread: threading.Thread | None = None
        self._running = False
        self._lock = threading.Lock()

    def _ensure_loaded(self):
        """Lazy-load the Kokoro model on first use."""
        if self._pipeline is not None:
            return
        log.info("Loading Kokoro TTS model (voice=%s, lang=%s)...", self._model_name, self._lang)
        try:
            from kokoro import KPipeline
            self._pipeline = KPipeline(lang_code=self._lang)
            log.info("Kokoro TTS loaded successfully")
        except Exception:
            log.exception("Failed to load Kokoro TTS")
            raise

    def update_config(self, model: str, speed: float, lang: str):
        """Update voice settings. Takes effect on next sentence."""
        self._model_name = model
        self._speed = speed
        if lang != self._lang:
            self._lang = lang
            self._pipeline = None  # Force reload for new language

    def start(self):
        """Start the synthesis and playback threads."""
        with self._lock:
            if self._running:
                return
            self._running = True
            self._sentence_q = queue.Queue()
            self._audio_q = queue.Queue()
            self._synth_thread = threading.Thread(target=self._synth_loop, daemon=True)
            self._play_thread = threading.Thread(target=self._play_loop, daemon=True)
            self._synth_thread.start()
            self._play_thread.start()

    def stop(self):
        """Stop threads and clear queues. Kills current playback."""
        with self._lock:
            if not self._running:
                return
            self._running = False

        # Clear queues and send stop sentinels
        self._drain_queue(self._sentence_q)
        self._drain_queue(self._audio_q)
        self._sentence_q.put(_STOP)
        self._audio_q.put(_STOP)

        # Kill current audio playback
        try:
            import sounddevice as sd
            sd.stop()
        except Exception:
            pass

        # Wait for threads
        if self._synth_thread:
            self._synth_thread.join(timeout=2)
        if self._play_thread:
            self._play_thread.join(timeout=2)

    def speak(self, sentence: str):
        """Queue a sentence for synthesis and playback."""
        if self._running:
            self._sentence_q.put(sentence)

    def finish(self):
        """Signal that no more sentences are coming. Blocks until playback done."""
        if self._running:
            self._sentence_q.put(_DONE)

    def wait_done(self, timeout=120):
        """Wait for playback to complete. Returns True if done, False if timeout."""
        if self._play_thread and self._play_thread.is_alive():
            self._play_thread.join(timeout=timeout)
            return not self._play_thread.is_alive()
        return True

    def _synth_loop(self):
        """Thread: pull sentences, synthesize with Kokoro, push audio."""
        try:
            self._ensure_loaded()
        except Exception:
            self._audio_q.put(_STOP)
            return

        while True:
            item = self._sentence_q.get()
            if item is _STOP:
                self._audio_q.put(_STOP)
                break
            if item is _DONE:
                self._audio_q.put(_DONE)
                break
            try:
                # Kokoro returns a generator; we take the first segment
                for _gs, _ps, audio in self._pipeline(
                    item, voice=self._model_name, speed=self._speed
                ):
                    if not self._running:
                        break
                    self._audio_q.put(audio)
            except Exception:
                log.exception("TTS synthesis error for: %s", item[:50])

    def _play_loop(self):
        """Thread: pull audio arrays, play through speakers."""
        import sounddevice as sd

        while True:
            item = self._audio_q.get()
            if item is _STOP or item is _DONE:
                break
            if not self._running:
                break
            try:
                sd.play(item, samplerate=24000)
                sd.wait()
            except Exception:
                log.exception("Audio playback error")

    @staticmethod
    def _drain_queue(q):
        """Empty a queue without blocking."""
        while True:
            try:
                q.get_nowait()
            except queue.Empty:
                break
```

**Step 2: Verify Kokoro loads and speaks**

```bash
cd ~/.local/share/claude-ask && .venv/bin/python -c "
from tts import TTSPipeline
p = TTSPipeline(model='af_heart', speed=1.0, lang='a')
p.start()
p.speak('Hello! This is Claude speaking to you.')
p.speak('How does this sound?')
p.finish()
p.wait_done(timeout=30)
p.stop()
print('Done')
"
```

Expected: Two sentences spoken aloud through speakers. If speakers/audio not set up, verify no crash and audio arrays are generated.

**Step 3: Commit**

```bash
cd ~/dotfiles
git add .local/share/claude-ask/tts.py
git commit -m "feat(claude-ask): add Kokoro TTS pipeline module"
```

---

### Task 5: Create Waybar Scripts

**Files:**
- Create: `.local/bin/claude-ask-status`
- Create: `.local/bin/claude-ask-toggle-speak`

**Step 1: Create the status script**

Create `.local/bin/claude-ask-status` (make executable):

```bash
#!/bin/bash
# Waybar module for claude-ask status

STATE_FILE="$HOME/.local/state/claude-ask/waybar.json"

if [ ! -f "$STATE_FILE" ]; then
    echo '{"text": "󰍩", "class": "idle", "tooltip": "Claude Ask (not running)"}'
    exit 0
fi

# Parse JSON with jq-less approach (python one-liner since it's always available)
eval "$(python3 -c "
import json, sys
d = json.load(open('$STATE_FILE'))
status = d.get('status', 'idle')
speak = d.get('speak_enabled', False)
usage = d.get('usage', {})
session = usage.get('session_cost', '\$0.00')
last = usage.get('last_query_cost', '\$0.00')
tokens = usage.get('total_tokens', 0)

# Icons (Nerd Font: nf-md-*)
if status == 'speaking':
    icon = '󰔊'  # volume_high
elif status == 'thinking':
    icon = '󰍩'  # message_text_outline
else:
    # idle
    icon = '󰍩'
    if speak:
        icon += ' 󰕾'  # + volume_high
    else:
        icon += ' 󰖁'  # + volume_off

# CSS class
css = status
if status == 'idle' and speak:
    css = 'idle speak-on'

# Tooltip
tip = f'Session: {session} | Last: {last} | Tokens: {tokens:,}'
if speak:
    tip += ' | Voice: ON'
else:
    tip += ' | Voice: OFF'

# Escape for JSON
tip = tip.replace('\"', '\\\\\"')
print(f'TEXT={icon!r}')
print(f'CLASS={css!r}')
print(f'TIP={tip!r}')
")"

echo "{\"text\":\"${TEXT}\",\"tooltip\":\"${TIP}\",\"class\":\"${CLASS}\"}"
```

**Step 2: Create the toggle script**

Create `.local/bin/claude-ask-toggle-speak` (make executable):

```bash
#!/bin/bash
# Toggle claude-ask speak mode on/off

STATE_FILE="$HOME/.local/state/claude-ask/waybar.json"
CONFIG_FILE="$HOME/.config/claude-ask/config.toml"

if [ ! -f "$STATE_FILE" ]; then
    echo "claude-ask not running" >&2
    exit 1
fi

# Toggle speak_enabled in state file
python3 -c "
import json
path = '$STATE_FILE'
with open(path) as f:
    d = json.load(f)
d['speak_enabled'] = not d.get('speak_enabled', False)
with open(path, 'w') as f:
    json.dump(d, f, indent=2)
state = 'ON' if d['speak_enabled'] else 'OFF'
print(f'Voice: {state}')
"

# Also persist to config file
python3 -c "
import re
path = '$CONFIG_FILE'
try:
    with open(path) as f:
        content = f.read()
    # Toggle the enabled line
    if 'enabled = true' in content:
        content = content.replace('enabled = true', 'enabled = false', 1)
    else:
        content = content.replace('enabled = false', 'enabled = true', 1)
    with open(path, 'w') as f:
        f.write(content)
except FileNotFoundError:
    pass
"

# Signal Waybar to refresh
pkill -SIGRTMIN+12 waybar 2>/dev/null
```

**Step 3: Make scripts executable and stow**

```bash
chmod +x ~/dotfiles/.local/bin/claude-ask-status ~/dotfiles/.local/bin/claude-ask-toggle-speak
cd ~/dotfiles && stow .
```

**Step 4: Test the scripts**

```bash
# Create a fake state file
mkdir -p ~/.local/state/claude-ask
echo '{"status":"idle","speak_enabled":false,"usage":{"session_cost":"$0.42","last_query_cost":"$0.03","total_tokens":12450}}' > ~/.local/state/claude-ask/waybar.json

# Test status script
claude-ask-status

# Test toggle
claude-ask-toggle-speak
claude-ask-status
```

Expected: First call shows idle with voice OFF icon. After toggle, shows voice ON.

**Step 5: Commit**

```bash
cd ~/dotfiles
git add .local/bin/claude-ask-status .local/bin/claude-ask-toggle-speak
git commit -m "feat(claude-ask): add Waybar status and toggle scripts"
```

---

### Task 6: Add Waybar Config and CSS

**Files:**
- Modify: `.config/waybar/config` (line 7 — modules-right array)
- Modify: `.config/waybar/style.css` (append at end)

**Step 1: Add module to Waybar config**

In `.config/waybar/config`, add `"custom/claude-ask"` to the `modules-right` array (insert before `"custom/notification"`):

```json
"modules-right": ["custom/claude-ask", "custom/notification", "tray", "custom/sysinfo", "network", "battery", "custom/system-freshness", "custom/gcal", "clock", "custom/swaync-open"],
```

Add the module definition (after the `custom/voice-typing` block, around line 183):

```json
"custom/claude-ask": {
    "exec": "claude-ask-status",
    "return-type": "json",
    "interval": 2,
    "on-click": "claude-ask-toggle-speak",
    "on-click-right": "claude-ask-open-emacs",
    "signal": 12,
    "tooltip": true
},
```

**Step 2: Add CSS styles**

Append to `.config/waybar/style.css`:

```css
/* Claude Ask module */
#custom-claude-ask {
    color: #b4befe;
}

#custom-claude-ask.thinking {
    color: #b4befe;
    animation-name: pulse;
    animation-duration: 1.5s;
    animation-timing-function: ease-in-out;
    animation-iteration-count: infinite;
}

#custom-claude-ask.speaking {
    color: #a6e3a1;
    animation-name: pulse;
    animation-duration: 1s;
    animation-timing-function: ease-in-out;
    animation-iteration-count: infinite;
}

#custom-claude-ask.speak-on {
    color: #cdd6f4;
}
```

**Step 3: Reload Waybar**

```bash
pkill waybar && hyprctl dispatch exec waybar
```

**Step 4: Verify module appears**

Check Waybar — the Claude icon should appear in the right modules. Click it to toggle speak state.

**Step 5: Commit**

```bash
cd ~/dotfiles
git add .config/waybar/config .config/waybar/style.css
git commit -m "feat(claude-ask): add Waybar module config and styling"
```

---

### Task 7: Integrate TTS into Daemon

This is the main integration task. Modifies `daemon.py` to wire together the sentence buffer, TTS pipeline, Waybar state, and config loading.

**Files:**
- Modify: `.local/share/claude-ask/daemon.py`

**Step 1: Add imports and config loading**

At the top of `daemon.py`, after line 17 (`from pathlib import Path`), add:

```python
import tomllib

from sentence_buffer import SentenceBuffer
from tts import TTSPipeline
from waybar_state import WaybarState
```

After `USAGE_LOG` (line 31), add:

```python
WAYBAR_STATE_FILE = Path.home() / ".local" / "state" / "claude-ask" / "waybar.json"
CONFIG_FILE = Path.home() / ".config" / "claude-ask" / "config.toml"
```

Add a config loading function after the `get_socket_path` function (after line 64):

```python
def load_voice_config():
    """Load voice configuration from config.toml."""
    defaults = {
        "enabled": False,
        "model": "af_heart",
        "speed": 1.0,
        "lang": "a",
        "filter": {"skip_code_blocks": True, "skip_urls": True},
    }
    try:
        with open(CONFIG_FILE, "rb") as f:
            config = tomllib.load(f)
        voice = config.get("voice", {})
        return {
            "enabled": voice.get("enabled", defaults["enabled"]),
            "model": voice.get("model", defaults["model"]),
            "speed": voice.get("speed", defaults["speed"]),
            "lang": voice.get("lang", defaults["lang"]),
            "filter": voice.get("filter", defaults["filter"]),
        }
    except (FileNotFoundError, tomllib.TOMLDecodeError):
        log.warning("Could not load voice config, using defaults")
        return defaults
```

**Step 2: Update Daemon.__init__**

Replace `Daemon.__init__` (lines 161-165) with:

```python
def __init__(self, api_key):
    self.api_key = api_key
    self.store = ConversationStore()
    self.client = anthropic.Anthropic(api_key=api_key)
    self.tools = self._load_tools()
    self.waybar = WaybarState()
    self._voice_config = load_voice_config()
    self.waybar.speak_enabled = self._voice_config["enabled"]
    self.tts = TTSPipeline(
        model=self._voice_config["model"],
        speed=self._voice_config["speed"],
        lang=self._voice_config["lang"],
    )
    self._session_tokens = 0
```

**Step 3: Modify _stream_response to support TTS**

Replace `_stream_response` (lines 376-411) with:

```python
def _stream_response(self, messages, tag):
    """Call Claude API with streaming, update notifications, return response.

    This is a synchronous blocking method meant to be called via
    run_in_executor from async code.

    Returns the full Message object from the API.
    """
    api_kwargs = {
        "model": MODEL,
        "max_tokens": MAX_TOKENS,
        "system": SYSTEM_PROMPT,
        "messages": messages,
    }
    if self.tools:
        api_kwargs["tools"] = self.tools

    accumulated_text = ""
    last_notify_time = 0.0
    sentence_buf = SentenceBuffer()

    # Check if speak is enabled (re-read in case toggled externally)
    self.waybar.reload_speak_enabled()
    speak_on = self.waybar.speak_enabled

    if speak_on:
        self.tts.start()
        self.waybar.set_status("speaking")
        # Show speaking notification with stop action
        self._notify_speaking(tag)
    else:
        self.waybar.set_status("thinking")

    self._notify(tag, "Thinking...")

    with self.client.messages.stream(**api_kwargs) as stream:
        for event in stream:
            if event.type == "content_block_delta":
                if event.delta.type == "text_delta":
                    accumulated_text += event.delta.text

                    # TTS: feed text to sentence buffer
                    if speak_on:
                        sentences = sentence_buf.add(event.delta.text)
                        for sentence in sentences:
                            self.tts.speak(sentence)

                    # Notification: only stream if speak is OFF
                    if not speak_on:
                        now = time.monotonic()
                        if now - last_notify_time >= NOTIFY_DEBOUNCE_SECS:
                            self._notify(tag, accumulated_text)
                            last_notify_time = now

        response = stream.get_final_message()

    # Flush remaining text in sentence buffer
    if speak_on:
        for sentence in sentence_buf.flush():
            self.tts.speak(sentence)
        self.tts.finish()

    return response
```

**Step 4: Add speaking notification method**

After `_notify` (line 249), add:

```python
def _notify_speaking(self, tag):
    """Show a persistent notification while speaking with a Stop action."""
    def _run():
        try:
            result = subprocess.run(
                [
                    "notify-send", "-t", "0",
                    "-h", f"string:x-canonical-private-synchronous:{tag}-speaking",
                    "-a", "Claude",
                    "-A", "stop=Stop",
                    "--wait",
                    "Claude", "Speaking...",
                ],
                capture_output=True,
                text=True,
            )
            if result.stdout.strip() == "stop":
                self.tts.stop()
                self.waybar.set_status("idle")
                # Dismiss the speaking notification
                subprocess.Popen(
                    ["notify-send", "-t", "1",
                     "-h", f"string:x-canonical-private-synchronous:{tag}-speaking",
                     "-a", "Claude", "Claude", "Stopped"],
                    stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                )
        except Exception:
            log.exception("Error in speaking notification")
    thread = threading.Thread(target=_run, daemon=True)
    thread.start()

def _dismiss_speaking_notify(self, tag):
    """Dismiss the speaking notification."""
    subprocess.Popen(
        ["notify-send", "-t", "1",
         "-h", f"string:x-canonical-private-synchronous:{tag}-speaking",
         "-a", "Claude", "Claude", ""],
        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
    )
```

**Step 5: Update _complete_conversation for TTS lifecycle**

In `_complete_conversation` (the section after the while loop, around line 559), replace the final notification section. After `self._save_last_state(conv["id"])` add TTS wait and cleanup:

```python
# Wait for TTS to finish speaking if active
if self.waybar.status == "speaking":
    self.tts.wait_done(timeout=120)
    self.tts.stop()
    self._dismiss_speaking_notify(tag)

self.waybar.set_status("idle")
```

**Step 6: Update _log_usage to feed Waybar state**

After the existing `_log_usage` call in `_complete_conversation`, add:

```python
# Update Waybar usage display
prices = TOKEN_PRICES.get(response.model, TOKEN_PRICES["claude-sonnet-4-6"])
query_cost = (response.usage.input_tokens * prices["input"] +
              response.usage.output_tokens * prices["output"]) / 1_000_000
self._session_tokens += response.usage.input_tokens + response.usage.output_tokens
self.waybar.update_usage(query_cost, self._session_tokens)
```

**Step 7: Remove _notify_usage call from _complete_conversation**

Delete or comment out the `self._notify_usage(tag)` call at line 577. Usage info now lives in the Waybar tooltip.

**Step 8: Test the integration**

```bash
# Restart the daemon
systemctl --user restart claude-ask

# Check logs for errors
journalctl --user -u claude-ask -f &

# Send a test query
echo '{"text": "Say hello in one sentence"}' | socat - UNIX-CONNECT:$XDG_RUNTIME_DIR/claude-ask.sock

# Toggle speak on
claude-ask-toggle-speak

# Send another query (should be spoken)
echo '{"text": "Tell me a short joke"}' | socat - UNIX-CONNECT:$XDG_RUNTIME_DIR/claude-ask.sock
```

Expected: First query responds via notification only. After toggling speak on and sending second query, response is spoken aloud AND shown in final notification.

**Step 9: Commit**

```bash
cd ~/dotfiles
git add .local/share/claude-ask/daemon.py
git commit -m "feat(claude-ask): integrate TTS and Waybar state into daemon"
```

---

### Task 8: End-to-End Testing

**Step 1: Full cycle test — speak OFF**

```bash
# Ensure speak is off
python3 -c "
import json
f = '$HOME/.local/state/claude-ask/waybar.json'
d = json.load(open(f)); d['speak_enabled'] = False
json.dump(d, open(f, 'w'), indent=2)
"

# Send query
claude-ask
# Type: "What is 2+2?" and press Enter
```

Verify:
- [ ] Waybar shows thinking animation
- [ ] Streaming notification appears
- [ ] Final notification shows with Reply/Mic/Emacs actions
- [ ] Waybar returns to idle
- [ ] Waybar tooltip shows usage stats
- [ ] No audio played

**Step 2: Full cycle test — speak ON**

```bash
# Toggle speak on
claude-ask-toggle-speak
```

Verify:
- [ ] Waybar icon changes to show speak ON indicator

```bash
# Send query
claude-ask
# Type: "Tell me something interesting in two sentences" and press Enter
```

Verify:
- [ ] Waybar shows speaking animation (green pulse)
- [ ] "Speaking..." notification appears with Stop button
- [ ] Audio plays through speakers (two sentences)
- [ ] Streaming text notification is NOT shown
- [ ] Final notification appears after speaking completes
- [ ] Speaking notification dismissed after audio done
- [ ] Waybar returns to idle

**Step 3: Interrupt test**

```bash
# Send a longer query
claude-ask
# Type: "Explain quantum computing in detail" and press Enter
# Click "Stop" on the speaking notification while audio is playing
```

Verify:
- [ ] Audio stops immediately
- [ ] Waybar returns to idle
- [ ] "Stopped" notification briefly appears

**Step 4: Tool call test with speak**

```bash
claude-ask
# Type: "What's the current CPU temperature?" and press Enter
```

Verify:
- [ ] Tool call executes (shell tool)
- [ ] Claude's text response (not tool call JSON) is spoken
- [ ] Tool result text is NOT spoken

**Step 5: Toggle persistence test**

```bash
# Toggle speak off
claude-ask-toggle-speak
# Restart daemon
systemctl --user restart claude-ask
# Check state
cat ~/.local/state/claude-ask/waybar.json | python3 -m json.tool
```

Verify: `speak_enabled` is false after restart (read from config.toml).

---

### Summary of All Files

**Created:**
| File | Purpose |
|---|---|
| `.config/claude-ask/config.toml` | Voice configuration |
| `.local/share/claude-ask/sentence_buffer.py` | Sentence detection for TTS |
| `.local/share/claude-ask/test_sentence_buffer.py` | Tests for sentence buffer |
| `.local/share/claude-ask/waybar_state.py` | Waybar JSON state writer |
| `.local/share/claude-ask/tts.py` | Kokoro TTS pipeline |
| `.local/bin/claude-ask-status` | Waybar module script |
| `.local/bin/claude-ask-toggle-speak` | Speak toggle script |

**Modified:**
| File | Changes |
|---|---|
| `.local/share/claude-ask/daemon.py` | TTS integration, config loading, state management |
| `.local/share/claude-ask/requirements.txt` | Added kokoro, soundfile, sounddevice |
| `.config/waybar/config` | Added custom/claude-ask module |
| `.config/waybar/style.css` | Added claude-ask styling |
