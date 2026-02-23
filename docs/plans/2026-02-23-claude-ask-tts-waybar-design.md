# Claude-Ask TTS & Waybar Module Design

## Overview

Add text-to-speech output and a Waybar status module to claude-ask. When speak mode is toggled on, Claude's streaming responses are spoken aloud sentence-by-sentence using Kokoro TTS. A new Waybar module displays status, usage stats, and provides a speak toggle.

## Architecture

**Approach: TTS integrated into daemon.py.** Kokoro loads inside the existing daemon process. Sentence detection and audio synthesis run in a thread pool to avoid blocking the async event loop. A state file bridges the daemon and Waybar module.

### Data Flow

```
input.py → daemon.py (socket)
                │
                ├── Claude API stream
                │       │
                │       ▼
                │   Sentence buffer (detect . ? ! \n boundaries)
                │       │
                │       ├── [speak OFF] → notification stream (existing)
                │       │
                │       └── [speak ON] → TTS pipeline (thread pool)
                │                              │
                │                              ├── Kokoro synthesis (thread 1)
                │                              └── sounddevice playback (thread 2)
                │
                └── State file → ~/.local/state/claude-ask/waybar.json
                                        │
                                        ▼
                                 Waybar module (polls every 2s + signal refresh)
```

### State File

Path: `~/.local/state/claude-ask/waybar.json`

```json
{
  "status": "idle",
  "speak_enabled": true,
  "usage": {
    "session_cost": "$0.42",
    "last_query_cost": "$0.03",
    "total_tokens": 12450
  }
}
```

States: `idle` | `thinking` | `speaking`

Transitions:
- `idle → thinking`: query received
- `thinking → speaking`: first sentence ready (speak ON)
- `thinking → idle`: response complete (speak OFF)
- `speaking → idle`: all sentences played OR interrupted

Each transition writes the state file and signals Waybar via `pkill -SIGRTMIN+12 waybar`.

## Waybar Module

### Config

```json
"custom/claude-ask": {
    "exec": "claude-ask-status",
    "return-type": "json",
    "interval": 2,
    "on-click": "claude-ask-toggle-speak",
    "on-click-right": "claude-ask-open-emacs",
    "signal": 12
}
```

### Display States

| State | Icon | CSS Class |
|---|---|---|
| Idle, speak OFF | `nf-md-message_text_outline` | `idle` |
| Idle, speak ON | `nf-md-message_text_outline` + `nf-md-volume_high` | `idle speak-on` |
| Thinking | `nf-md-message_text_outline` (pulsing) | `thinking` |
| Speaking | `nf-md-volume_high` (pulsing green) | `speaking` |

### Tooltip

Shows usage stats: session cost, last query cost, total tokens.

### Click Actions

- **Left click**: Toggle speak on/off (updates config + state file)
- **Right click**: Open last conversation in Emacs

### CSS

```css
#custom-claude-ask { color: #b4befe; }
#custom-claude-ask.thinking { animation: pulse 1.5s ease-in-out infinite; }
#custom-claude-ask.speaking { color: #a6e3a1; animation: pulse 1s ease-in-out infinite; }
#custom-claude-ask.speak-on { color: #cdd6f4; }
```

## TTS Integration

### Engine

Kokoro TTS (PyTorch, `kokoro>=0.9.4`). 82M parameter model, ~300MB. Loaded lazily on first speak request.

### Sentence Detection

Text from `content_block_delta` events (type `text_delta` only) accumulates in a buffer. Regex splits on sentence boundaries (`. `, `? `, `! `, `\n`) while keeping partial sentences buffered until the next boundary.

### Content Filtering

| Content | TTS Behavior |
|---|---|
| Plain text | Speak (strip markdown first) |
| Tool call blocks (`tool_use`) | Skip |
| Text around tool calls | Speak |
| Tool results | Skip (wait for Claude's next text) |
| Code blocks (fenced) | Skip entirely |
| Image references | Skip markdown syntax |
| Bare URLs | Skip |
| Link text | Speak if descriptive |

### Audio Pipeline

Two-thread pipeline with async queues:

1. **Synthesis thread**: Pulls sentences from queue, runs `KPipeline(sentence, voice=..., speed=...)`, pushes audio arrays to playback queue
2. **Playback thread**: Pulls audio from queue, calls `sounddevice.play(audio, 24000)`, waits for completion

Synthesis runs ahead of playback for zero-gap between sentences.

### Interrupt Handling

On stop signal (Waybar click, notification action, or new query):
1. Clear sentence queue
2. Clear audio queue
3. `sounddevice.stop()` to kill current playback
4. Update state to `idle`

## Notification Changes

### When Speak is ON

- **Streaming notifications suppressed** (voice IS the stream)
- **Speaking notification shown**: persistent notification with "Stop" action
- **Final notification still shown**: Reply / Mic / Open in Emacs actions (no usage info — that's in Waybar now)

### When Speak is OFF

- Existing notification behavior unchanged (streaming text updates)
- Usage info still moves to Waybar tooltip

## Configuration

Path: `~/.config/claude-ask/config.toml`

```toml
[voice]
enabled = false          # persisted toggle state
model = "af_heart"       # Kokoro voice name
speed = 1.0              # 0.5 - 2.0
lang = "a"               # a=US, b=UK, j=JP, z=ZH, e=ES, f=FR

[voice.filter]
skip_code_blocks = true
skip_urls = true
```

Toggle script updates `enabled` field. Daemon watches or re-reads on each query.

## Dependencies

### Python (requirements.txt additions)

```
kokoro>=0.9.4
soundfile
sounddevice
tomli
```

### System

```
espeak-ng    # pacman -S espeak-ng
```

## New Files

| File | Purpose |
|---|---|
| `~/.local/bin/claude-ask-status` | Waybar module script |
| `~/.local/bin/claude-ask-toggle-speak` | Toggle speak on/off |
| `~/.config/claude-ask/config.toml` | Voice configuration |

## Modified Files

| File | Changes |
|---|---|
| `.local/share/claude-ask/daemon.py` | TTS pipeline, sentence buffer, state file, Kokoro loading |
| `.config/waybar/config` | Add `custom/claude-ask` module |
| `.config/waybar/style.css` | Add claude-ask styling |
| `.local/share/claude-ask/requirements.txt` | Add dependencies |
