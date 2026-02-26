# Claude Pipeline Consolidation Design

## Problem

The message pipeline from input to Claude API response has conversation continuation logic scattered across three places:

1. **claude-voice** `_read_last_conversation()` — checks `last.json` for <60s recency on wake word path
2. **input.py** `read_last_state()` — same check, reimplemented, for the TUI path
3. **claude-ask notification mic button** — passes explicit `conv_id` through claude-voice

Each new entry point (HID button, wake word, TUI, notification buttons) has to independently "hack in" the conversation check. The HID button path was missing it entirely, so it never continued conversations.

Beyond conversation resolution, the query pipeline (API call, streaming, tool execution, TTS, notifications) lives entirely inside claude-ask's `Daemon` class — a god object that mixes daemon concerns (socket server, event loop) with business logic (API calls, conversation management).

## Design

### Architecture: Two Daemons + Helper Functions

**Daemons** handle only event detection and dispatch:
- **claude-hid** — listens for HID button events, sends to claude-voice's control socket
- **claude-voice** — listens for wake word, runs control socket, dispatches query threads

**Helper functions** in `.local/share/claude-ask/` do all the work. They are self-contained, composable, and have no daemon/socket/event-loop dependencies.

**claude-ask daemon is removed.** Its socket server, Daemon class, and event loop are eliminated. Claude-voice becomes the single long-lived process that runs queries.

### Message Flow

All paths converge on one function: `send_query()`.

```
claude-hid ──button press──> claude-voice control socket ──> capture thread ──> query thread
wake word ─────────────────> (internal detection) ─────────> capture thread ──> query thread
TUI (input.py) ──text──────> claude-voice control socket ──> query thread (no capture needed)
notification mic button ───> claude-voice control socket ──> capture thread ──> query thread
notification reply button ─> TUI (--reply flag) ───────────> claude-voice control socket ──> query thread
cancel script ─────────────> claude-voice control socket ──> sets threading.Event
```

### Tri-State Conversation Resolution

One sentinel, one function, three behaviors:

```python
NEW_CONVERSATION = object()

def send_query(text, conversation_id=None, cancel_event=None):
    if conversation_id is NEW_CONVERSATION:
        # Explicit fresh conversation (user chose "new" in TUI)
        conv = store.create()
    elif conversation_id is not None:
        # Explicit conversation ID (reply button, mic button, TUI selection)
        conv = store.get_or_create(conversation_id)
    else:
        # No opinion — auto-detect from last.json (<60s check)
        resolved = auto_resolve_conversation()
        conv = store.get_or_create(resolved) if resolved else store.create()
```

The <60s check lives in exactly one place: `auto_resolve_conversation()`, called only from the `else` branch of `send_query()`.

### Cancellation

A `threading.Event` flows through the entire query pipeline:

1. Claude-voice creates a `cancel_event` when spawning a query thread
2. Claude-voice's control socket handler holds a reference to it
3. When `{"action": "cancel"}` arrives, it calls `cancel_event.set()`
4. `send_query()` passes `cancel_event` to `stream_response()`, `speak()`, etc.
5. Each function checks `cancel_event.is_set()` at natural breakpoints (between stream chunks, between TTS sentences)

This is instant, non-hacky (`threading.Event` is the standard paradigm), and fully async — nothing blocks anything else.

### Concurrency Model

Each concern runs independently in its own thread:

| Concern | Thread | Blocks on |
|---|---|---|
| Wake word detection | Main thread | Audio read (expected) |
| Control socket (asyncio) | Background thread | I/O (non-blocking) |
| Speech capture | Spawned per capture | Audio read |
| Query (API + tools + notifications) | Spawned per query | Network I/O |
| TTS synthesis | Spawned per query | Kokoro model |
| TTS playback | Spawned per query | sounddevice |

Nothing blocks anything else. Wake word detection continues during queries. Multiple queries could theoretically run concurrently (though in practice one at a time).

### TTS Model Warmth

The Kokoro TTS model (~2-3s cold start) is kept warm as a module-level lazy singleton in the helpers module. Since claude-voice is long-lived, the model stays loaded in its process memory after first use.

### Helper Functions

All live in `.local/share/claude-ask/`. Claude-voice adds this to `sys.path`.

```
# Core query pipeline
send_query(text, conversation_id=None, cancel_event=None)
    # Resolves conversation (tri-state), calls Claude API, handles tool loop,
    # streams to notifications, speaks via TTS, saves conversation

stream_response(messages, cancel_event=None) -> (response, accumulated_text)
    # Single API call with streaming, updates notifications

run_tool(name, input) -> result
    # Execute a tool plugin by name

# Conversation management
auto_resolve_conversation() -> conv_id | None
    # Reads last.json, returns conv_id if <60s ago, else None

ConversationStore.get_or_create(conv_id) -> conv
ConversationStore.save(conv)

# Speech
transcribe(audio_bytes) -> str
    # Whisper STT

# Output
notify(tag, text) / notify_final(tag, text, conv_id)
speak(text, cancel_event=None) / stop_speaking()
    # TTS via module-level singleton
```

### Claude-Voice Control Socket Extensions

Claude-voice's existing control socket gains two new actions:

| Action | Payload | Behavior |
|---|---|---|
| `query` | `{"action": "query", "text": "...", "conversation_id": "..."\|"__new__"\|null}` | Spawn query thread with helpers |
| `cancel` | `{"action": "cancel"}` | Set cancel_event on running query thread |

Existing actions (`listen`, `mute`, `unmute`, `toggle-mute`, `get-mute`) unchanged.

### TUI Changes

`input.py` still reads `last.json` for the UI hint (pre-selecting a conversation in the picker). But it no longer calls Claude or manages conversations. On submit, it sends to claude-voice's control socket:

- User selected a conversation: `{"action": "query", "text": "...", "conversation_id": "abc123"}`
- User chose new (Tab to deselect): `{"action": "query", "text": "...", "conversation_id": "__new__"}`
- User didn't interact with picker: `{"action": "query", "text": "..."}`  (conv_id absent = auto-resolve)

### What Dies

- `claude-ask/daemon.py` `Daemon` class and socket server
- `claude-ask.sock` unix socket
- Duplicated conversation checks in claude-voice and input.py
- `claude-ask-cancel` talking to claude-ask's socket (retargeted to claude-voice)

### What Stays

- `claude-hid/daemon.py` — already clean, just detects and dispatches
- `claude-voice/daemon.py` — grows slightly (query action, cancel action)
- `input.py` — simplifies (sends to claude-voice instead of claude-ask)
- All claude-ask tool plugins in `tools/` — unchanged
- `format.py`, `sentence_buffer.py` — unchanged
- `waybar_state.py` — unchanged, used by helpers

### Migration Notes

- The `claude-ask` systemd service is removed
- The `claude-voice` systemd service becomes the single query-running service
- `claude-ask` bash script (`.local/bin/claude-ask`) still opens the TUI, just the TUI now talks to claude-voice's socket
- `claude-ask-cancel` retargets from `claude-ask.sock` to `claude-voice.sock`
- `claude-ask-toggle-speak` retargets similarly
