# Claude Voice: Wake Word + Voice Input for Claude Ask

## Overview

Add always-on voice interaction to the existing claude-ask system. Say "OK Computer" (or "Computer") to trigger speech capture, speak naturally, and have the transcribed text sent to Claude via the existing claude-ask daemon. Claude's response notification includes a Mic button for voice replies.

## Architecture

New service: **claude-voice** — a separate Python daemon handling wake word detection, speech capture, STT, and end-of-speech detection. Communicates with claude-ask over its existing Unix socket.

```
                    ┌──────────────────────────┐
                    │     claude-voice daemon   │
                    │                           │
 Microphone ──────> │  OpenWakeWord (always on) │
                    │        |                  │
                    │   wake word detected      │
                    │        v                  │
                    │  Audio capture + VAD      │
                    │  (live transcription via  │
                    │   notification)           │
                    │        |                  │
                    │   end-of-speech detected  │
                    │        v                  │
                    │  faster-whisper (final)   │
                    │        |                  │
                    └────────|──────────────────┘
                             | text via Unix socket
                             v
                    ┌──────────────────────────┐
                    │    claude-ask daemon      │
                    │  (existing, unchanged*)   │
                    │                           │
                    │  Claude API -> notification│
                    │  with new "Mic" button    │
                    └──────────────────────────┘

* Only change: add "Mic" action button to notification
```

### States

1. **Idle** — OpenWakeWord listening on mic, ~1-2% CPU
2. **Listening** — Wake word detected, capturing audio, showing live transcription in notification
3. **Processing** — Final Whisper transcription, sending to claude-ask
4. **Waiting** — Claude responding; back to idle for wake word, also accepts mic-button triggers

## Wake Word Detection

- **Engine:** OpenWakeWord with a community-trained model from the Home Assistant Wakewords Collection (`ok_computer` or `computer`)
- **Audio format:** 16kHz, mono, int16, 1280-sample chunks (80ms)
- **Speex noise suppression:** Enabled (Linux-native)
- **Detection threshold:** Configurable (default 0.5)
- **On detection:** Immediately transition to Listening state; preserve last ~0.5s of audio as pre-roll buffer

## Speech Capture & End-of-Speech Detection

### Audio Pipeline

Once wake word fires:
1. Pre-roll buffer (~0.5s) preserved so start of speech isn't clipped
2. Audio streams to WebRTC VAD for voice activity tracking
3. Whisper runs periodically (~every 2s) on accumulated audio for interim transcription
4. Live transcription shown in notification (debounced ~500ms)

### End-of-Speech Detection (Hybrid)

**Smart silence detection** (default ON, togglable):
- Base silence timeout: 2.5 seconds
- If interim transcription ends with sentence-ending pattern (`.` `?` `!`), reduce to 1.5s
- If mid-sentence (no punctuation, trailing conjunctions), extend to 3.5s

**Force-send triggers:** Say "send it" or "that's it" to immediately send

**Fallback mode** (when smart detection OFF): Fixed 3-second silence timeout

### Final Transcription

When end-of-speech triggers, run Whisper on the complete audio buffer for the final, most accurate transcription. This is the text sent to claude-ask.

## Integration with claude-ask

### Sending Queries

claude-voice sends final transcription to claude-ask's Unix socket at `$XDG_RUNTIME_DIR/claude-ask.sock` using the same JSON protocol as the TUI:
- New conversation: `{"query": "transcribed text"}`
- Reply (mic button): `{"query": "transcribed text", "conversation_id": "abc123"}`

The existing <30-second auto-reply behavior works automatically — voice queries without a conversation_id that arrive within 30s of the last query auto-continue the most recent conversation.

### Mic Button

claude-ask adds a "Mic" action button to the final response notification (alongside "Reply" and "Open in Emacs").

When clicked, claude-ask sends a listen command to claude-voice's control socket at `$XDG_RUNTIME_DIR/claude-voice.sock`:
```json
{"action": "listen", "conversation_id": "abc123"}
```

This makes claude-voice skip wake word detection and go straight to Listening state, with the conversation_id attached to the eventual query.

### Notification Flow

1. Wake word detected -> notification: "Listening..." (with live transcription updating)
2. End-of-speech -> notification: "Sending to Claude..."
3. Claude streams response -> same streaming notification as today
4. Final notification -> "Reply" | "Mic" | "Open in Emacs"

## Shared STT Configuration

claude-voice reads Whisper model settings (model size, device, compute type) from `~/.config/voice-typing/config.yaml` — the same config used by the existing voice-typing daemon. Changing models in one place affects both systems on restart.

## Configuration

`~/.config/claude-voice/config.yaml`:
```yaml
wake_word:
  model_path: ~/.local/share/claude-voice/models/ok_computer.tflite
  threshold: 0.5
  pre_roll_seconds: 0.5

speech:
  smart_silence: true
  silence_timeout: 2.5
  force_send_phrases: ["send it", "that's it"]
```

## Deployment

### File Layout

```
~/.local/share/claude-voice/
  ├── daemon.py
  ├── requirements.txt
  ├── .venv/
  └── models/
      └── ok_computer.tflite

~/.config/claude-voice/
  └── config.yaml

~/.config/systemd/user/claude-voice.service
```

### Systemd Service

Always-on, starts on login, depends on claude-ask:

```ini
[Unit]
Description=Claude Voice daemon (wake word + STT for claude-ask)
After=claude-ask.service
Requires=claude-ask.service

[Service]
Type=simple
Environment=PYTHONUNBUFFERED=1
ExecStart=%h/.local/share/claude-voice/.venv/bin/python3 %h/.local/share/claude-voice/daemon.py
Restart=on-failure
RestartSec=5

[Install]
WantedBy=default.target
```

### Dependencies (requirements.txt)

- openwakeword
- faster-whisper
- pyaudio
- webrtcvad
- numpy
- pyyaml

## Changes to claude-ask

Minimal — add "Mic" action to notify-send and handle the mic action by sending a listen command to claude-voice's socket (~15-20 lines).
