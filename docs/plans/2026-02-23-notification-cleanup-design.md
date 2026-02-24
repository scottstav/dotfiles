# Notification & Status Overhaul Design

## Goal

Reduce notification noise by moving state indicators to Waybar. Overhaul the claude-ask Waybar module with distinct animated states. Add startup health reporting for all custom daemons.

## Part 1: Claude-Ask Notification Cleanup

### Notifications to REMOVE

1. **"Thinking..."** — replaced by Waybar thinking animation
2. **"Speaking..." + Stop button** — replaced by Waybar speaking animation; Stop action moves to Waybar click
3. **Usage notification** — already shown in Waybar tooltip
4. **"Running tool: X..."** — replaced by Waybar tool-use animation with tool name in tooltip

### Notifications to KEEP

1. **Streaming text updates** — in-place replacement, debounced every 0.2s. This IS the response content.
2. **Final response** — persistent, with Reply/Mic/Open in Emacs action buttons.

### Net effect

From 5+ notification types per query down to 2. All state/status info lives in Waybar.

## Part 2: Waybar Claude Module Overhaul

### State Machine

```
idle → thinking → [tool_use ↔ thinking]* → speaking → idle
idle → thinking → [tool_use ↔ thinking]* → idle  (speak disabled)
```

### States

| State | Icon | Color | Animation | Tooltip |
|-------|------|-------|-----------|---------|
| idle | chat bubble (nf-md-message_text_outline) + volume/mic indicators | lavender (#b4befe) | static | "Feb: $X.XX \| Voice: ON/OFF \| Wake: ON/OFF" |
| thinking | sparkle (nf-md-creation) | blue (#89cff0) | color pulse (1.5s cycle) | "Thinking..." |
| tool_use | wrench (nf-md-wrench) | amber (#fab387) | color pulse (1s cycle) | "Running: <tool_name>" |
| speaking | speaker (nf-md-volume_high) | green (#a6e3a1) | glow pulse (1s cycle) | "Speaking..." |
| listening | microphone (nf-md-microphone) | peach (#fab387) | fast pulse (0.8s cycle) | "Listening..." |

### Click Actions

| Click | Action |
|-------|--------|
| Left-click (speaking) | Stop TTS |
| Left-click (other) | Toggle speak on/off |
| Middle-click | Toggle wake word on/off |

### Implementation

**waybar_state.py changes:**
- Add `tool_use` status with `tool_name` field
- Add `listening` status (triggered by claude-voice socket)

**claude-ask-status script changes:**
- Read new states from JSON, map to icons/colors/tooltips
- Add distinct CSS classes for each state

**Waybar style.css changes:**
- Add `@keyframes` for each state's unique animation
- Different animation speeds and colors per state

**daemon.py changes:**
- Remove `_notify("Thinking...")` call
- Remove `_notify_speaking()` and `_dismiss_speaking_notify()`
- Remove `_notify_usage()`
- Remove tool-use `_notify()` calls
- Add `waybar.set_status("tool_use", tool_name=...)` during tool execution
- Add stop-TTS IPC: write a command socket or use SIGUSR1 for TTS stop from Waybar click

### Stop TTS from Waybar

Option: `claude-ask-toggle-speak` script checks if status is "speaking" and sends stop signal.

Simplest approach: when left-clicked while speaking, the click handler sends a message to the daemon's Unix socket:
```json
{"action": "stop_tts"}
```

The daemon receives this, calls `self.tts.stop()`, and transitions to idle.

Need to add a simple IPC handler for control commands alongside the existing query handler.

## Part 3: Startup Health Notifications

### Script: `startup-health`

Runs as `exec-once` in Hyprland config (with sleep delay for services to start).

**Checks:**

1. **Systemd user services** (must be `active`):
   - claude-ask, claude-voice, voice-typing
   - dropbox, system-freshness, gcal-refresh, mbsync

2. **One-shot processes** (must be running):
   - waybar, swaync, hyprpaper, hypridle
   - kdeconnectd, rquickshare, emacs

**Output:**

Single notification summarizing health:
- All green: "All N services healthy" (normal urgency, 8s timeout)
- Partial: "M/N services running — failed: X, Y" (warning urgency, persistent)
- Bad: "Only M/N services running" (critical urgency, persistent)

### Hyprland integration

```
exec-once = sleep 8 && startup-health
```

## Summary of File Changes

1. `~/.local/share/claude-ask/daemon.py` — remove 4 notification types, add tool_use waybar state, add stop_tts IPC
2. `~/.local/share/claude-ask/waybar_state.py` — add tool_use and listening states, add tool_name field
3. `~/.local/bin/claude-ask-status` — rewrite with new states/icons/tooltips
4. `~/.config/waybar/style.css` — add per-state animations
5. `~/.local/bin/startup-health` — new script
6. `~/.config/hypr/hyprland.conf` — add exec-once for startup-health
