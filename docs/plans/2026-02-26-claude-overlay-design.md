# Claude Overlay Design

**Date:** 2026-02-26
**Status:** Approved

## Problem

Claude-ask streams Claude's responses through swaync notifications by spawning a
new `notify-send` subprocess every 200ms. This is slow (~5 spawns/sec), buggy
(swaync wasn't designed for rapid in-place updates), and visually janky.

Notifications should be reserved for "hard state" — final responses with action
buttons, errors, worker lifecycle events. Streaming text needs a dedicated,
fast, unobtrusive overlay.

## Solution

A small C program (`claude-overlay`) that uses `wlr-layer-shell-unstable-v1`
and Cairo/Pango to render a streaming text overlay directly on the Wayland
compositor. No toolkit, no framework — just direct protocol + rendering. Same
stack as wob.

## Architecture

```
Claude API stream -> query.py -> Unix socket -> claude-overlay (render)
                         |
                    notify_final() -> swaync (actions only)
```

### Components

1. **`claude-overlay`** — C binary, runs as a systemd user service. Listens on
   a Unix domain socket, renders text with Pango/Cairo on a wlr-layer-shell
   surface.
2. **Config file** — `~/.config/claude-overlay/config`. Font, colors,
   dimensions, animation timing. All optional, sane defaults compiled in.
3. **Python integration** — `stream_response()` in `query.py` sends text deltas
   to the overlay socket instead of calling `notify()`.

## Socket Protocol

**Location:** `$XDG_RUNTIME_DIR/claude-overlay.sock`

### Client -> Overlay (JSON lines)

| Command | Description |
|---------|-------------|
| `{"cmd": "open"}` | Show surface, clear previous content |
| `{"cmd": "text", "data": "chunk"}` | Append text (streaming delta) |
| `{"cmd": "done"}` | Start fade-out timer |
| `{"cmd": "clear"}` | Immediately hide and clear |
| `{"cmd": "replace", "data": "full text"}` | Replace entire content |

### Overlay -> Client (future, not P0)

| Event | Description |
|-------|-------------|
| `{"event": "dismissed"}` | User dismissed overlay |
| `{"event": "action", "name": "..."}` | User clicked action button |

### Behavior

- Multiple clients can connect. One surface — last writer wins.
- `text` without prior `open` implicitly opens.
- After `done`, fade out over ~500ms, then hide surface.
- New `open` during fade-out cancels fade, starts fresh.

## Rendering

### Surface Positioning

- **Layer:** `ZWLR_LAYER_SHELL_V1_LAYER_OVERLAY`
- **Anchor:** top-center, offset below waybar's exclusive zone (drops down from
  under the `custom/claude-ask` waybar module)
- **Width:** configurable, default 600px
- **Height:** dynamic, 1 line to 12 lines max

### Text

- Pango for font loading, shaping, line wrapping, Unicode
- Font from config (Pango font description string)
- Word-wrap at surface width minus padding

### Growth and Scroll Animation

- Surface starts at 1-line height, grows as text wraps to more lines
- At 12 lines, height locks. New text triggers smooth scroll animation.
- Scroll: floating-point `scroll_offset` animates toward target over ~150ms
  (configurable), eased. Render via Cairo translation.
- Animation driven by `timerfd` at ~60fps during animation, idle otherwise.

### Appearance

- Semi-transparent dark background (default `#1a1a2e` at 90% opacity)
- Light text (default `#e0e0e0`)
- Rounded corners (default 8px radius)
- Subtle border (1px, configurable)
- Padding (12px horizontal, 8px vertical)
- Fade-out: animate surface opacity 100% -> 0% over ~500ms

### Input

No keyboard or mouse input for P0. Surface is non-interactive. Actions bar is a
future addition.

## Config File

**Location:** `~/.config/claude-overlay/config`

Key-value format, `#` comments, all keys optional with compiled-in defaults.

```ini
# Font (Pango font description)
font = Sans 13

# Colors (RGBA hex)
background = #1a1a2ee6
text_color = #e0e0e0ff
border_color = #333355ff

# Dimensions (pixels)
width = 600
max_lines = 12
padding_x = 12
padding_y = 8
corner_radius = 8
border_width = 1

# Position
margin_top = 0

# Animation (milliseconds)
scroll_duration = 150
fade_duration = 500
```

## Python Integration

Changes to `stream_response()` in `query.py`:

- Replace `notify()` calls with socket writes sending text **deltas** (not
  accumulated text)
- Drop or significantly reduce debounce — socket write is ~1000x cheaper than
  spawning `notify-send`
- Send `open` before streaming, `done` after
- `notify_final()` unchanged — swaync still handles persistent action
  notifications
- Graceful degradation: if overlay socket unavailable, skip silently

Helper functions (~15 lines):
- `_connect_overlay()` — connect to socket, return socket or None
- `_overlay_send(sock, msg)` — JSON-line write with error handling

## Process Lifecycle

### Systemd Service

`~/.config/systemd/user/claude-overlay.service`:

```ini
[Unit]
Description=Claude streaming text overlay
After=graphical-session.target

[Service]
ExecStart=%h/.local/bin/claude-overlay
Restart=on-failure
RestartSec=1

[Install]
WantedBy=graphical-session.target
```

- Idles at near-zero CPU when no surface is shown
- Creates socket on startup, removes on exit
- Connects to compositor via `WAYLAND_DISPLAY`

## File Layout

```
.local/share/claude-overlay/    # C source + meson.build
.config/claude-overlay/config   # Default config
.config/systemd/user/claude-overlay.service
.local/bin/claude-overlay       # Built binary (not tracked in git)
```

## Build

Meson + ninja. Dependencies: wayland-client, wayland-protocols,
wlr-protocols, cairo, pango, pangocairo.

## Future (not P0)

- Bidirectional socket communication for overlay -> client events
- Actions bar (reply, mic, open buttons) replacing swaync final notification
- Keyboard/mouse input on the overlay surface
- Full UI for claude-ask system built on this foundation

## Approach Chosen

Pure C + wlr-layer-shell + Cairo/Pango. No toolkit (GTK4 too heavy, defeats
the speed goal). No external event loop library (`poll()` with 2-3 fds is
trivial). Matches the wob model — tiny, direct, fast.
