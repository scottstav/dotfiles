# Notification & Status Overhaul Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Move all state indicators from notifications to Waybar, add tool_use state, add stop-TTS via Waybar click, add startup health notifications.

**Architecture:** Daemon writes status + tool_name to JSON state file. Waybar script reads JSON, outputs icons/CSS classes/tooltips. Waybar CSS animates per-state. Click handler sends stop_tts command via Unix socket to daemon. Separate startup-health script checks systemd services and exec-once processes.

**Tech Stack:** Python 3, bash, Waybar custom modules, SwayNC notifications, systemd

---

### Task 1: Add tool_use status to waybar_state.py

**Files:**
- Modify: `~/.local/share/claude-ask/waybar_state.py:53-57`

**Step 1: Update set_status to accept optional tool_name**

Replace `set_status` method in `waybar_state.py`:

```python
def set_status(self, status: str, tool_name: str = ""):
    """Set status to idle, thinking, speaking, or tool_use. Writes + signals."""
    with self._lock:
        self._state["status"] = status
        self._state["tool_name"] = tool_name
        self._write()
```

**Step 2: Add tool_name to initial state dict**

In `__init__`, add `"tool_name": ""` to the `_state` dict after the `"status": "idle"` line.

**Step 3: Verify the daemon still starts**

Run: `cd ~/.local/share/claude-ask && .venv/bin/python3 -c "from waybar_state import WaybarState; w = WaybarState(); w.set_status('tool_use', tool_name='web_search'); import json; print(json.load(open(w._state['status'])))" 2>/dev/null; cat ~/.local/state/claude-ask/waybar.json`

Expected: JSON with `"status": "tool_use"` and `"tool_name": "web_search"`

**Step 4: Commit**

```bash
git add .local/share/claude-ask/waybar_state.py
git commit -m "feat(waybar): add tool_use status with tool_name field"
```

---

### Task 2: Remove notification-based state from daemon.py

**Files:**
- Modify: `~/.local/share/claude-ask/daemon.py`

This task removes 4 notification types and replaces them with Waybar state updates.

**Step 1: Remove _notify_speaking method and _dismiss_speaking_notify method**

Delete lines 291-328 (the `_notify_speaking` and `_dismiss_speaking_notify` methods). They are no longer needed — speaking state is in Waybar.

**Step 2: Remove _notify_usage method**

Delete lines 374-408 (the `_notify_usage` method). Usage is already in Waybar tooltip.

**Step 3: Remove "Thinking..." notification from _stream_response**

In `_stream_response` (around line 472), remove:
```python
self._notify(tag, "Thinking...")
```

The thinking state is now shown in Waybar.

**Step 4: Replace tool-use notification with Waybar state**

In `_complete_conversation`, in the tool execution loop (around line 640), replace:
```python
self._notify(tag, f"Running tool: {tool_block.name}...")
```
with:
```python
self.waybar.set_status("tool_use", tool_name=tool_block.name)
```

**Step 5: Remove _notify_speaking call from _complete_conversation**

In `_complete_conversation` (around line 593), remove:
```python
self._notify_speaking(tag)
```

**Step 6: Remove _dismiss_speaking_notify calls**

In `_complete_conversation` (around line 681), remove:
```python
self._dismiss_speaking_notify(tag)
```

Also remove both `_dismiss_speaking_notify` calls in the error handlers of `handle_query` (around lines 573 and 581).

**Step 7: Set thinking status when entering tool loop iteration**

After a tool result is fed back and the loop continues, re-set thinking state. Add after `conv["messages"].append({"role": "user", "content": tool_results})`:
```python
if self.waybar.speak_enabled:
    self.waybar.set_status("speaking")
else:
    self.waybar.set_status("thinking")
```

**Step 8: Verify daemon still works end-to-end**

Run: `systemctl --user restart claude-ask && sleep 2 && journalctl --user -u claude-ask -n 5 --no-pager`

Expected: Clean startup with "Listening on" message, no import errors.

**Step 9: Commit**

```bash
git add .local/share/claude-ask/daemon.py
git commit -m "feat(notifications): remove state notifications, use Waybar for thinking/speaking/tool_use"
```

---

### Task 3: Add stop_tts IPC to daemon

**Files:**
- Modify: `~/.local/share/claude-ask/daemon.py`

The daemon's `handle_client` already reads JSON from the socket. We need to handle `{"action": "stop_tts"}` messages alongside regular text queries.

**Step 1: Add action routing to handle_client**

In `handle_client`, after parsing the JSON message, check for an action field before the existing text-query logic:

```python
msg = json.loads(data.decode("utf-8"))

# Handle control actions
action = msg.get("action")
if action == "stop_tts":
    log.info("Stop TTS requested via socket")
    self.tts.stop()
    self.waybar.set_status("idle")
    return

text = msg.get("text", "").strip()
# ... rest of existing query handling
```

**Step 2: Update claude-ask-toggle-speak to handle stop-while-speaking**

Modify `~/.local/bin/claude-ask-toggle-speak` to detect if currently speaking and send stop_tts instead:

```bash
#!/bin/bash
# Toggle claude-ask speak mode, or stop TTS if currently speaking

STATE_FILE="$HOME/.local/state/claude-ask/waybar.json"
CONFIG_FILE="$HOME/.config/claude-ask/config.toml"
SOCK="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/claude-ask.sock"

if [ ! -f "$STATE_FILE" ]; then
    echo "claude-ask not running" >&2
    exit 1
fi

# If currently speaking, stop TTS instead of toggling
status=$(python3 -c "import json; print(json.load(open('$STATE_FILE')).get('status',''))")
if [ "$status" = "speaking" ]; then
    python3 -c "
import socket, json
sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
sock.connect('$SOCK')
sock.sendall(json.dumps({'action': 'stop_tts'}).encode())
sock.close()
"
    pkill -SIGRTMIN+12 waybar 2>/dev/null
    exit 0
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

**Step 3: Test stop_tts command**

Run: `echo '{"action":"stop_tts"}' | socat - UNIX-CONNECT:$XDG_RUNTIME_DIR/claude-ask.sock`

Expected: daemon log shows "Stop TTS requested via socket"

**Step 4: Commit**

```bash
git add .local/share/claude-ask/daemon.py .local/bin/claude-ask-toggle-speak
git commit -m "feat(claude-ask): add stop_tts IPC, toggle-speak stops TTS when speaking"
```

---

### Task 4: Overhaul claude-ask-status Waybar script

**Files:**
- Modify: `~/.local/bin/claude-ask-status`

**Step 1: Rewrite the status script with all 5 states**

```bash
#!/bin/bash
# Waybar module for claude-ask status

STATE_FILE="$HOME/.local/state/claude-ask/waybar.json"

if [ ! -f "$STATE_FILE" ]; then
    echo '{"text": "\u000f0369", "class": "idle", "tooltip": "Claude Ask (not running)"}'
    exit 0
fi

python3 -c "
import json
from datetime import datetime

d = json.load(open('$STATE_FILE'))
status = d.get('status', 'idle')
speak = d.get('speak_enabled', False)
wake_muted = d.get('wake_muted', False)
tool_name = d.get('tool_name', '')
usage = d.get('usage', {})
month = usage.get('month_cost', '\$0.00')
last = usage.get('last_query_cost', '\$0.00')

# Icons per state (Nerd Font: nf-md-*)
icons = {
    'idle':     '\U000f0369',  # nf-md-message_text_outline (chat bubble)
    'thinking': '\U000f0674',  # nf-md-creation (sparkle)
    'tool_use': '\U000f0552',  # nf-md-wrench
    'speaking': '\U000f042a',  # nf-md-volume_high (speaker)
    'listening':'\U000f036c',  # nf-md-microphone
}

icon = icons.get(status, icons['idle'])

# Idle: append voice/wake indicators
if status == 'idle':
    if speak:
        icon += ' \U000f057e'  # + nf-md-volume_high (small)
    else:
        icon += ' \U000f0581'  # + nf-md-volume_off
    if wake_muted:
        icon += ' \U000f036d'  # + nf-md-microphone_off

# CSS class
css = status

# Tooltip
month_name = datetime.now().strftime('%b')
if status == 'tool_use' and tool_name:
    tip = f'Running: {tool_name}'
elif status == 'thinking':
    tip = 'Thinking...'
elif status == 'speaking':
    tip = 'Speaking... (click to stop)'
elif status == 'listening':
    tip = 'Listening...'
else:
    tip = f'{month_name}: {month} | Last: {last}'
    tip += ' | Voice: ON' if speak else ' | Voice: OFF'
    tip += ' | Wake: OFF' if wake_muted else ' | Wake: ON'

print(json.dumps({'text': icon, 'tooltip': tip, 'class': css}, ensure_ascii=False))
"
```

**Step 2: Test with each status**

Run:
```bash
# Write test state and check output
echo '{"status":"tool_use","tool_name":"web_search","speak_enabled":false,"usage":{"month_cost":"$1.23","last_query_cost":"$0.01","total_tokens":5000}}' > ~/.local/state/claude-ask/waybar.json
claude-ask-status
```

Expected: JSON with wrench icon, class "tool_use", tooltip "Running: web_search"

**Step 3: Commit**

```bash
git add .local/bin/claude-ask-status
git commit -m "feat(waybar): overhaul claude-ask-status with 5 distinct states"
```

---

### Task 5: Add Waybar CSS animations for each state

**Files:**
- Modify: `~/.config/waybar/style.css`

**Step 1: Add new keyframe animations**

Add after the existing `@keyframes pulse` block:

```css
/* Smooth color glow for claude-ask states */
@keyframes glow-blue {
    0% { color: #89cff0; }
    50% { color: #b4d8f0; }
    100% { color: #89cff0; }
}

@keyframes glow-green {
    0% { color: #a6e3a1; }
    50% { color: #d4f0d3; }
    100% { color: #a6e3a1; }
}

@keyframes glow-amber {
    0% { color: #fab387; }
    50% { color: #fcd5b8; }
    100% { color: #fab387; }
}
```

**Step 2: Replace existing claude-ask CSS block**

Replace the entire `/* Claude Ask module */` section (lines 273-296) with:

```css
/* Claude Ask module */
#custom-claude-ask {
    color: #b4befe;
}

#custom-claude-ask.thinking {
    animation-name: glow-blue;
    animation-duration: 1.5s;
    animation-timing-function: ease-in-out;
    animation-iteration-count: infinite;
}

#custom-claude-ask.tool_use {
    animation-name: glow-amber;
    animation-duration: 1s;
    animation-timing-function: ease-in-out;
    animation-iteration-count: infinite;
}

#custom-claude-ask.speaking {
    animation-name: glow-green;
    animation-duration: 1s;
    animation-timing-function: ease-in-out;
    animation-iteration-count: infinite;
}

#custom-claude-ask.listening {
    color: #fab387;
    animation-name: pulse;
    animation-duration: 0.8s;
    animation-timing-function: ease-in-out;
    animation-iteration-count: infinite;
}
```

**Step 3: Reload Waybar and test visually**

Run: `pkill -SIGUSR2 waybar` (reload config+css)

Then cycle through states:
```bash
# Test each state
for s in idle thinking tool_use speaking listening; do
    echo "{\"status\":\"$s\",\"tool_name\":\"web_search\",\"speak_enabled\":false,\"usage\":{\"month_cost\":\"\$1.23\",\"last_query_cost\":\"\$0.01\",\"total_tokens\":5000}}" > ~/.local/state/claude-ask/waybar.json
    pkill -SIGRTMIN+12 waybar
    sleep 3
done
# Reset to idle
echo '{"status":"idle","speak_enabled":false,"usage":{"month_cost":"$1.23","last_query_cost":"$0.01","total_tokens":5000}}' > ~/.local/state/claude-ask/waybar.json
pkill -SIGRTMIN+12 waybar
```

Expected: Each state shows a distinct icon with its own color animation.

**Step 4: Commit**

```bash
git add .config/waybar/style.css
git commit -m "feat(waybar): add per-state color glow animations for claude-ask"
```

---

### Task 6: Create startup-health script

**Files:**
- Create: `~/.local/bin/startup-health`

**Step 1: Write the startup health checker**

```bash
#!/bin/bash
# Check health of all custom user services and processes after login

# Systemd user services to check
SERVICES=(
    claude-ask
    claude-voice
    voice-typing
    dropbox
    system-freshness
    gcal-refresh
    mbsync
)

# Processes that should be running (from exec-once)
PROCESSES=(
    waybar
    swaync
    hyprpaper
    hypridle
    kdeconnectd
    rquickshare
    emacs
)

failed_services=()
failed_processes=()

# Check systemd services
for svc in "${SERVICES[@]}"; do
    if ! systemctl --user is-active --quiet "$svc.service" 2>/dev/null; then
        failed_services+=("$svc")
    fi
done

# Check processes
for proc in "${PROCESSES[@]}"; do
    if ! pgrep -x "$proc" >/dev/null 2>&1; then
        failed_processes+=("$proc")
    fi
done

total=$(( ${#SERVICES[@]} + ${#PROCESSES[@]} ))
failed=$(( ${#failed_services[@]} + ${#failed_processes[@]} ))
running=$(( total - failed ))

if [ "$failed" -eq 0 ]; then
    notify-send -a "System" \
        -t 8000 \
        "Startup Health" \
        "All $total services healthy"
elif [ "$failed" -le 3 ]; then
    all_failed=("${failed_services[@]}" "${failed_processes[@]}")
    IFS=', '; names="${all_failed[*]}"; unset IFS
    notify-send -a "System" \
        --urgency=normal \
        -t 0 \
        "Startup Health" \
        "$running/$total running — failed: $names"
else
    all_failed=("${failed_services[@]}" "${failed_processes[@]}")
    IFS=', '; names="${all_failed[*]}"; unset IFS
    notify-send -a "System" \
        --urgency=critical \
        -t 0 \
        "Startup Health" \
        "$running/$total running — failed: $names"
fi
```

**Step 2: Make it executable and stow**

Run:
```bash
chmod +x ~/.local/bin/startup-health  # (the dotfiles copy)
cd ~/dotfiles && stow .
```

**Step 3: Test it**

Run: `startup-health`

Expected: Notification showing health status of all services.

**Step 4: Commit**

```bash
git add .local/bin/startup-health
git commit -m "feat: add startup-health script for boot service monitoring"
```

---

### Task 7: Add startup-health to Hyprland config

**Files:**
- Modify: `~/.config/hypr/hyprland.conf`

**Step 1: Add exec-once line**

Add after the existing `exec-once = ssh-add` line (line 323):

```
exec-once = sleep 10 && startup-health
```

The 10-second delay gives systemd services time to start.

**Step 2: Commit**

```bash
git add .config/hypr/hyprland.conf
git commit -m "feat(hyprland): run startup-health on login"
```

---

### Task 8: Integration test — full query cycle

**Step 1: Restart the daemon**

Run: `systemctl --user restart claude-ask`

**Step 2: Send a test query that uses tools**

Run:
```bash
echo '{"text": "What time is it? Use the shell tool to check."}' | socat - UNIX-CONNECT:$XDG_RUNTIME_DIR/claude-ask.sock
```

**Step 3: Observe Waybar transitions**

Expected state sequence in Waybar:
1. Idle → Thinking (blue glow animation, sparkle icon)
2. Thinking → Tool use (amber glow, wrench icon, tooltip "Running: shell")
3. Tool use → Thinking (back to blue)
4. Thinking → Speaking OR Idle (depending on speak_enabled)
5. Final notification appears with response text + Reply/Mic/Open buttons

**Step 4: Test stop TTS (if speak is enabled)**

While Claude is speaking, left-click the Waybar claude-ask module.

Expected: TTS stops, Waybar returns to idle.

**Step 5: Commit all remaining changes**

If any fixes were needed during integration:
```bash
git add -A
git commit -m "fix: integration fixes for notification cleanup"
```
