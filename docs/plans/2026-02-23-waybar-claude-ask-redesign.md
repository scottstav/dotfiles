# Waybar Claude-Ask Redesign + Startup Health Fix Button

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Redesign the claude-ask waybar module into a persistent mode icon + dynamic activity indicator, remove wake toggle notifications, and add a "Fix with Claude" button to startup-health failures.

**Architecture:** The waybar status script is rewritten to output a two-zone display: a persistent Nerd Font icon reflecting speak/wake state (keyboard, robot, ear, talking) plus an optional activity section (thinking, tool_use, speaking, listening) separated by a powerline arrow. The startup-health script uses `notify-send -A` action buttons to launch a floating foot terminal running Claude Code with a fix prompt.

**Tech Stack:** Bash, Python (inline), Waybar JSON protocol, Hyprland window rules, swaync notifications

---

### Task 1: Rewrite claude-ask-status script

**Files:**
- Modify: `.local/bin/claude-ask-status`

**Step 1: Rewrite the status script**

Replace the entire contents of `.local/bin/claude-ask-status` with:

```bash
#!/bin/bash
# Waybar module for claude-ask status — two-zone display

STATE_FILE="$HOME/.local/state/claude-ask/waybar.json"

if [ ! -f "$STATE_FILE" ]; then
    echo '{"text": "󰌌", "class": "idle", "tooltip": "Claude Ask (not running)"}'
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

# Persistent mode icon: reflects speak/wake toggles
if speak and not wake_muted:
    mode_icon = '\U000f06a9'   # 󰚩 robot (both on)
elif speak and wake_muted:
    mode_icon = '\U000f05ca'   # 󰗊 message_text (speak only)
elif not speak and not wake_muted:
    mode_icon = '\U000f02ce'   # 󰋎 ear_hearing (wake only)
else:
    mode_icon = '\U000f030c'   # 󰌌 keyboard (both off)

# Activity indicator: only when not idle
activity_icons = {
    'thinking':  '\U000f0674',  # 󰙴 creation (sparkle)
    'tool_use':  '\U000f0493',  # 󰒓 cog
    'speaking':  '\U000f057e',  # 󰕾 volume_high
    'listening': '\U000f036c',  # 󰍬 microphone
}

if status != 'idle' and status in activity_icons:
    activity = activity_icons[status]
    if status == 'tool_use' and tool_name:
        activity += f' {tool_name}'
    icon = f'{mode_icon} \ue0b0 {activity}'
else:
    icon = mode_icon

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

**Step 2: Test the script manually**

Run: `claude-ask-status`
Expected: JSON output with the mode icon matching current speak/wake state, no activity indicator when idle.

**Step 3: Commit**

```bash
git add .local/bin/claude-ask-status
git commit -m "feat(waybar): redesign claude-ask status with two-zone display"
```

---

### Task 2: Remove wake toggle notification from claude-ask-toggle-wake

**Files:**
- Modify: `.local/bin/claude-ask-toggle-wake`

**Step 1: Remove the notify-send calls at the bottom of the script**

Delete lines 43-47 (the `if/else` block that sends "Wake word enabled/disabled" notifications). Keep everything above — the socket communication, state file update, and waybar signal.

The end of the file should be:

```bash
# Signal Waybar to refresh
pkill -SIGRTMIN+12 waybar 2>/dev/null
```

**Step 2: Test the toggle**

Run: `claude-ask-toggle-wake`
Expected: Waybar icon changes to reflect new wake state. No notification appears.

**Step 3: Commit**

```bash
git add .local/bin/claude-ask-toggle-wake
git commit -m "feat(waybar): remove wake toggle notification, icon shows state"
```

---

### Task 3: Add startup-fix Hyprland window rules

**Files:**
- Modify: `.config/hypr/hyprland.conf:158-161`

**Step 1: Add window rules for startup-fix after the system-update rules**

Insert after line 161 (the last system-update rule):

```
# Startup health fix terminal (PiP-style, top right)
windowrule = match:class ^(startup-fix)$, float on
windowrule = match:class ^(startup-fix)$, size (monitor_w*0.2) (monitor_h*0.2)
windowrule = match:class ^(startup-fix)$, move (monitor_w*0.75) (monitor_h*0.1)
```

**Step 2: Reload Hyprland config**

Run: `hyprctl reload`
Expected: No errors.

**Step 3: Commit**

```bash
git add .config/hypr/hyprland.conf
git commit -m "feat(hyprland): add startup-fix floating window rules"
```

---

### Task 4: Add "Fix with Claude" button to startup-health

**Files:**
- Modify: `.local/bin/startup-health`

**Step 1: Rewrite the notification section to include an action button on failure**

Replace the entire file with:

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
    exit 0
fi

all_failed=("${failed_services[@]}" "${failed_processes[@]}")
IFS=', '; names="${all_failed[*]}"; unset IFS

urgency="normal"
[ "$failed" -gt 3 ] && urgency="critical"

# Show notification with Fix button; notify-send -A waits for action
action=$(notify-send -a "System" \
    --urgency="$urgency" \
    -t 0 \
    -A "fix=Fix with Claude" \
    "Startup Health" \
    "$running/$total running — failed: $names")

if [ "$action" = "fix" ]; then
    prompt="The following services/processes failed on startup: ${names}. Fix this and ensure it doesnt happen again."
    hyprctl dispatch exec -- \
        "foot --app-id=startup-fix -- claude --dangerously-skip-permissions -p '${prompt}' --cwd $HOME/dotfiles"
fi
```

**Step 2: Test the script**

Run: `startup-health`
Expected: If all services are healthy, a simple notification. If any fail, notification with "Fix with Claude" button. Clicking the button opens a floating foot terminal with Claude Code.

**Step 3: Commit**

```bash
git add .local/bin/startup-health
git commit -m "feat(startup-health): add Fix with Claude action button"
```

---

### Task 5: Signal Waybar and verify end-to-end

**Step 1: Signal Waybar to pick up changes**

Run: `pkill -SIGRTMIN+12 waybar`

**Step 2: Visual verification**

Check waybar:
- Idle icon shows correct mode glyph for current speak/wake state
- Left-click toggles speak → icon changes, no extra notification
- Middle-click toggles wake → icon changes, no notification
- Hover shows usage tooltip
- When claude-ask is active, activity indicator appears after powerline arrow

**Step 3: Final commit if any tweaks were needed**
