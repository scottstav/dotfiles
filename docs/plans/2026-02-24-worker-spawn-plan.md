# Claude Ask → Worker Spawn Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Let claude-ask queue pending Claude Code worker sessions that users approve asynchronously via notifications, conversation replies, or waybar.

**Architecture:** Two new tool plugins (`spawn_worker`, `manage_workers`) create/manage pending worker state files in `~/.local/state/claude-worker/`. A new `claude-worker-approve` script handles approval/denial from any source. Waybar clicks are remapped (left=workers, middle=voice cycle, right=new worker). Worker sessions get completion notifications.

**Tech Stack:** Python 3 (tool plugins), Bash (scripts), JSON state files, notify-send, fuzzel, waybar, foot terminal

---

### Task 1: `claude-worker-approve` Script

The foundation. Everything else calls this script to approve or deny pending workers.

**Files:**
- Create: `.local/bin/claude-worker-approve`

**Step 1: Write the script**

```bash
#!/bin/bash
# Approve or deny a pending Claude worker
# Usage: claude-worker-approve [--deny] <id>

STATE_DIR="$HOME/.local/state/claude-worker"

deny=false
if [[ "$1" == "--deny" ]]; then
    deny=true
    shift
fi

id="$1"
if [[ -z "$id" ]]; then
    echo "Usage: claude-worker-approve [--deny] <id>" >&2
    exit 1
fi

state_file="$STATE_DIR/$id.json"
if [[ ! -f "$state_file" ]]; then
    echo "No state file for worker $id" >&2
    exit 1
fi

# Read state
status=$(python3 -c "import json; print(json.load(open('$state_file')).get('status',''))")
if [[ "$status" != "pending" ]]; then
    echo "Worker $id is not pending (status: $status)" >&2
    exit 1
fi

if $deny; then
    rm -f "$state_file"
    pkill -SIGRTMIN+12 waybar 2>/dev/null
    echo "Worker $id denied and removed."
    exit 0
fi

# Approve: read dir and task, launch worker session
dir=$(python3 -c "import json; print(json.load(open('$state_file')).get('directory',''))")
task=$(python3 -c "import json; print(json.load(open('$state_file')).get('task',''))")

if [[ -z "$dir" || -z "$task" ]]; then
    echo "Invalid state file for worker $id" >&2
    exit 1
fi

# Launch in foot terminal via hyprctl so it gets its own window
hyprctl dispatch exec -- \
    "foot --app-id=claude-worker-$id -- claude-worker-session '$id' '$dir' '$task'"

echo "Worker $id approved and launching."
```

**Step 2: Make it executable and test**

Run: `chmod +x .local/bin/claude-worker-approve`

Manual test: Create a fake pending state file, run the script with `--deny`, verify it's removed. Then create another, run without `--deny`, verify foot launches.

**Step 3: Commit**

```bash
git add .local/bin/claude-worker-approve
git commit -m "feat: add claude-worker-approve script for pending worker management"
```

---

### Task 2: `spawn_worker` Tool Plugin

**Files:**
- Create: `.local/share/claude-ask/tools/spawn_worker.py`

**Step 1: Write the tool**

```python
"""Queue a pending Claude Code worker session."""

import json
import os
import subprocess
import uuid
from datetime import datetime, timezone
from pathlib import Path

name = "spawn_worker"
description = (
    "Queue a pending Claude Code worker session for complex coding tasks. "
    "The worker runs autonomously in its own terminal with full codebase access, "
    "file editing, git, and multi-file reasoning. Use this when the task requires "
    "changes across multiple files or deep codebase understanding. "
    "The user must approve the worker before it starts."
)
input_schema = {
    "type": "object",
    "properties": {
        "directory": {
            "type": "string",
            "description": "Absolute path to the project directory the worker should operate in",
        },
        "task": {
            "type": "string",
            "description": (
                "Detailed task description for the worker. Be thorough — "
                "the worker operates autonomously without follow-up questions."
            ),
        },
    },
    "required": ["directory", "task"],
}

STATE_DIR = Path.home() / ".local" / "state" / "claude-worker"


def run(input_data):
    directory = input_data["directory"]
    task = input_data["task"]

    # Validate directory exists
    if not Path(directory).is_dir():
        return f"Error: directory {directory} does not exist."

    # Generate worker ID and write pending state
    worker_id = str(int(datetime.now().timestamp()))
    STATE_DIR.mkdir(parents=True, exist_ok=True)
    state_file = STATE_DIR / f"{worker_id}.json"
    state = {
        "status": "pending",
        "directory": directory,
        "task": task,
        "created": datetime.now(timezone.utc).isoformat(),
    }
    state_file.write_text(json.dumps(state, indent=2))

    # Signal waybar
    subprocess.Popen(
        ["pkill", "-SIGRTMIN+12", "waybar"],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )

    # Fire non-blocking notification with approve/deny actions
    task_preview = task[:150] + ("..." if len(task) > 150 else "")
    dir_short = directory.replace(str(Path.home()), "~")
    subprocess.Popen(
        [
            "notify-send", "-t", "0",
            "-a", "Claude Worker",
            "-A", f"approve=Start",
            "-A", f"deny=Decline",
            "--wait",
            f"Pending Worker ({dir_short})",
            task_preview,
        ],
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        # Handle action in a wrapper — see below
    )
    # We can't easily wait for the notification action in a non-blocking way
    # from within the tool. Instead, spawn a background handler.
    _spawn_notification_handler(worker_id)

    return (
        f"Worker queued as pending (id: {worker_id}).\n"
        f"Directory: {dir_short}\n"
        f"The user will see a notification to approve or decline."
    )


def _spawn_notification_handler(worker_id):
    """Spawn a background process to handle notification actions."""
    # This script waits for the notification action and calls approve/deny
    handler_script = f"""
import subprocess, sys
result = subprocess.run(
    ["notify-send", "-t", "0", "-a", "Claude Worker",
     "-A", "approve=Start", "-A", "deny=Decline", "--wait",
     "Pending Worker", "Check notification center"],
    capture_output=True, text=True
)
action = result.stdout.strip()
if action == "approve":
    subprocess.run(["claude-worker-approve", "{worker_id}"])
elif action == "deny":
    subprocess.run(["claude-worker-approve", "--deny", "{worker_id}"])
"""
    # Actually, the simpler approach: use a shell one-liner as background process
    # that reads the first notification's action
    cmd = (
        f'action=$(notify-send -t 0 -a "Claude Worker" '
        f'-A "approve=Start" -A "deny=Decline" --wait '
        f'"Pending Worker" "$(python3 -c \'import json; d=json.load(open("{STATE_DIR}/{worker_id}.json")); '
        f'print(d[\"task\"][:200])\')" 2>/dev/null); '
        f'[ "$action" = "approve" ] && claude-worker-approve {worker_id}; '
        f'[ "$action" = "deny" ] && claude-worker-approve --deny {worker_id}'
    )
    subprocess.Popen(
        ["bash", "-c", cmd],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
```

Wait — that notification handler is getting gnarly. Let me simplify. The tool should just write the state file, fire a simple notification handler script, and return. Let me refactor.

Actually, the cleanest approach: create a tiny `claude-worker-notify-pending` script that `spawn_worker.py` calls in the background. This keeps the tool plugin clean.

**Step 1 (revised): Write a notification handler script**

Add to `.local/bin/claude-worker-notify-pending`:

```bash
#!/bin/bash
# Show pending worker notification and handle action
# Usage: claude-worker-notify-pending <id>

id="$1"
STATE_DIR="$HOME/.local/state/claude-worker"
state_file="$STATE_DIR/$id.json"

[[ ! -f "$state_file" ]] && exit 1

task=$(python3 -c "import json; print(json.load(open('$state_file')).get('task','')[:200])")
dir=$(python3 -c "import json; d=json.load(open('$state_file')).get('directory',''); print(d.replace('$HOME','~'))")

action=$(notify-send -t 0 -a "Claude Worker" \
    -A "approve=\U000f040a Start" -A "deny=\U000f0156 Decline" \
    --wait \
    "Pending Worker ($dir)" "$task" 2>/dev/null)

case "$action" in
    approve) claude-worker-approve "$id" ;;
    deny) claude-worker-approve --deny "$id" ;;
esac
```

**Step 2: Write the tool plugin (simplified)**

`.local/share/claude-ask/tools/spawn_worker.py`:

```python
"""Queue a pending Claude Code worker session."""

import json
import os
import subprocess
from datetime import datetime, timezone
from pathlib import Path

name = "spawn_worker"
description = (
    "Queue a pending Claude Code worker session for complex coding tasks. "
    "The worker runs autonomously in its own terminal with full codebase access, "
    "file editing, git, and multi-file reasoning. Use this when the task requires "
    "changes across multiple files or deep codebase understanding. "
    "The user must approve the worker before it starts."
)
input_schema = {
    "type": "object",
    "properties": {
        "directory": {
            "type": "string",
            "description": "Absolute path to the project directory the worker should operate in",
        },
        "task": {
            "type": "string",
            "description": (
                "Detailed task description for the worker. Be thorough — "
                "the worker operates autonomously without follow-up questions."
            ),
        },
    },
    "required": ["directory", "task"],
}

STATE_DIR = Path.home() / ".local" / "state" / "claude-worker"


def run(input_data):
    directory = input_data["directory"]
    task = input_data["task"]

    if not Path(directory).is_dir():
        return f"Error: directory {directory} does not exist."

    worker_id = str(int(datetime.now().timestamp()))
    STATE_DIR.mkdir(parents=True, exist_ok=True)
    state_file = STATE_DIR / f"{worker_id}.json"
    state = {
        "status": "pending",
        "directory": directory,
        "task": task,
        "created": datetime.now(timezone.utc).isoformat(),
    }
    state_file.write_text(json.dumps(state, indent=2))

    # Signal waybar
    subprocess.Popen(
        ["pkill", "-SIGRTMIN+12", "waybar"],
        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
    )

    # Fire notification handler in background
    subprocess.Popen(
        ["claude-worker-notify-pending", worker_id],
        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
    )

    dir_short = directory.replace(str(Path.home()), "~")
    return (
        f"Worker queued as pending (id: {worker_id}).\n"
        f"Directory: {dir_short}\n"
        f"The user will see a notification to approve or decline."
    )
```

**Step 3: Make notification script executable**

Run: `chmod +x .local/bin/claude-worker-notify-pending`

**Step 4: Test the tool manually**

```bash
cd ~/.local/share/claude-ask
python3 -c "
from tools.spawn_worker import run
print(run({'directory': '$HOME/dotfiles', 'task': 'Test pending worker'}))
"
```

Verify: state file created in `~/.local/state/claude-worker/`, notification appears, waybar refreshes.

**Step 5: Commit**

```bash
git add .local/bin/claude-worker-notify-pending .local/share/claude-ask/tools/spawn_worker.py
git commit -m "feat: add spawn_worker tool plugin and notification handler"
```

---

### Task 3: `manage_workers` Tool Plugin

**Files:**
- Create: `.local/share/claude-ask/tools/manage_workers.py`

**Step 1: Write the tool**

```python
"""List, approve, or cancel Claude Code workers."""

import glob
import json
import subprocess
from pathlib import Path

name = "manage_workers"
description = (
    "List all Claude Code workers (pending, working, done), approve pending workers "
    "to start them, or cancel pending workers. Use when the user asks about worker "
    "status, wants to approve or cancel a queued worker, or says something like "
    "'go ahead with that worker' or 'start the pending worker'."
)
input_schema = {
    "type": "object",
    "properties": {
        "action": {
            "type": "string",
            "enum": ["list", "approve", "cancel"],
            "description": (
                "list: show all workers and their status. "
                "approve: start a pending worker. "
                "cancel: remove a pending worker."
            ),
        },
        "worker_id": {
            "type": "string",
            "description": "Worker ID or partial match (required for approve/cancel).",
        },
    },
    "required": ["action"],
}

STATE_DIR = Path.home() / ".local" / "state" / "claude-worker"


def _load_workers():
    """Load all worker state files."""
    workers = []
    for f in sorted(STATE_DIR.glob("*.json")):
        try:
            data = json.loads(f.read_text())
            data["id"] = f.stem
            workers.append(data)
        except (json.JSONDecodeError, OSError):
            pass
    return workers


def _find_worker(worker_id, workers):
    """Find a worker by exact or partial ID match."""
    for w in workers:
        if w["id"] == worker_id or w["id"].startswith(worker_id):
            return w
    return None


def run(input_data):
    action = input_data["action"]
    worker_id = input_data.get("worker_id", "")

    workers = _load_workers()

    if action == "list":
        if not workers:
            return "No workers."
        lines = []
        for w in workers:
            dir_short = w.get("directory", "").replace(str(Path.home()), "~")
            task_preview = w.get("task", "")[:80]
            lines.append(f"[{w['status']}] {w['id']} | {dir_short} | {task_preview}")
        return "\n".join(lines)

    if not worker_id:
        # If only one pending worker, auto-select it
        pending = [w for w in workers if w["status"] == "pending"]
        if len(pending) == 1:
            worker_id = pending[0]["id"]
        else:
            return f"Multiple pending workers. Specify worker_id. Use action=list to see them."

    worker = _find_worker(worker_id, workers)
    if not worker:
        return f"No worker found matching '{worker_id}'."

    if action == "approve":
        if worker["status"] != "pending":
            return f"Worker {worker['id']} is {worker['status']}, not pending."
        result = subprocess.run(
            ["claude-worker-approve", worker["id"]],
            capture_output=True, text=True,
        )
        return result.stdout.strip() or f"Worker {worker['id']} approved."

    if action == "cancel":
        if worker["status"] != "pending":
            return f"Worker {worker['id']} is {worker['status']}, not pending."
        result = subprocess.run(
            ["claude-worker-approve", "--deny", worker["id"]],
            capture_output=True, text=True,
        )
        return result.stdout.strip() or f"Worker {worker['id']} cancelled."

    return f"Unknown action: {action}"
```

**Step 2: Test manually**

```bash
# Create a fake pending worker
mkdir -p ~/.local/state/claude-worker
echo '{"status":"pending","directory":"/tmp","task":"test"}' > ~/.local/state/claude-worker/99999.json

cd ~/.local/share/claude-ask
python3 -c "
from tools.manage_workers import run
print(run({'action': 'list'}))
print(run({'action': 'cancel', 'worker_id': '99999'}))
"
```

Verify: list shows the worker, cancel removes the file.

**Step 3: Commit**

```bash
git add .local/share/claude-ask/tools/manage_workers.py
git commit -m "feat: add manage_workers tool plugin for listing/approving/cancelling"
```

---

### Task 4: Update Daemon System Prompt

**Files:**
- Modify: `.local/share/claude-ask/daemon.py:51-65` (SYSTEM_PROMPT)

**Step 1: Update SYSTEM_PROMPT**

Add the worker paragraph after the existing tool instructions. The full SYSTEM_PROMPT becomes:

```python
SYSTEM_PROMPT = """\
You are a quick-answer assistant running as a desktop overlay on a Linux workstation \
(Arch Linux, Hyprland/Wayland, Emacs). Responses are displayed in desktop notifications \
with limited space.

Be extremely concise. Default to 1-3 sentences. Use bullet points over paragraphs. \
Skip preamble, hedging, and sign-offs. Only give longer responses when the user \
explicitly asks for detail or the question genuinely requires it.

You have tools. Use them proactively:
- shell: run commands to answer questions about the system, files, processes, etc.
- web_search + fetch_url: search the web, then read articles for current info/news.
- clipboard: copy useful output to the user's clipboard without being asked.
- screenshot: capture the screen when the user asks about something visible.

You can delegate complex coding tasks to a Claude Code worker session. Workers are \
autonomous agents running in their own terminal with full codebase context, file \
editing, git, and multi-file reasoning. Use spawn_worker when the task requires:
- Changes across multiple files
- Understanding significant codebase context to make even a single change \
  (tracing call chains, reading tests, understanding architecture)
- Implementing a feature, refactoring, or debugging that requires exploration

Do NOT use spawn_worker for simple tasks you can handle with the shell tool: \
quick file edits, adding a journal entry, changing a config value, running a command. \
If in doubt, just answer — the user can always ask for a worker explicitly.

When using spawn_worker, infer the project directory from context. Common locations: \
~/code/, ~/dotfiles/, ~/Dropbox/. Use the shell tool to search if unsure (e.g. \
fd -t d <name> ~). Write a thorough, detailed task description for the worker — \
it operates autonomously without follow-up.\
"""
```

**Step 2: Restart daemon and test**

Run: `systemctl --user restart claude-ask`

Test via `claude-ask`: type something like "Add a new API endpoint to ai-media-svc for fetching user profiles, with tests and error handling". Claude should call `spawn_worker`.

**Step 3: Commit**

```bash
git add .local/share/claude-ask/daemon.py
git commit -m "feat: update system prompt with worker delegation guidance"
```

---

### Task 5: Worker Completion Notification

**Files:**
- Modify: `.local/bin/claude-worker-session:56-72`

**Step 1: Add notification after claude finishes**

After the `claude -p` command completes and state is updated to "done", add a notification before the interactive prompt. Replace lines 56-72 of `claude-worker-session` with:

```bash
# Update state to done
jq -n --arg dir "$dir" --arg task "$task" \
    '{"status": "done", "directory": $dir, "task": $task}' > "$STATE_FILE"
pkill -SIGRTMIN+12 waybar

# Notify completion — run in background so terminal prompt appears immediately
(
    task_preview=$(echo "$task" | head -c 200)
    action=$(notify-send -t 0 -a "Claude Worker" \
        -A "focus=Focus" -A "dismiss=OK" \
        --wait \
        "Worker Done" "$task_preview" 2>/dev/null)

    if [[ "$action" == "focus" ]]; then
        # Bring this worker's foot window to the foreground
        window_addr=$(hyprctl clients -j | jq -r \
            ".[] | select(.class == \"claude-worker-$ID\") | .address")
        if [[ -n "$window_addr" ]]; then
            ws=$(hyprctl clients -j | jq -r \
                ".[] | select(.class == \"claude-worker-$ID\") | .workspace.name")
            if [[ "$ws" == special:* ]]; then
                hyprctl dispatch movetoworkspace e+0,address:"$window_addr"
            fi
            hyprctl dispatch focuswindow address:"$window_addr"
        fi
    fi
) &

echo
echo "Worker done."
echo "  [Enter] Close window"
echo "  [r]     Resume session interactively"
read -r -n 1 choice
echo

if [[ "$choice" == "r" || "$choice" == "R" ]]; then
    setsid alacritty --working-directory "$dir" -e claude --resume "$SESSION_ID" --dangerously-skip-permissions &>/dev/null &
fi
```

**Step 2: Test by running a quick worker**

```bash
claude-worker-session test123 /tmp "echo hello world"
```

Verify notification appears with "Worker Done" and Focus/OK actions.

**Step 3: Commit**

```bash
git add .local/bin/claude-worker-session
git commit -m "feat: add completion notification to claude-worker-session"
```

---

### Task 6: Waybar Click Remapping

**Files:**
- Modify: `.config/waybar/config:196-205`

**Step 1: Remap clicks**

Change the `custom/claude-ask` module:

```json
"custom/claude-ask": {
    "exec": "claude-ask-status",
    "return-type": "json",
    "interval": 2,
    "on-click": "claude-worker-list",
    "on-click-middle": "claude-voice-cycle",
    "on-click-right": "claude-worker-toggle",
    "signal": 12,
    "tooltip": true
}
```

- Left click: `claude-worker-list` (new — shows fuzzel list of workers)
- Middle click: `claude-voice-cycle` (new — cycles voice modes)
- Right click: `claude-worker-toggle` (existing — spawns new worker)

**Step 2: Commit**

```bash
git add .config/waybar/config
git commit -m "feat: remap waybar claude-ask clicks (left=workers, mid=voice, right=new)"
```

---

### Task 7: `claude-worker-list` Script (Left-Click)

**Files:**
- Create: `.local/bin/claude-worker-list`

**Step 1: Write the script**

```bash
#!/bin/bash
# Show fuzzel menu of workers. Left-click waybar action.
# - Pending workers → approve on selection
# - Working workers → focus window on selection
# - Done workers → focus window on selection
# - No workers → no-op

STATE_DIR="$HOME/.local/state/claude-worker"

# Build list of workers
entries=()
ids=()
for f in "$STATE_DIR"/*.json; do
    [[ -f "$f" ]] || continue
    id=$(basename "$f" .json)
    read -r status dir task < <(python3 -c "
import json
d = json.load(open('$f'))
s = d.get('status','?')
dr = d.get('directory','').replace('$HOME','~')
t = d.get('task','')[:60]
print(f'{s} {dr} {t}')
")
    icon=""
    case "$status" in
        pending) icon="⏳" ;;
        working) icon="⚒️" ;;
        done)    icon="✅" ;;
    esac
    entries+=("$icon $status | $dir | $task")
    ids+=("$id:$status")
done

if [[ ${#entries[@]} -eq 0 ]]; then
    exit 0
fi

# Show fuzzel picker
selected=$(printf '%s\n' "${entries[@]}" | fuzzel --dmenu --prompt="Workers: " --width=60)
[[ -z "$selected" ]] && exit 0

# Find which entry was selected (by index)
for i in "${!entries[@]}"; do
    if [[ "${entries[$i]}" == "$selected" ]]; then
        IFS=: read -r sel_id sel_status <<< "${ids[$i]}"
        break
    fi
done

case "$sel_status" in
    pending)
        claude-worker-approve "$sel_id"
        ;;
    working|done)
        # Focus the worker's foot window
        window_addr=$(hyprctl clients -j | jq -r \
            ".[] | select(.class == \"claude-worker-$sel_id\") | .address")
        if [[ -n "$window_addr" ]]; then
            ws=$(hyprctl clients -j | jq -r \
                ".[] | select(.class == \"claude-worker-$sel_id\") | .workspace.name")
            if [[ "$ws" == special:* ]]; then
                hyprctl dispatch movetoworkspace e+0,address:"$window_addr"
            fi
            hyprctl dispatch focuswindow address:"$window_addr"
        fi
        ;;
esac
```

**Step 2: Make executable**

Run: `chmod +x .local/bin/claude-worker-list`

**Step 3: Test with a fake pending worker**

```bash
echo '{"status":"pending","directory":"/home/ifit/code/foo","task":"Test task"}' > ~/.local/state/claude-worker/77777.json
claude-worker-list
```

Verify fuzzel appears with the pending worker listed.

**Step 4: Commit**

```bash
git add .local/bin/claude-worker-list
git commit -m "feat: add claude-worker-list fuzzel menu for waybar left-click"
```

---

### Task 8: `claude-voice-cycle` Script (Middle-Click)

**Files:**
- Create: `.local/bin/claude-voice-cycle`

**Step 1: Write the script**

Cycles through: wake only → speak only → both → neither.

```bash
#!/bin/bash
# Cycle voice modes: wake only → speak only → both on → both off
# Middle-click waybar action.

STATE_FILE="$HOME/.local/state/claude-ask/waybar.json"
CONFIG_FILE="$HOME/.config/claude-ask/config.toml"
VOICE_SOCK="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/claude-voice.sock"
ASK_SOCK="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/claude-ask.sock"

if [[ ! -f "$STATE_FILE" ]]; then
    exit 1
fi

# If currently speaking, stop TTS and exit
status=$(python3 -c "import json; print(json.load(open('$STATE_FILE')).get('status',''))")
if [[ "$status" == "speaking" ]]; then
    python3 -c "
import socket, json
sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
sock.connect('$ASK_SOCK')
sock.sendall(json.dumps({'action': 'stop_tts'}).encode())
sock.close()
"
    pkill -SIGRTMIN+12 waybar 2>/dev/null
    exit 0
fi

# Read current state
read -r speak_on wake_muted < <(python3 -c "
import json
d = json.load(open('$STATE_FILE'))
speak = d.get('speak_enabled', False)
muted = d.get('wake_muted', True)
# wake_muted=True means wake is OFF, False means wake is ON
print(f'{speak} {muted}')
")

# Determine current mode and cycle to next
# Modes: wake_only (wake=on, speak=off) → speak_only (wake=off, speak=on)
#       → both (wake=on, speak=on) → neither (wake=off, speak=off)
# wake ON = wake_muted=False, wake OFF = wake_muted=True

if [[ "$wake_muted" == "False" && "$speak_on" == "False" ]]; then
    # wake only → speak only
    new_wake_muted=True
    new_speak=true
    mode="Speak only"
elif [[ "$wake_muted" == "True" && "$speak_on" == "True" ]]; then
    # speak only → both
    new_wake_muted=False
    new_speak=true
    mode="Wake + Speak"
elif [[ "$wake_muted" == "False" && "$speak_on" == "True" ]]; then
    # both → neither
    new_wake_muted=True
    new_speak=false
    mode="Voice off"
else
    # neither → wake only
    new_wake_muted=False
    new_speak=false
    mode="Wake only"
fi

# Update speak_enabled in state file and config
python3 -c "
import json
path = '$STATE_FILE'
with open(path) as f:
    d = json.load(f)
d['speak_enabled'] = $new_speak
d['wake_muted'] = $([[ "$new_wake_muted" == "True" ]] && echo "True" || echo "False")
with open(path, 'w') as f:
    json.dump(d, f, indent=2)
"

# Persist speak to config.toml
python3 -c "
path = '$CONFIG_FILE'
try:
    with open(path) as f:
        content = f.read()
    target = 'enabled = true' if not $new_speak else 'enabled = false'
    replacement = 'enabled = $([[ "$new_speak" == "true" ]] && echo "true" || echo "false")'
    if 'enabled = true' in content and not $new_speak:
        content = content.replace('enabled = true', 'enabled = false', 1)
    elif 'enabled = false' in content and $new_speak:
        content = content.replace('enabled = false', 'enabled = true', 1)
    with open(path, 'w') as f:
        f.write(content)
except FileNotFoundError:
    pass
"

# Toggle wake mute via claude-voice daemon if socket exists
if [[ -S "$VOICE_SOCK" ]]; then
    # Check current mute state and toggle if needed
    response=$(python3 -c "
import socket, json
sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
sock.connect('$VOICE_SOCK')
sock.sendall(json.dumps({'action': 'toggle-mute'}).encode())
sock.shutdown(socket.SHUT_WR)
data = sock.recv(4096)
sock.close()
print(data.decode())
" 2>/dev/null)
    current_muted=$(echo "$response" | python3 -c "import json,sys; print(json.load(sys.stdin).get('muted', False))" 2>/dev/null)
    # If the toggle put us in the wrong state, toggle again
    if [[ "$current_muted" != "$new_wake_muted" ]]; then
        python3 -c "
import socket, json
sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
sock.connect('$VOICE_SOCK')
sock.sendall(json.dumps({'action': 'toggle-mute'}).encode())
sock.shutdown(socket.SHUT_WR)
sock.recv(4096)
sock.close()
" 2>/dev/null
    fi
fi

pkill -SIGRTMIN+12 waybar 2>/dev/null
```

**Step 2: Make executable**

Run: `chmod +x .local/bin/claude-voice-cycle`

**Step 3: Test by clicking middle button multiple times**

Verify waybar tooltip shows the voice state cycling through all four modes.

**Step 4: Commit**

```bash
git add .local/bin/claude-voice-cycle
git commit -m "feat: add claude-voice-cycle for 4-state voice mode toggle"
```

---

### Task 9: Waybar Status — Pending Worker Support

**Files:**
- Modify: `.local/bin/claude-ask-status`

**Step 1: Update the Python block in claude-ask-status**

Add `worker_pending` count alongside `worker_working` and `worker_done`. Add a new CSS class `worker-pending` and update the shovel display logic.

In the Python block, after the existing worker counting (around the `worker_working`/`worker_done` lines), add:

```python
worker_pending = sum(1 for w in workers if w.get('status') == 'pending')
worker_total = worker_pending + worker_working + worker_done
worker_state = 'working' if worker_working > 0 else ('done' if worker_done > 0 else ('pending' if worker_pending > 0 else None))
```

Update the tooltip section for workers:

```python
if worker_state:
    parts = []
    if worker_pending: parts.append(f'{worker_pending} pending')
    if worker_working: parts.append(f'{worker_working} working')
    if worker_done: parts.append(f'{worker_done} done')
    tip += ' | Workers: ' + ', '.join(parts)
```

For the shovel color, add pending state — use a dimmer color (e.g., `#585b70` overlay/surface2):

```python
DIM = '#585b70'
# In the shovel_suffix building:
if worker_state == 'pending':
    shovel_color = DIM
elif worker_state == 'working':
    shovel_color = YELLOW
else:
    shovel_color = GREEN
```

Add `worker-pending` CSS class:

```python
elif worker_state == 'pending':
    css = 'worker-pending'
```

**Step 2: Add CSS for pending workers**

In `.config/waybar/style.css`, add after the `worker-done` block:

```css
#custom-claude-ask.worker-pending {
    color: #585b70;
}
```

**Step 3: Test**

Create a fake pending worker, verify waybar shows dim shovel.

**Step 4: Commit**

```bash
git add .local/bin/claude-ask-status .config/waybar/style.css
git commit -m "feat: add pending worker state to waybar status display"
```

---

### Task 10: Stow and Integration Test

**Files:**
- All new files created in tasks 1-9

**Step 1: Stow new files**

```bash
cd /home/ifit/dotfiles && stow .
```

**Step 2: Restart the daemon**

```bash
systemctl --user restart claude-ask
```

**Step 3: End-to-end test**

1. Open claude-ask (`Super+Shift+G`)
2. Type: "Refactor the error handling in ai-media-svc to use a centralized error class across all route handlers"
3. Verify Claude calls `spawn_worker` with the right directory and a detailed task
4. Verify notification appears with Start/Decline
5. Verify waybar shows dim shovel (pending)
6. Click "Start" — verify foot terminal launches with claude running
7. Verify waybar shovel turns yellow (working)
8. When done, verify notification "Worker Done" appears
9. Verify waybar shovel turns green (done)

**Step 4: Test manage_workers via conversation**

1. Queue another pending worker via claude-ask
2. Start a new conversation: "what workers do I have?"
3. Verify Claude calls `manage_workers(action="list")`
4. Reply: "approve it"
5. Verify Claude calls `manage_workers(action="approve")`

**Step 5: Test waybar clicks**

1. Left-click waybar module → fuzzel list appears
2. Middle-click → voice mode cycles
3. Right-click → new worker flow

**Step 6: Final commit if any fixes needed**

```bash
git add -A && git commit -m "fix: integration test fixes for worker spawn"
```
