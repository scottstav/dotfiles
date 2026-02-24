# Claude Ask → Worker Spawn Integration

## Problem

Claude Ask is great for quick questions but can't handle complex coding tasks
that require multi-file edits, codebase exploration, or significant reasoning.
Claude Worker sessions can, but they're manually launched. There's no bridge
between "I asked Claude a question" and "this needs a full Claude Code session."

## Solution

Add a `spawn_worker` tool to claude-ask's tool system so Claude can queue
pending worker sessions. Workers are approved asynchronously — via notification
buttons, natural conversation replies, or the waybar worker list. Approval and
denial flow through a single `claude-worker-manage` script.

## Architecture

### State Model

All workers (pending, working, done) live as JSON files in
`~/.local/state/claude-worker/`:

```json
{
  "status": "pending",
  "directory": "/home/ifit/code/ai-media-svc",
  "task": "Fix the authentication bug in the login handler",
  "created": "2026-02-24T14:30:00Z"
}
```

Status transitions: `pending → working → done` (or `pending → cancelled`).

### New Components

#### `spawn_worker` tool (`~/.local/share/claude-ask/tools/spawn_worker.py`)

Claude calls this when it recognizes a task that needs a full Claude Code
session. The tool:

1. Generates a unique worker ID
2. Writes a pending state file
3. Fires a non-blocking notification with `[Start]` and `[Decline]` actions
4. Signals waybar to refresh
5. Returns immediately: `"Worker queued as pending (id: <id>). User will approve when ready."`

The notification action handlers call `claude-worker-manage <id>` or
`claude-worker-manage --deny <id>`.

**When Claude should use this tool** (system prompt guidance):
- Changes across multiple files
- Understanding significant codebase context to make even a single change
  (tracing call chains, reading tests, understanding architecture)
- Implementing features, refactoring, or debugging that requires exploration

**When Claude should NOT use this tool:**
- Simple file edits (journal entries, config changes, adding a line)
- Running a command and reporting output
- Anything the shell tool can handle

#### `manage_workers` tool (`~/.local/share/claude-ask/tools/manage_workers.py`)

Lists, approves, or cancels workers. Claude uses this when users say things like
"approve that worker", "what workers are running?", or "cancel the pending one."

```python
input_schema = {
    "type": "object",
    "properties": {
        "action": {
            "type": "string",
            "enum": ["list", "approve", "cancel"],
            "description": "list: show all workers. approve: start a pending worker. cancel: remove a pending worker."
        },
        "worker_id": {
            "type": "string",
            "description": "Worker ID or partial match (for approve/cancel)."
        }
    },
    "required": ["action"]
}
```

Internally calls `claude-worker-manage` for approve/cancel actions.

#### `claude-worker-manage` script (`~/.local/bin/claude-worker-manage`)

Single entry point for approving or denying pending workers:

```
claude-worker-manage <id>          # approve: launch worker session
claude-worker-manage --deny <id>   # deny: remove pending state file
```

On approval:
1. Reads the pending state file
2. Launches: `foot --app-id=claude-worker-<id> -- claude-worker-session <id> <dir> <task>`
3. State transitions to `working` when `claude-worker-session` starts

On denial:
1. Removes the state file
2. Signals waybar

### Modified Components

#### `daemon.py` — System prompt update

Add worker guidance to `SYSTEM_PROMPT`:

```
You can delegate complex coding tasks to a Claude Code worker session. Workers are
autonomous agents running in their own terminal with full codebase context, file
editing, git, and multi-file reasoning. Use spawn_worker when the task requires:
- Changes across multiple files
- Understanding significant codebase context to make even a single change
- Implementing a feature, refactoring, or debugging that requires exploration

Do NOT use spawn_worker for simple tasks you can handle with the shell tool:
quick file edits, adding a journal entry, changing a config value, running a command.
If in doubt, just answer — the user can always ask for a worker explicitly.

When using spawn_worker, infer the project directory from context. Common locations:
~/code/, ~/dotfiles/, ~/Dropbox/. Use the shell tool to search if unsure.
Write a thorough, detailed task description — the worker operates autonomously.
```

#### `claude-worker-session` — Completion notification

Add `notify-send` when a worker finishes:

```bash
notify-send -t 0 -a "Claude Worker" \
    -A "focus=Focus" -A "dismiss=OK" \
    --wait \
    "Worker Done" "$(echo "$task" | head -c 200)"
```

"Focus" action brings the worker's foot window to the foreground.

#### `claude-ask-status` — Waybar module updates

- Pending workers show as dim/lavender shovels (new state)
- Tooltip: "Workers: 1 pending, 1 working"
- Left click: worker list/management (fuzzel menu of workers)
- Middle click: cycle voice modes (wake only → speak only → both → neither)
- Right click: spawn new worker interactively

#### `claude-worker-toggle` — Worker list on left-click

When workers exist, show a fuzzel menu listing each worker by task snippet and
status. Selecting a worker:
- Pending → approve it
- Working → focus its window
- Done → focus its window

When no workers exist, no-op (right-click spawns new workers).

#### `claude-ask-toggle-speak` — Voice mode cycler

Rewrite to cycle through four states:
1. Wake on, speak off
2. Wake off, speak on
3. Both on
4. Both off

Updates config.toml, waybar state, and signals both daemons.

## Data Flow

```
User: "Fix the auth bug in ai-media-svc"
    ↓
Claude (daemon): recognizes coding task → calls spawn_worker tool
    ↓
spawn_worker.py:
    → writes <id>.json {status: pending, dir, task}
    → fires notification [Start] [Decline]
    → signals waybar (dim shovel appears)
    → returns: "Worker queued as pending"
    ↓
Claude: "I've queued a worker for that. Approve it when ready."
    ↓
Approval (any path):
    a) Notification [Start] → claude-worker-manage <id>
    b) User replies "go ahead" → Claude calls manage_workers(approve)
    c) Waybar left-click → fuzzel → select pending worker
    d) Hours later: "start that worker" → Claude lists + approves
    ↓
claude-worker-manage <id>:
    → reads pending state
    → launches foot + claude-worker-session
    → state: pending → working
    ↓
Worker runs autonomously in foot terminal
    ↓
Worker completes:
    → state: working → done
    → notify-send "Worker Done" with [Focus] [OK]
    → waybar: shovel turns green
```

## Files Summary

### New
| File | Purpose |
|------|---------|
| `.local/share/claude-ask/tools/spawn_worker.py` | Tool: create pending workers |
| `.local/share/claude-ask/tools/manage_workers.py` | Tool: list/approve/cancel workers |
| `.local/bin/claude-worker-manage` | Script: approve or deny pending workers |

### Modified
| File | Change |
|------|--------|
| `.local/share/claude-ask/daemon.py` | System prompt: add worker guidance |
| `.local/bin/claude-worker-session` | Add completion notification |
| `.local/bin/claude-ask-status` | Pending worker state, updated tooltip |
| `.config/waybar/config` | Remap clicks: left=workers, middle=voice, right=new worker |
| `.local/bin/claude-ask-toggle-speak` | Rewrite as 4-state voice mode cycler |
| `.local/bin/claude-worker-toggle` | Show fuzzel worker list when workers exist |
