# wreccless (ccl) — Claude Code Launcher

**Date:** 2026-02-27
**Repo:** ~/projects/wreccless
**Binary:** ccl

## Overview

Standalone Go CLI that manages background Claude Code worker sessions. Handles launching, tracking, killing, resuming, and querying workers. UI concerns (waybar, notifications, fzf pickers, future TUI) live outside ccl — they consume ccl's output and hook system.

## Core Decisions

- **Language:** Go — single compiled binary, no runtime deps
- **State storage:** File-per-worker JSON in `~/.local/state/ccl/`
- **Process model:** `ccl new` spawns detached `ccl run <id>` process. No systemd. PID tracked in state file.
- **Claude flags:** `--output-format stream-json` and `--verbose` are hardcoded (ccl needs them). `skip_permissions` is configurable (default: on).
- **Output:** Human-readable by default, `--json` flag for machine consumption.
- **Auto-start default:** `ccl new` launches immediately. `--pending` defers to manual approval.
- **Hooks:** Configurable shell commands fired on state transitions. ccl doesn't know about waybar/notifications — hooks handle that.
- **Migration:** Coexist with existing bash scripts. New state directory (`~/.local/state/ccl/`). Old scripts keep working on `~/.local/state/claude-worker/`.

## CLI Reference

```
ccl new --dir <path> --task <text> [--pending] [--image <path>]
    Create and start a worker. Returns worker ID to stdout.
    --pending: defer start, require ccl approve
    --image: attach an image path for claude to reference

ccl list [--json] [--status pending|working|done|error]
    List all workers. Detects stale workers (dead PID → error).

ccl status <id> [--json]
    Detailed info for one worker.

ccl logs <id> [--follow] [--json]
    Show log output. --follow tails live.
    Default: human-readable (parsed from stream-json NDJSON).
    --json: raw NDJSON events.

ccl approve <id>
    Start a pending worker.

ccl deny <id>
    Remove a pending worker.

ccl kill <id>
    SIGTERM the claude process. Mark state as error.

ccl resume <id>
    exec into claude --resume <session_id> in worker's directory.
    Takes over the current terminal.

ccl clean [--all]
    Remove done/error workers and logs.
    --all: remove everything including working.
```

## Project Structure

```
~/projects/wreccless/
├── cmd/ccl/main.go          # Entry point, subcommand routing
├── internal/
│   ├── worker/              # Worker lifecycle (create, run, kill)
│   ├── state/               # State file read/write/scan
│   ├── hooks/               # Hook execution engine
│   └── config/              # Config loading (TOML)
├── config.example.toml
├── go.mod
└── Makefile                 # build, install (to ~/.local/bin)
```

## State File Schema

Path: `~/.local/state/ccl/<id>.json`

```json
{
  "id": "1740678000",
  "status": "working",
  "directory": "/home/ifit/projects/foo",
  "task": "Fix the authentication bug",
  "image": null,
  "pid": 12345,
  "session_id": "a1b2c3d4-...",
  "created_at": "2026-02-27T15:00:00Z",
  "started_at": "2026-02-27T15:00:01Z",
  "finished_at": null
}
```

State transitions:
- `ccl new` → `working` (or `pending` with `--pending`)
- `ccl approve` → `pending` → `working`
- `ccl run` completes → `working` → `done`
- `ccl run` fails → `working` → `error`
- `ccl kill` / `ccl deny` → removes state file
- `ccl clean` → removes `done`/`error` state files
- Stale detection: `ccl list` checks if PID is alive; dead PID → `error`

## Configuration

Path: `~/.config/ccl/config.toml`

```toml
[claude]
# Hardcoded internally: --output-format stream-json --verbose --session-id <uuid>
# skip_permissions: pass --dangerously-skip-permissions (default: true)
skip_permissions = true

# System prompt appended for autonomous workers
system_prompt = """You are the user's trusted programmer. Do not ask questions. \
Complete the entire task before stopping. If you encounter issues, debug and fix them. \
When finished, end with a 1-2 sentence summary."""

# Additional flags to pass to claude -p
extra_flags = []

[hooks]
# Shell commands executed on state transitions via sh -c
# Template variables: {{.ID}}, {{.Task}}, {{.Dir}}, {{.Status}}, {{.SessionID}}
# Hooks fire asynchronously and don't block state transitions.
on_start = ["pkill -SIGRTMIN+12 waybar"]
on_done = [
    "pkill -SIGRTMIN+12 waybar",
    "notify-send -t 0 -a 'ccl' 'Worker Done ({{.Dir}})' '{{.Task}}'"
]
on_pending = ["pkill -SIGRTMIN+12 waybar"]
on_error = [
    "pkill -SIGRTMIN+12 waybar",
    "notify-send -u critical -a 'ccl' 'Worker Failed' '{{.Task}}'"
]
on_kill = ["pkill -SIGRTMIN+12 waybar"]
```

## Process Model

1. `ccl new --dir /foo --task "fix bug"` writes state file, spawns detached `ccl run <id>` (Go: `exec.Command` with `Setsid: true`), exits immediately with ID on stdout
2. `ccl run <id>` reads state, `cd`s to directory, launches `claude -p` with hardcoded + configured flags, pipes stdout to log file
3. `cmd.Wait()` blocks until claude exits
4. On exit: update state to `done` or `error`, fire hooks
5. `ccl run` exits (cleanup trap removes state only on abnormal exit while still `working`)

## Log Streaming

`claude -p --output-format stream-json --verbose` writes NDJSON events to `~/.local/state/ccl/<id>.log`:

```json
{"type":"system","subtype":"init",...}
{"type":"assistant","subtype":"text","content":"Let me look at..."}
{"type":"tool_use","name":"Read","input":{...}}
{"type":"result",...}
```

`ccl logs <id>` parses these into human-readable output. `ccl logs <id> --follow` tails the file live. `ccl logs <id> --json` outputs raw NDJSON.

## Migration Plan

1. Build ccl with new state dir (`~/.local/state/ccl/`)
2. Existing bash scripts keep working on `~/.local/state/claude-worker/`
3. Update bash wrappers to call ccl (e.g. `claude-worker-list` → fzf around `ccl list --json`)
4. Update claude-ask Python tools to call ccl
5. Remove old bash logic, keeping only thin UI wrappers
