"""List, approve, or cancel Claude Code workers."""

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
            return "Multiple pending workers. Specify worker_id. Use action=list to see them."

    worker = _find_worker(worker_id, workers)
    if not worker:
        return f"No worker found matching '{worker_id}'."

    if action == "approve":
        if worker["status"] != "pending":
            return f"Worker {worker['id']} is {worker['status']}, not pending."
        result = subprocess.run(
            ["claude-worker-manage", worker["id"]],
            capture_output=True, text=True,
        )
        return result.stdout.strip() or f"Worker {worker['id']} approved."

    if action == "cancel":
        if worker["status"] != "pending":
            return f"Worker {worker['id']} is {worker['status']}, not pending."
        result = subprocess.run(
            ["claude-worker-manage", "--deny", worker["id"]],
            capture_output=True, text=True,
        )
        return result.stdout.strip() or f"Worker {worker['id']} cancelled."

    return f"Unknown action: {action}"
