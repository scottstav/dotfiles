"""List, approve, or cancel Claude Code workers via ccl."""

import json
import subprocess
from pathlib import Path

TOOL_SPEC = {
    "name": "manage_workers",
    "description": (
        "List all Claude Code workers (pending, working, done), approve pending workers "
        "to start them, or cancel pending workers. Use when the user asks about worker "
        "status, wants to approve or cancel a queued worker, or says something like "
        "'go ahead with that worker' or 'start the pending worker'."
    ),
    "parameters": {
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
                "description": "Worker ID (required for approve/cancel).",
            },
        },
        "required": ["action"],
    },
}


def _list_workers():
    """Get all workers from ccl."""
    result = subprocess.run(
        ["ccl", "list", "--json"],
        capture_output=True, text=True,
    )
    if result.returncode != 0 or not result.stdout.strip() or result.stdout.strip() == "No workers.":
        return []
    try:
        return json.loads(result.stdout.strip())
    except json.JSONDecodeError:
        return []


def run(action: str, worker_id: str = None) -> str:
    if action == "list":
        workers = _list_workers()
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
        workers = _list_workers()
        pending = [w for w in workers if w["status"] == "pending"]
        if len(pending) == 1:
            worker_id = pending[0]["id"]
        else:
            return "Multiple pending workers. Specify worker_id. Use action=list to see them."

    if action == "approve":
        result = subprocess.run(
            ["ccl", "approve", worker_id],
            capture_output=True, text=True,
        )
        if result.returncode != 0:
            return f"Error: {result.stderr.strip()}"
        return result.stdout.strip() or f"Worker {worker_id} approved."

    if action == "cancel":
        result = subprocess.run(
            ["ccl", "deny", worker_id],
            capture_output=True, text=True,
        )
        if result.returncode != 0:
            return f"Error: {result.stderr.strip()}"
        return result.stdout.strip() or f"Worker {worker_id} cancelled."

    return f"Unknown action: {action}"
