"""Queue a pending Claude Code worker session."""

import json
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
