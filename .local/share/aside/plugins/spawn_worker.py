"""Queue a pending Claude Code worker session via ccl."""

import json
import subprocess
from pathlib import Path

TOOL_SPEC = {
    "name": "spawn_worker",
    "description": (
        "Queue a pending Claude Code worker session for complex coding tasks. "
        "The worker runs autonomously in its own terminal with full codebase access, "
        "file editing, git, and multi-file reasoning. Use this when the task requires "
        "changes across multiple files or deep codebase understanding. "
        "The user must approve the worker before it starts."
    ),
    "parameters": {
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
    },
}


def run(directory: str, task: str) -> str:
    if not Path(directory).is_dir():
        return f"Error: directory {directory} does not exist."

    result = subprocess.run(
        ["ccl", "new", "--pending", "--json", "--dir", directory, "--task", task],
        capture_output=True, text=True,
    )

    if result.returncode != 0:
        return f"Error creating worker: {result.stderr.strip()}"

    try:
        data = json.loads(result.stdout.strip())
        worker_id = data["id"]
    except (json.JSONDecodeError, KeyError):
        return f"Worker created but couldn't parse output: {result.stdout.strip()}"

    dir_short = directory.replace(str(Path.home()), "~")
    return (
        f"Worker queued as pending (id: {worker_id}).\n"
        f"Directory: {dir_short}\n"
        f"The user will see a notification to approve or decline."
    )
