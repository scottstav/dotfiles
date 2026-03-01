"""Queue a pending Claude Code worker session via ccl."""

import difflib
import json
import re
import subprocess
from pathlib import Path

TOOL_SPEC = {
    "name": "spawn_worker",
    "description": (
        "Queue a pending Claude Code worker session for complex coding tasks. "
        "The worker runs autonomously in its own terminal with full codebase access, "
        "file editing, git, and multi-file reasoning. Use this when the task requires "
        "changes across multiple files or deep codebase understanding. "
        "The user must approve the worker before it starts.\n\n"
        "For 'directory': pass the project name as the user said it — does NOT need "
        "to be an absolute path. Fuzzy matching resolves names like 'dotfiles', "
        "'dot files', 'aside', 'wreccless' to actual repos under ~/projects, ~, "
        "or ~/Dropbox. Absolute paths still work too."
    ),
    "parameters": {
        "type": "object",
        "properties": {
            "directory": {
                "type": "string",
                "description": (
                    "Project name or absolute path. Can be fuzzy — e.g. 'dotfiles', "
                    "'dot files', 'my aside project'. Resolved against git repos on disk."
                ),
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


def _normalize(name: str) -> str:
    """Lowercase, strip spaces/punctuation/filler words for comparison."""
    s = name.lower()
    s = re.sub(r"\b(my|the|project|repo|repository|codebase|directory|folder)\b", "", s)
    s = re.sub(r"[^a-z0-9]", "", s)
    return s


def _find_repos() -> list[Path]:
    """Discover git repos under common locations."""
    result = subprocess.run(
        ["fd", "-t", "d", "-H", "^.git$", "-d", "4", str(Path.home()), "--prune"],
        capture_output=True, text=True, timeout=5,
    )
    repos = []
    for line in result.stdout.strip().splitlines():
        repo_dir = Path(line).parent  # strip trailing .git/
        repos.append(repo_dir)
    return repos


def _resolve_directory(raw: str) -> tuple[str | None, str | None]:
    """Resolve a fuzzy directory name to an absolute path.

    Returns (resolved_path, error_message).
    """
    # Already a valid absolute path — use it directly
    p = Path(raw).expanduser()
    if p.is_absolute() and p.is_dir():
        return str(p), None

    repos = _find_repos()
    if not repos:
        return None, "Could not discover any git repos under ~."

    query = _normalize(raw)
    if not query:
        return None, f"Could not parse a project name from: {raw!r}"

    # Build mapping: normalized basename -> repo path
    candidates: dict[str, Path] = {}
    for repo in repos:
        candidates[_normalize(repo.name)] = repo

    # 1. Exact normalized match
    if query in candidates:
        return str(candidates[query]), None

    # 2. Substring match (query in candidate or candidate in query)
    substring_hits = [
        (name, path) for name, path in candidates.items()
        if query in name or name in query
    ]
    if len(substring_hits) == 1:
        return str(substring_hits[0][1]), None

    # 3. Fuzzy match via difflib
    names = list(candidates.keys())
    close = difflib.get_close_matches(query, names, n=3, cutoff=0.5)
    if len(close) == 1:
        return str(candidates[close[0]]), None
    if len(close) > 1:
        options = ", ".join(str(candidates[c]) for c in close)
        return None, f"Ambiguous directory {raw!r}. Did you mean one of: {options}"

    # 4. Nothing found
    available = ", ".join(sorted(r.name for r in repos))
    return None, (
        f"No repo matching {raw!r} found. Available repos: {available}"
    )


def run(directory: str, task: str) -> str:
    resolved, err = _resolve_directory(directory)
    if err:
        return f"Error: {err}"

    result = subprocess.run(
        ["ccl", "new", "--pending", "--json", "--dir", resolved, "--task", task],
        capture_output=True, text=True,
    )

    if result.returncode != 0:
        return f"Error creating worker: {result.stderr.strip()}"

    try:
        data = json.loads(result.stdout.strip())
        worker_id = data["id"]
    except (json.JSONDecodeError, KeyError):
        return f"Worker created but couldn't parse output: {result.stdout.strip()}"

    dir_short = resolved.replace(str(Path.home()), "~")
    return (
        f"Worker queued as pending (id: {worker_id}).\n"
        f"Directory: {dir_short}\n"
        f"The user will see a notification to approve or decline."
    )
