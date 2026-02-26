"""Persistent memory — save and recall information across conversations."""

import re
import subprocess
from datetime import datetime
from pathlib import Path

name = "memory"

description = """\
Save and recall information across conversations. Use this proactively whenever \
the user mentions plans, intentions, ideas, preferences, decisions, or anything \
they might want to remember later — even if they don't explicitly ask you to \
remember it.

Actions:
- save: Store a memory. Write a clear, self-contained summary (not the user's \
  raw words). Include enough context that it makes sense on its own later.
- search: Find memories by keyword. Returns matching entries with context.
- recent: Show the most recent memories (default 10).
- delete: Remove a memory by its exact timestamp prefix (YYYY-MM-DD HH:MM)."""

input_schema = {
    "type": "object",
    "properties": {
        "action": {
            "type": "string",
            "enum": ["save", "search", "recent", "delete"],
            "description": "What to do: save a new memory, search existing ones, show recent, or delete one.",
        },
        "content": {
            "type": "string",
            "description": "For 'save': the memory to store. Write a clear summary, not raw user words.",
        },
        "query": {
            "type": "string",
            "description": "For 'search': keywords to find in memories.",
        },
        "count": {
            "type": "integer",
            "description": "For 'recent': how many entries to show (default 10).",
        },
        "timestamp": {
            "type": "string",
            "description": "For 'delete': the timestamp prefix of the entry to remove (e.g. '2026-02-26 14:30').",
        },
    },
    "required": ["action"],
}

MEMORY_FILE = Path.home() / ".local" / "state" / "claude-ask" / "memory.md"


def _ensure_file():
    MEMORY_FILE.parent.mkdir(parents=True, exist_ok=True)
    if not MEMORY_FILE.exists():
        MEMORY_FILE.write_text("# Claude Memory\n\n")


def _save(content: str) -> str:
    _ensure_file()
    ts = datetime.now().strftime("%Y-%m-%d %H:%M")
    entry = f"- **{ts}** — {content}\n"
    with open(MEMORY_FILE, "a") as f:
        f.write(entry)
    return f"Saved to memory."


def _search(query: str) -> str:
    _ensure_file()
    text = MEMORY_FILE.read_text()
    lines = text.splitlines()

    terms = query.lower().split()
    matches = []
    for line in lines:
        if line.startswith("- **"):
            lower = line.lower()
            score = sum(1 for t in terms if t in lower)
            if score > 0:
                matches.append((score, line))

    if not matches:
        return f"No memories found matching: {query}"

    matches.sort(key=lambda x: x[0], reverse=True)
    results = [line for _, line in matches[:20]]
    return f"Found {len(matches)} matching memories:\n\n" + "\n".join(results)


def _recent(count: int = 10) -> str:
    _ensure_file()
    text = MEMORY_FILE.read_text()
    entries = [l for l in text.splitlines() if l.startswith("- **")]
    if not entries:
        return "No memories saved yet."
    recent = entries[-count:]
    return f"Last {len(recent)} memories:\n\n" + "\n".join(recent)


def _delete(timestamp: str) -> str:
    _ensure_file()
    text = MEMORY_FILE.read_text()
    lines = text.splitlines()
    new_lines = []
    removed = 0
    for line in lines:
        if line.startswith("- **") and timestamp in line:
            removed += 1
            continue
        new_lines.append(line)

    if removed == 0:
        return f"No memory found with timestamp '{timestamp}'."

    MEMORY_FILE.write_text("\n".join(new_lines) + "\n")
    return f"Removed {removed} memory entry."


def run(input_data: dict) -> str:
    action = input_data.get("action", "recent")

    if action == "save":
        content = input_data.get("content", "").strip()
        if not content:
            return "Error: 'content' is required for save."
        return _save(content)

    elif action == "search":
        query = input_data.get("query", "").strip()
        if not query:
            return "Error: 'query' is required for search."
        return _search(query)

    elif action == "recent":
        count = input_data.get("count", 10)
        return _recent(count)

    elif action == "delete":
        timestamp = input_data.get("timestamp", "").strip()
        if not timestamp:
            return "Error: 'timestamp' is required for delete."
        return _delete(timestamp)

    return f"Unknown action: {action}"
