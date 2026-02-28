"""Search past conversation transcripts to recall things from previous sessions."""

import re
import subprocess
from collections import defaultdict
from datetime import datetime, timedelta
from pathlib import Path

TOOL_SPEC = {
    "name": "search_conversations",
    "description": (
        "Search past voice assistant conversations to find things discussed in previous sessions. "
        "Use this whenever the user refers to a past conversation or something previously discussed "
        "that isn't in the current context — e.g. \"what were those mods you recommended?\", "
        "\"remember that recipe?\", \"what did we talk about yesterday?\"\n\n"
        "Provide search terms relevant to the topic. Include synonyms and related words to cast a "
        "wide net. Recent conversations are prioritized automatically — only older history is "
        "searched if nothing recent matches."
    ),
    "parameters": {
        "type": "object",
        "properties": {
            "terms": {
                "type": "array",
                "items": {"type": "string"},
                "description": "Search terms — keywords, synonyms, related words, alternate spellings. Case-insensitive.",
            },
        },
        "required": ["terms"],
    },
}

CHATS_DIR = Path.home() / "Dropbox" / "LLM" / "Chats"
MAX_RESULTS = 5
MAX_OUTPUT_CHARS = 6000
MAX_CONV_CHARS = 2000
DATE_RE = re.compile(r"^(\d{4}-\d{2}-\d{2})-")
TURN_RE = re.compile(r"^(# \d+\. (?:User|Claude))$", re.MULTILINE)


def _file_date(path):
    """Extract date from filename like 2026-02-26-slug.md."""
    m = DATE_RE.match(path.name)
    if m:
        try:
            return datetime.strptime(m.group(1), "%Y-%m-%d").date()
        except ValueError:
            pass
    return None


def _search_all(terms):
    """Ripgrep files-with-matches for each term across all transcripts."""
    file_terms = defaultdict(set)
    for term in terms:
        cmd = [
            "rg", "--ignore-case", "--files-with-matches",
            "--glob", "*.md", "--", term, str(CHATS_DIR),
        ]
        result = subprocess.run(cmd, capture_output=True, text=True, timeout=10)
        for line in result.stdout.strip().splitlines():
            if line:
                file_terms[Path(line)].add(term)
    return file_terms


def _narrow_by_recency(file_terms):
    """Return matches from the narrowest time window that has results."""
    if not file_terms:
        return {}

    today = datetime.now().date()
    tiers = [3, 7, 30, None]

    for days in tiers:
        if days is None:
            return file_terms
        cutoff = today - timedelta(days=days)
        filtered = {
            path: matched for path, matched in file_terms.items()
            if (_file_date(path) or datetime.min.date()) >= cutoff
        }
        if filtered:
            return filtered

    return file_terms


def _extract_relevant_turns(text, terms):
    """From a long conversation, extract turns containing search terms plus context."""
    parts = TURN_RE.split(text)

    # Build (header, body) pairs
    pairs = []
    i = 1
    while i < len(parts) - 1:
        pairs.append((parts[i], parts[i + 1].strip()))
        i += 2

    if not pairs:
        return text[:MAX_CONV_CHARS]

    # Find turns containing any term
    lower_terms = [t.lower() for t in terms]
    matching_indices = set()
    for j, (header, body) in enumerate(pairs):
        if any(t in body.lower() or t in header.lower() for t in lower_terms):
            matching_indices.add(j)

    # Include neighboring turns for context (question-answer pairs)
    expanded = set()
    for j in matching_indices:
        if j > 0:
            expanded.add(j - 1)
        expanded.add(j)
        if j + 1 < len(pairs):
            expanded.add(j + 1)

    relevant = []
    for j in sorted(expanded):
        header, body = pairs[j]
        relevant.append(f"{header}\n\n{body}")

    result = "\n\n".join(relevant)
    if len(result) > MAX_CONV_CHARS:
        result = result[:MAX_CONV_CHARS] + "\n[...truncated]"
    return result


def run(terms: list) -> str:
    if not terms:
        return "Please provide at least one search term."

    if not CHATS_DIR.is_dir():
        return f"Chat directory not found: {CHATS_DIR}"

    # Search all files, then narrow by recency
    all_matches = _search_all(terms)
    file_terms = _narrow_by_recency(all_matches)

    if not file_terms:
        return f"No past conversations found matching: {', '.join(terms)}"

    # Rank by (terms matched, date) — most relevant + most recent first
    ranked = sorted(
        file_terms.items(),
        key=lambda x: (len(x[1]), _file_date(x[0]) or datetime.min.date()),
        reverse=True,
    )[:MAX_RESULTS]

    output = []
    total = len(file_terms)
    shown = min(len(ranked), MAX_RESULTS)
    output.append(f"Found {total} conversation(s), showing top {shown}:\n")

    for path, matched in ranked:
        date = _file_date(path)
        date_str = date.strftime("%Y-%m-%d") if date else "unknown"
        slug = path.stem[11:] if len(path.stem) > 10 else path.stem

        try:
            text = path.read_text(errors="replace")
        except OSError:
            continue

        output.append(f"## {date_str} — {slug}")
        output.append(f"Matched: {', '.join(sorted(matched))}")

        if len(text) > MAX_CONV_CHARS:
            text = _extract_relevant_turns(text, terms)

        output.append(text)
        output.append("")

    result = "\n".join(output)
    if len(result) > MAX_OUTPUT_CHARS:
        result = result[:MAX_OUTPUT_CHARS] + "\n\n[...truncated]"
    return result
