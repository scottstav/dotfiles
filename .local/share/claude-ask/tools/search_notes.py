"""Search denote notes for topics, people, concepts, etc."""

import re
import subprocess
from collections import defaultdict
from pathlib import Path

name = "search_notes"

description = """\
Search the user's denote notes (org-mode files in ~/Dropbox/org/denote/) for information \
about a topic. Provide search terms that are relevant to the user's question — include \
synonyms, related words, and alternate spellings to cast a wide net.

For example, if the user asks "anything about my dog Nori", you might search with \
terms ["nori", "dog", "pet", "puppy", "animal"].

Returns matching notes ranked by relevance (number of term hits) with titles, tags, \
and context snippets. Use the results to give the user a fuzzy summary — mention exact \
matches and also near-misses or loosely related notes."""

input_schema = {
    "type": "object",
    "properties": {
        "terms": {
            "type": "array",
            "items": {"type": "string"},
            "description": "Search terms to grep for. Include the literal query words plus synonyms, related concepts, alternate spellings, and partial matches. Case-insensitive.",
        },
    },
    "required": ["terms"],
}

NOTES_DIR = Path.home() / "Dropbox" / "org" / "denote"
MAX_SNIPPET_LINES = 3
MAX_RESULTS = 20
MAX_OUTPUT_CHARS = 6000


def _extract_metadata(filepath):
    """Extract title and tags from org-mode front matter."""
    title = filepath.stem
    tags = ""
    try:
        with open(filepath, "r", errors="replace") as f:
            for line in f:
                if line.startswith("#+title:"):
                    title = line.split(":", 1)[1].strip()
                elif line.startswith("#+filetags:"):
                    tags = line.split(":", 1)[1].strip()
                elif not line.startswith("#"):
                    break
    except OSError:
        pass
    return title, tags


def _search_term(term):
    """Run ripgrep for a single term, return {filepath: [matching lines]}."""
    cmd = [
        "rg",
        "--ignore-case",
        "--no-heading",
        "--line-number",
        "--max-count=5",
        "--glob=*.org",
        "--",
        term,
        str(NOTES_DIR),
    ]
    result = subprocess.run(cmd, capture_output=True, text=True, timeout=15)
    hits = defaultdict(list)
    for line in result.stdout.splitlines():
        # rg output: /path/file.org:LINE_NUM:content
        match = re.match(r"^(.+\.org):(\d+):(.*)$", line)
        if match:
            path, lineno, content = match.groups()
            hits[path].append((int(lineno), content.strip()))
    return hits


def run(input):
    terms = input.get("terms", [])
    if not terms:
        return "Please provide at least one search term."

    # Search for each term and aggregate results
    file_scores = defaultdict(lambda: {"terms_matched": set(), "snippets": []})

    for term in terms:
        hits = _search_term(term)
        for filepath, lines in hits.items():
            entry = file_scores[filepath]
            entry["terms_matched"].add(term)
            for lineno, content in lines:
                # Avoid duplicate snippets
                if len(entry["snippets"]) < MAX_SNIPPET_LINES * 2:
                    entry["snippets"].append((lineno, content, term))

    if not file_scores:
        return f"No notes found matching any of: {', '.join(terms)}"

    # Rank by number of distinct terms matched, then by total snippet count
    ranked = sorted(
        file_scores.items(),
        key=lambda x: (len(x[1]["terms_matched"]), len(x[1]["snippets"])),
        reverse=True,
    )[:MAX_RESULTS]

    # Format output
    output = []
    output.append(f"Found {len(file_scores)} note(s) matching across {len(ranked)} shown:\n")

    for filepath, data in ranked:
        p = Path(filepath)
        title, tags = _extract_metadata(p)
        terms_hit = sorted(data["terms_matched"])
        rel_path = str(p.relative_to(NOTES_DIR)) if str(p).startswith(str(NOTES_DIR)) else p.name

        output.append(f"## {title}")
        if tags:
            output.append(f"   Tags: {tags}")
        output.append(f"   File: {rel_path}")
        output.append(f"   Matched terms ({len(terms_hit)}): {', '.join(terms_hit)}")

        # Show unique snippets
        seen = set()
        for lineno, content, term in data["snippets"][:MAX_SNIPPET_LINES]:
            if content not in seen:
                seen.add(content)
                # Truncate long lines
                if len(content) > 200:
                    content = content[:200] + "..."
                output.append(f"   L{lineno}: {content}")

        output.append("")

    result = "\n".join(output)
    if len(result) > MAX_OUTPUT_CHARS:
        result = result[:MAX_OUTPUT_CHARS] + "\n\n[...truncated]"
    return result
