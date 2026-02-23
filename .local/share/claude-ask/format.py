"""Shared conversation formatter and archiver for claude-ask."""

import re
from pathlib import Path


ARCHIVE_DIR = Path.home() / "Dropbox" / "LLM" / "Chats"


def _extract_text(content):
    """Extract plain text from a message content field, skipping tool blocks."""
    if isinstance(content, str):
        return content
    if isinstance(content, list):
        parts = []
        for block in content:
            if isinstance(block, dict):
                if block.get("type") == "text":
                    parts.append(block["text"])
                # Skip tool_use, tool_result, image blocks
            elif isinstance(block, str):
                parts.append(block)
        return "\n\n".join(parts)
    return ""


def _slugify(text, max_words=6):
    """Turn text into a filename-safe slug from its first few words."""
    text = text.strip().lower()
    text = re.sub(r"[^\w\s-]", "", text)
    words = text.split()[:max_words]
    slug = "-".join(words)
    return slug[:60] if slug else "conversation"


def format_conversation(conv):
    """Render a conversation as numbered markdown.

    Returns a string like:
        # 1. User
        <text>

        # 1. Claude
        <text>

        # 2. User
        ...
    """
    lines = []
    pair_num = 0

    for msg in conv.get("messages", []):
        role = msg.get("role")
        content = msg.get("content")

        # Skip tool_result messages (role=user but content is tool results)
        if role == "user":
            if isinstance(content, list) and all(
                isinstance(b, dict) and b.get("type") == "tool_result"
                for b in content
            ):
                continue

        text = _extract_text(content).strip()
        if not text:
            continue

        if role == "user":
            pair_num += 1
            lines.append(f"# {pair_num}. User\n")
            lines.append(text)
            lines.append("")
        elif role == "assistant":
            lines.append(f"# {pair_num}. Claude\n")
            lines.append(text)
            lines.append("")

    return "\n".join(lines)


def archive_path(conv):
    """Return ~/Dropbox/LLM/Chats/<date>-<slug>.md for a conversation.

    Creates the directory if needed. The date comes from the conversation's
    created timestamp (YYYY-MM-DD).
    """
    ARCHIVE_DIR.mkdir(parents=True, exist_ok=True)

    # Extract date from conversation created field
    created = conv.get("created", "")
    date_str = created[:10] if len(created) >= 10 else "unknown"

    # Get slug from first user message
    slug = "conversation"
    for msg in conv.get("messages", []):
        if msg.get("role") == "user":
            text = _extract_text(msg.get("content", ""))
            if text.strip():
                slug = _slugify(text)
                break

    return ARCHIVE_DIR / f"{date_str}-{slug}.md"
