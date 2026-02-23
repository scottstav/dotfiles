"""Shared conversation formatter and archiver for claude-ask."""

import base64
import re
from datetime import datetime, timezone
from pathlib import Path


ARCHIVE_DIR = Path.home() / "Dropbox" / "LLM" / "Chats"
MEDIA_DIR = ARCHIVE_DIR / "Media"


def _save_image(data_b64, media_type="image/png"):
    """Decode base64 image, save to Media/, return relative path or None."""
    if not data_b64:
        return None
    try:
        raw = base64.b64decode(data_b64)
    except Exception:
        return None
    MEDIA_DIR.mkdir(parents=True, exist_ok=True)
    ext = media_type.split("/")[-1] if "/" in media_type else "png"
    stamp = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H-%M-%S")
    path = MEDIA_DIR / f"{stamp}.{ext}"
    # Deduplicate: append -2, -3, etc. if file exists
    counter = 2
    while path.exists():
        path = MEDIA_DIR / f"{stamp}-{counter}.{ext}"
        counter += 1
    path.write_bytes(raw)
    return f"Media/{path.name}"


def _extract_text(content, save_images=False):
    """Extract plain text from a message content field.

    When save_images is True, image blocks are saved to Media/ and replaced
    with markdown image links.  Otherwise image blocks are skipped.
    Tool blocks are always skipped.
    """
    if isinstance(content, str):
        return content
    if isinstance(content, list):
        parts = []
        for block in content:
            if isinstance(block, dict):
                if block.get("type") == "text":
                    parts.append(block["text"])
                elif save_images and block.get("type") == "image":
                    source = block.get("source", {})
                    if source.get("type") == "base64" and source.get("data"):
                        rel = _save_image(source["data"], source.get("media_type", "image/png"))
                        if rel:
                            parts.append(f"![screenshot]({rel})")
                # Skip tool_use, tool_result, and image blocks (when not saving)
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


def _extract_tool_result_images(content):
    """Extract and save images from tool_result blocks. Return markdown or empty string."""
    parts = []
    for block in content:
        if not isinstance(block, dict) or block.get("type") != "tool_result":
            continue
        inner = block.get("content")
        if not isinstance(inner, list):
            continue
        for item in inner:
            if isinstance(item, dict) and item.get("type") == "image":
                source = item.get("source", {})
                if source.get("type") == "base64" and source.get("data"):
                    rel = _save_image(source["data"], source.get("media_type", "image/png"))
                    if rel:
                        parts.append(f"![screenshot]({rel})")
    return "\n\n".join(parts)


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

        # Tool result messages: extract any images, append to previous turn
        if role == "user" and isinstance(content, list) and all(
            isinstance(b, dict) and b.get("type") == "tool_result"
            for b in content
        ):
            images = _extract_tool_result_images(content)
            if images and lines:
                lines.append(images)
                lines.append("")
            continue

        text = _extract_text(content, save_images=True).strip()
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
