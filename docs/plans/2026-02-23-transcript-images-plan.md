# Transcript Image Embedding Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Save images from claude-ask conversations as PNG files and embed them as inline markdown links in the transcript.

**Architecture:** Modify `format.py` only. When the formatter encounters image blocks (clipboard pastes or screenshot tool results), it decodes the base64 data, writes a PNG to `~/Dropbox/LLM/Chats/Media/`, and inserts `![screenshot](Media/<file>.png)` into the markdown output.

**Tech Stack:** Python 3.14, base64 stdlib, pathlib

---

### Task 1: Add `_save_image()` helper

**Files:**
- Modify: `/home/ifit/.local/share/claude-ask/format.py:1-8`

**Step 1: Add imports and MEDIA_DIR constant**

Add `import base64` and `from datetime import datetime, timezone` to imports. Add `MEDIA_DIR` constant after `ARCHIVE_DIR`.

```python
"""Shared conversation formatter and archiver for claude-ask."""

import base64
import re
from datetime import datetime, timezone
from pathlib import Path


ARCHIVE_DIR = Path.home() / "Dropbox" / "LLM" / "Chats"
MEDIA_DIR = ARCHIVE_DIR / "Media"
```

**Step 2: Add `_save_image()` function**

Insert after the `MEDIA_DIR` line, before `_extract_text()`:

```python
def _save_image(data_b64, media_type="image/png"):
    """Decode base64 image, save to Media/, return relative path."""
    MEDIA_DIR.mkdir(parents=True, exist_ok=True)
    ext = media_type.split("/")[-1] if "/" in media_type else "png"
    stamp = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H-%M-%S")
    path = MEDIA_DIR / f"{stamp}.{ext}"
    # Deduplicate: append -2, -3, etc. if file exists
    counter = 2
    while path.exists():
        path = MEDIA_DIR / f"{stamp}-{counter}.{ext}"
        counter += 1
    path.write_bytes(base64.b64decode(data_b64))
    return f"Media/{path.name}"
```

**Step 3: Verify syntax**

Run: `cd /home/ifit/.local/share/claude-ask && .venv/bin/python -c "import format; print('OK')"`
Expected: `OK`

**Step 4: Commit**

```bash
cd /home/ifit/dotfiles && git add .local/share/claude-ask/format.py && git commit -m "feat(claude-ask): add _save_image helper for transcript images"
```

---

### Task 2: Modify `_extract_text()` to handle image blocks

**Files:**
- Modify: `/home/ifit/.local/share/claude-ask/format.py:_extract_text`

**Step 1: Update `_extract_text()` to process image blocks**

Replace the existing `_extract_text()` function with:

```python
def _extract_text(content):
    """Extract plain text from a message content field.

    Image blocks are saved to Media/ and replaced with markdown image links.
    Tool blocks are skipped.
    """
    if isinstance(content, str):
        return content
    if isinstance(content, list):
        parts = []
        for block in content:
            if isinstance(block, dict):
                if block.get("type") == "text":
                    parts.append(block["text"])
                elif block.get("type") == "image":
                    source = block.get("source", {})
                    if source.get("type") == "base64" and source.get("data"):
                        rel = _save_image(source["data"], source.get("media_type", "image/png"))
                        parts.append(f"![screenshot]({rel})")
                # Skip tool_use, tool_result blocks
            elif isinstance(block, str):
                parts.append(block)
        return "\n\n".join(parts)
    return ""
```

**Step 2: Verify syntax**

Run: `cd /home/ifit/.local/share/claude-ask && .venv/bin/python -c "import format; print('OK')"`
Expected: `OK`

**Step 3: Commit**

```bash
cd /home/ifit/dotfiles && git add .local/share/claude-ask/format.py && git commit -m "feat(claude-ask): extract images from content blocks in transcripts"
```

---

### Task 3: Handle images inside tool_result messages

Currently `format_conversation()` skips tool_result messages entirely (lines 57-62). But screenshot tool results contain images we want. We need to extract images from tool_result blocks without rendering them as separate turns.

**Files:**
- Modify: `/home/ifit/.local/share/claude-ask/format.py:format_conversation`

**Step 1: Update the tool_result skip logic**

Replace the tool_result skip block in `format_conversation()`. Instead of skipping all-tool_result messages, extract any images from them and attach to the previous assistant turn.

Replace this section of `format_conversation()`:

```python
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
```

With:

```python
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
```

**Step 2: Add `_extract_tool_result_images()` helper**

Insert before `format_conversation()`:

```python
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
                    parts.append(f"![screenshot]({rel})")
    return "\n\n".join(parts)
```

**Step 3: Verify syntax**

Run: `cd /home/ifit/.local/share/claude-ask && .venv/bin/python -c "import format; print('OK')"`
Expected: `OK`

**Step 4: Commit**

```bash
cd /home/ifit/dotfiles && git add .local/share/claude-ask/format.py && git commit -m "feat(claude-ask): extract images from screenshot tool results in transcripts"
```

---

### Task 4: End-to-end manual verification

**Step 1: Create a test conversation with a fake image**

```bash
cd /home/ifit/.local/share/claude-ask && .venv/bin/python -c "
import json, base64, format

# Tiny 1x1 red PNG
png_b64 = 'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8/5+hHgAHggJ/PchI7wAAAABJRU5ErkJggg=='

conv = {
    'id': 'test-conv',
    'created': '2026-02-23T12:00:00+00:00',
    'messages': [
        # User sends text + clipboard image
        {'role': 'user', 'content': [
            {'type': 'image', 'source': {'type': 'base64', 'media_type': 'image/png', 'data': png_b64}},
            {'type': 'text', 'text': 'What is this image?'},
        ]},
        # Claude responds with text, then uses screenshot tool
        {'role': 'assistant', 'content': [
            {'type': 'text', 'text': 'Let me take a screenshot to compare.'},
            {'type': 'tool_use', 'id': 'tool_1', 'name': 'screenshot', 'input': {}},
        ]},
        # Tool result with screenshot image
        {'role': 'user', 'content': [
            {'type': 'tool_result', 'tool_use_id': 'tool_1', 'content': [
                {'type': 'image', 'source': {'type': 'base64', 'media_type': 'image/png', 'data': png_b64}},
            ]},
        ]},
        # Claude's final response
        {'role': 'assistant', 'content': [
            {'type': 'text', 'text': 'That looks like a red pixel.'},
        ]},
    ],
}

result = format.format_conversation(conv)
print(result)
print()
print('--- Checking Media/ ---')
import os
media = format.MEDIA_DIR
if media.exists():
    for f in sorted(media.iterdir()):
        print(f'  {f.name} ({f.stat().st_size} bytes)')
else:
    print('  Media/ not created!')
"
```

Expected output should contain:
- `# 1. User` section with `![screenshot](Media/...)` AND `What is this image?`
- `# 1. Claude` section with `Let me take a screenshot to compare.`
- A screenshot image link appended after Claude's tool use turn
- `# 1. Claude` section (second) with `That looks like a red pixel.`
- Two PNG files in Media/

**Step 2: Clean up test files**

```bash
rm -f ~/Dropbox/LLM/Chats/Media/2026-02-23T*.png
```

**Step 3: Live test with actual claude-ask**

Restart the daemon and send a message with a clipboard image paste:

```bash
systemctl --user restart claude-ask
# Then use claude-ask normally — paste an image and ask about it
# Check the transcript: ls -la ~/Dropbox/LLM/Chats/ | tail -3
# Check for image: ls -la ~/Dropbox/LLM/Chats/Media/
```

**Step 4: Final commit (if any fixes needed)**

```bash
cd /home/ifit/dotfiles && git add .local/share/claude-ask/format.py && git commit -m "fix(claude-ask): polish transcript image embedding"
```
