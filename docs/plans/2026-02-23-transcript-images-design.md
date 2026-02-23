# Transcript Image Embedding

## Problem

claude-ask stores screenshots and clipboard-pasted images as base64 in conversation JSON, but the markdown transcripts in `~/Dropbox/LLM/Chats/` discard them entirely. Images are lost when reviewing past conversations.

## Solution

Modify `format.py` to detect image blocks during transcript formatting, save them as PNG files to `~/Dropbox/LLM/Chats/Media/`, and insert inline markdown image links at the position they occurred in the conversation.

## Design

### Image sources (both handled identically)

- **Clipboard pastes**: `{"type": "image", "source": {"type": "base64", "media_type": "image/png", "data": "..."}}`
- **Screenshot tool results**: Same format, inside tool_result messages

### Changes to `format.py`

1. Add `MEDIA_DIR = ARCHIVE_DIR / "Media"` constant
2. Add `_save_image(data_b64, media_type)` function:
   - Decodes base64 data
   - Generates timestamp filename: `YYYY-MM-DDTHH-MM-SS.png`
   - Deduplicates with `-2`, `-3` suffix if needed
   - Writes to `MEDIA_DIR`
   - Returns relative path `Media/<filename>`
3. Modify `_extract_text()` to also handle image blocks:
   - When encountering an image block, call `_save_image()`
   - Include `![screenshot](<relative_path>)` in the returned text
4. Handle tool_result messages that contain images (currently skipped entirely)

### File layout

```
~/Dropbox/LLM/Chats/
  2026-02-23-can-you-explain.md      # transcript with ![screenshot](Media/...) links
  Media/
    2026-02-23T14-32-01.png           # saved images
    2026-02-23T14-32-01-2.png         # deduped if same second
```

### What doesn't change

- Numbered heading format (`# 1. User`, `# 1. Claude`)
- Text extraction for non-image content
- Archive path generation
- daemon.py — no changes needed
