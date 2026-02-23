# Claude Ask: Tools & Image Support

## Overview

Extend claude-ask with 5 tool plugins and clipboard image paste support.

## Tools

All tools are Python files in `~/.local/share/claude-ask/tools/` using the existing plugin interface (`name`, `description`, `input_schema`, `run()`).

### 1. shell.py — Run shell commands

- Input: `{"command": "..."}`
- `subprocess.run(command, shell=True, capture_output=True, timeout=30)`
- Returns stdout + stderr, truncated to ~4000 chars
- No approval gate — runs immediately

### 2. web_search.py — Search the web

- Input: `{"query": "..."}`
- Uses `duckduckgo-search` Python library (no API key)
- Returns top 5 results: title, URL, snippet
- New dependency: `duckduckgo-search`

### 3. fetch_url.py — Read a web page

- Input: `{"url": "..."}`
- Fetches page with `httpx` (already installed)
- Extracts article text with `trafilatura`
- Returns extracted text, truncated to ~6000 chars
- New dependency: `trafilatura`

### 4. clipboard.py — Copy to clipboard

- Input: `{"text": "..."}`
- Pipes text to `wl-copy`
- Returns confirmation message

### 5. screenshot.py — Capture screen region

- Input: `{"region": "full" | "select"}`
- `select`: `slurp` for region pick, then `grim -g` to capture
- `full`: `grim` for full screen capture
- Returns dict: `{"type": "image", "data": "<base64>", "media_type": "image/png"}`

## Image Support

### Tool result images

Extend `_run_tool` in daemon.py: if a tool's `run()` returns a dict with `type: "image"`, wrap it as an image content block in the `tool_result` message instead of a text block.

### Clipboard image paste in TUI

Extend `input.py`: on submit, check if clipboard has image data (`wl-paste --list-types` contains `image/png`). If so, grab bytes via `wl-paste --type image/png`, base64-encode, include in socket message as `"image": "<base64>"`. Daemon prepends image content block to user message.

### System prompt update

Add tool descriptions to system prompt so Claude uses them proactively.

## Dependencies

New pip packages: `duckduckgo-search`, `trafilatura`

System tools (already installed): `wl-copy`, `wl-paste`, `grim`, `slurp`
