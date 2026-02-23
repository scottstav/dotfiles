---
name: slack-message
description: Use when composing a Slack message to share technical findings, code references, or investigation results with teammates
---

# Slack Message

Compose a concise Slack message and copy it to the clipboard.

## Rules

- **Short and direct.** Lead with the answer. Cut filler words.
- **Slack markdown.** Use `*bold*` for emphasis, `•` for bullets, `` `inline code` `` for paths/endpoints.
- **Inline links.** Always use `[link text](url)` for GitHub references. Never paste raw URLs when you can describe the link.
- **GitHub links.** Point to a specific commit SHA, not a branch, so links don't rot. Use line ranges: `#L10-L20`.
- **No greetings or sign-offs.** No "Hey team", no emoji, no "Let me know if you have questions."

## Process

1. Gather the relevant code locations (file paths, line numbers).
2. Get the GitHub remote URL and a stable commit SHA for each repo.
3. Write the message to `/tmp/slack-msg.txt`.
4. Copy to clipboard with `wl-copy < /tmp/slack-msg.txt` (fall back to `xclip -selection clipboard` if wl-copy unavailable).

## Example Output

```
There's no endpoint for X today. Here's what exists:

*service-a* has a [create widget endpoint](https://github.com/org/service-a/blob/abc123/src/controllers/widget.ts#L40-L55) but it requires admin auth.

*service-b* handles it in two places:
• [POST /api/widgets](https://github.com/org/service-b/blob/def456/routes/widgets.js#L12-L18) — requires session auth
• [WidgetService.create](https://github.com/org/service-b/blob/def456/lib/widget-service.js#L30-L45) — called internally only

So this would need to be built as a new endpoint if you need token auth.
```
