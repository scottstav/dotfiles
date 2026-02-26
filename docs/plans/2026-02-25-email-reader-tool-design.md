# Email Reader Tool for Claude Ask

**Date:** 2026-02-25

## Overview

A single `email.py` tool for the claude-ask system that searches and reads emails from the local Maildir store via `mu find`. The LLM interacts through semantic parameters (sender, subject, keywords, etc.) — the tool handles mu query construction with fuzzy wildcard matching.

## Architecture

One tool, two modes:

1. **Search mode** — returns a list of matching emails (metadata only)
2. **Read mode** — returns full headers + body of a specific email file

## Input Schema

```python
{
    "sender":      "partial name or email address",
    "recipient":   "partial name or email address",
    "subject":     "words expected in the subject line",
    "keywords":    "words expected anywhere in the email body",
    "date_range":  "today, 1w, 3d, 2025-01-01..2025-02-01, etc.",
    "unread_only": bool,
    "folder":      "inbox | sent | drafts | trash | archive",
    "read_path":   "/absolute/path/to/email/file"
}
```

All fields optional. At least one of the search fields or `read_path` must be provided.

## Fuzzy Query Construction

Each text parameter (sender, recipient, subject, keywords) is split into tokens. Each token gets a `*` wildcard suffix. Tokens within a field are ANDed together.

Examples:
- `sender="john smith"` → `from:john* AND from:smith*`
- `subject="deploy update"` → `subject:deploy* AND subject:update*`
- `keywords="invoice payment"` → `body:invoice* AND body:payment*`

This matches partial names, addresses, and word stems without requiring exact strings.

## Search Mode

Runs: `mu find <query> --format=json --maxnum=10 --skip-dups --sortfield=date --reverse`

Returns a formatted list:

```
Found 5 emails:

1. [2026-02-25] John Smith <john@example.com>
   Subject: Deployment update for staging
   Flags: seen
   Path: /home/ifit/.mail/gmail/Inbox/cur/...

2. ...
```

The path is included so the LLM can call read mode on a specific email.

## Read Mode

When `read_path` is provided, opens the file with Python's `email` module:
- Extracts headers: From, To, Cc, Date, Subject
- Extracts body: prefers text/plain, falls back to text/html with tag stripping
- Truncates body at 4000 characters with `[...truncated]` marker

## Folder Mapping

| Parameter  | Maildir path       |
|------------|--------------------|
| inbox      | /gmail/Inbox       |
| sent       | /gmail/Sent        |
| drafts     | /gmail/Drafts      |
| trash      | /gmail/Trash       |
| archive    | /gmail/Archive     |

## Error Handling

- Empty search results: return "No emails found matching your search."
- Invalid read_path: return "Email file not found at: <path>"
- No parameters provided: return usage hint
- mu not available: return error message

## Typical LLM Flow

1. User: "What did John say about the deployment?"
2. LLM calls: `email(sender="john", keywords="deployment")`
3. Tool returns list of matching emails with paths
4. LLM calls: `email(read_path="/home/ifit/.mail/gmail/Inbox/cur/...")`
5. Tool returns full email content
6. LLM answers the user's question

## File Location

`/home/ifit/dotfiles/.local/share/claude-ask/tools/email.py`
