# Efrit Workflow Integration Design

## Goal

Streamline the AI tooling in Emacs to three clear layers, with efrit as the primary in-buffer AI tool. Remove gptel entirely.

## Three-Layer AI Stack

| Layer | Tool | Trigger |
|-------|------|---------|
| Autocomplete | Copilot | Passive, `prog-mode` only, accept with `<backtab>` |
| In-buffer AI | Efrit | Interactive: chat, do, agent |
| Full agent | Claude Code (claude-code-ide) | Project-wide, multi-file, git, tests |

### When to use efrit vs Claude Code

- **Efrit:** You're in a buffer, want help with this file or nearby files, want to stay in Emacs's buffer model.
- **Claude Code:** Project-wide search, git operations, running tests, MCP servers, web search, long multi-step sessions.

## Efrit Commands

| Binding | Command | Use for |
|---------|---------|---------|
| `C-c a c` | `efrit-chat` | Ask questions about code, get explanations |
| `C-c a d` | `efrit-do` | Quick async edits (add error handling, write docstrings, refactor) |
| `C-c a a` | `efrit-agent` | Structured multi-step work with visibility and interactive control |
| `C-c a p` | `efrit-do-show-progress` | View progress of running efrit-do task |

### efrit-agent tips

- Press `i` to inject guidance mid-session
- Press `k` to cancel if it goes off-track
- Has TODO tracking for visibility into multi-step progress

## Claude Code Bindings (unchanged)

| Binding | Command |
|---------|---------|
| `C-c a s` | `claude-code-ide-menu` |
| `C-c a t` | `claude-code-ide-toggle` |
| `C-c a n` | `claude-code-ide` (new session) |
| `C-c a k` | `claude-code-ide-continue` |
| `C-c a @` | `claude-code-ide-insert-at-mentioned` |
| `C-c a i` | `claude-code-ide-send-prompt` |
| `C-c a e` | `claude-code-ide-send-escape` |

## Copilot (unchanged)

- Hook: `prog-mode` only (not in text/org/markdown)
- Accept: `<backtab>`
- `copilot-indent-offset-warning-disable` set to `t`

## What Gets Removed

All gptel packages and associated config:

- `gptel` (package, model config, backend config)
- `gptel-agent`
- `gptel-magit`
- `gptel-send` binding (`C-c a g`)
- `gptel-rewrite` binding (`C-c a r`)
- `gptel-generate-code-here` function
- `my/ai-dired-rename` function
- `my/gptel-image-download-setup` function and hook

## Future: MCP Remote Queue (not implementing now)

Efrit includes an MCP server that enables Claude Code to send commands into the running Emacs instance via a file-based queue (`efrit-remote-queue`). This allows agent-to-agent communication:

- Claude Code writes JSON request to `~/.emacs.d/.efrit/queues/default/requests/`
- Efrit picks it up, executes in Emacs, writes response
- Request types: command, chat, eval, status

To set up later:
1. Build MCP server: `cd ~/.emacs.d/straight/repos/efrit/mcp && npm install && npm run build`
2. Add MCP server config to `.claude/settings.json`
3. Set `efrit-remote-queue-auto-start` to `t` in init.org

## Implementation Steps

1. Remove all gptel-related code from init.org (gptel, gptel-agent, gptel-magit, gptel-send/rewrite bindings, custom functions)
2. Verify efrit config is correct (model set to claude-opus-4-6, keybindings)
3. Verify copilot is prog-mode only
4. Tangle init.org and evaluate in running Emacs
5. Verify efrit-chat, efrit-do, efrit-agent all work
