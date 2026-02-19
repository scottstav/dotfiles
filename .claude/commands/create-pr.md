---
allowed-tools: Bash, Read, Glob, Grep, AskUserQuestion, mcp__github__pull_request_read
description: Create a pull request using the repo's PR template with properly formatted markdown.
---

You are a PR creation assistant. Your job is to create a well-formatted pull request that follows the repo's PR template exactly. You MUST use `gh pr create` with a HEREDOC for the body to ensure proper newline handling. NEVER use the MCP `create_pull_request` tool — it mangles newlines.

## Step 1: Gather context

1. Run `git status` and `git log --oneline origin/test..HEAD` to understand what's on this branch.
2. Run `git diff origin/test...HEAD --stat` to see changed files.
3. Read the PR template at `.github/PULL_REQUEST_TEMPLATE.md` in the repo root.
4. If the user provided arguments (e.g. a title or description), use those. Otherwise, infer from the commits and diff.

## Step 2: Determine base branch

- **ALWAYS use `test` as the base branch. NEVER use `master` or `main`.**
- The only exception is if the user explicitly specifies a different base branch.

## Step 3: Draft the PR

Fill in the PR template with:
- **Description**: A clear summary of what changed and why. Include a JIRA link if a ticket is identifiable from the branch name (pattern: `PROJECT-ISSUE`).
- **Known Impacted Areas of Code**: List files/modules affected.
- **Steps taken to Recreate / Verify**: What was done to test.
- **Checklists**: Keep all checklist items from the template. Check off items that are clearly satisfied.

Remove comment-only lines (`[//]: #`) from the template — they're instructions for humans, not content.

Do NOT include a FED checklist unless the PR touches frontend UI code (CSS, components, views, etc.).

## Step 4: Confirm with user

Show the user the draft title and body. Ask if they want to proceed or make changes. Present this as an `AskUserQuestion` with options:
- **Create PR** — proceed with the draft
- **Create as draft** — create as a draft PR
- **Edit** — let them provide changes

## Step 5: Create the PR

Use `gh pr create` with a HEREDOC. This is the ONLY acceptable method — it preserves newlines correctly.

```bash
gh pr create --base test --title "the title here" --draft --body "$(cat <<'PREOF'
The full PR body here with real newlines.

## Description
...

## Checklists
...
PREOF
)"
```

Key rules:
- Use `--draft` if the user requested a draft PR.
- The HEREDOC delimiter MUST be quoted (`'PREOF'`) to prevent shell interpolation.
- NEVER use `\n` for newlines — use actual newlines in the HEREDOC.
- NEVER use the MCP `create_pull_request` or `update_pull_request` tools for the body.

## Step 6: Report

Show the user the PR URL and open it in their browser with `xdg-open`.
