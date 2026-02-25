# /update-prs Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** A reusable `/update-prs` slash command that discovers open PRs, triages them, builds a beads dependency DAG, dispatches subagents to rebase and fix each, and batch-confirms force-pushes.

**Architecture:** Single slash command file that orchestrates 5 phases: discover, triage, plan (beads DAG), execute (subagent loop on `bd ready`), confirm (batch push). Beads tracks sequencing — same-repo PRs chain oldest-first via `bd dep --blocks`. Cross-repo PRs run in parallel.

**Tech Stack:** Claude Code slash command (markdown), `gh` CLI, `bd` CLI (beads), git, Task tool for subagents.

**Design doc:** `docs/plans/2026-02-24-update-prs-design.md`

---

### Task 1: Initialize beads at ~/projects

Initialize a beads database at `~/projects` so the command has a centralized place to track work.

**Step 1: Check if beads is already initialized**

Run: `ls ~/projects/.beads/ 2>/dev/null && echo "exists" || echo "not found"`

**Step 2: Initialize if needed**

Run: `cd ~/projects && bd init`

Expected: `.beads/` directory created with a database file.

**Step 3: Verify**

Run: `bd status --db ~/projects/.beads/*.db`

Expected: Shows empty database overview.

---

### Task 2: Write the /update-prs slash command

**Files:**
- Create: `~/.claude/commands/update-prs.md`

This is the core deliverable. The command file contains the full orchestration prompt.

**Step 1: Write the command file**

Create `~/.claude/commands/update-prs.md` with the content below.

The frontmatter needs:
- `allowed-tools`: Bash, Read, Glob, Grep, Task, TaskOutput, AskUserQuestion, WebFetch, mcp__github__search_pull_requests, mcp__github__pull_request_read
- `description`: Discover open PRs, triage, build beads DAG, dispatch subagents to rebase/fix, batch confirm pushes.

The command body covers all 5 phases. Key sections:

#### Phase 1: Discover

```
gh api "search/issues?q=is:pr+is:open+author:@me+org:ifit+draft:false&sort=created&order=asc&per_page=30" \
  --jq '.items[] | {repo: (.repository_url | split("/") | .[-1]), number, title, head: .pull_request, created_at}'
```

Then for each PR, fetch full details to get `head.ref` (feature branch) and `base.ref` (target branch):

```
gh api repos/ifit/<repo>/pulls/<number> --jq '{head_branch: .head.ref, base_branch: .base.ref, html_url: .html_url}'
```

#### Phase 2: Triage

For each PR, fetch in parallel:
- Check runs: `gh pr checks ifit/<repo>#<number> --json name,state,conclusion` — look for any with conclusion=failure
- Reviews: `gh api repos/ifit/<repo>/pulls/<number>/reviews --jq '[.[] | select(.state == "CHANGES_REQUESTED")]'`
- Review comments: `gh api repos/ifit/<repo>/pulls/<number>/comments --jq '[.[] | {body, path, line, user: .user.login, created_at}]'`
- If CI failed, fetch the failed run logs: `gh run view <run-id> --repo ifit/<repo> --log-failed`

Classify:
- **needs-work**: any check failed OR any review with state CHANGES_REQUESTED
- **skip**: all checks pass AND no CHANGES_REQUESTED reviews

Display a triage summary table:
```
## Triage Summary

| # | Repo | PR | Title | CI | Reviews | Action |
|---|------|----|-------|----|---------|--------|
| 1 | ifit | #21915 | Bridge req.session.user | pass | changes requested | needs-work |
| 2 | ifit | #21941 | bridge sendLegalNotice | pass | review required | skip |
...
```

#### Phase 3: Plan (Beads DAG)

```bash
# Create epic
EPIC_ID=$(bd create "update-prs $(date +%Y-%m-%d)" --type epic --db ~/projects/.beads/*.db --json | jq -r '.id')

# Create beads per needs-work PR (store metadata as labels or in description)
BEAD_ID=$(bd create "<repo>#<number>: <title>" --type task --parent $EPIC_ID \
  --db ~/projects/.beads/*.db --json | jq -r '.id')

# Wire same-repo deps (oldest first)
bd dep $OLDER_BEAD --blocks $NEWER_BEAD --db ~/projects/.beads/*.db

# Close skip beads immediately
bd close $SKIP_BEAD --db ~/projects/.beads/*.db
```

Display the DAG:
```bash
bd graph --parent $EPIC_ID --db ~/projects/.beads/*.db
```

#### Phase 4: Execute

Loop on `bd ready`:

```
while bd ready --parent $EPIC_ID --db ~/projects/.beads/*.db returns results:
    for each ready bead:
        dispatch subagent via Task tool
    wait for completion
    close bead on success
```

Each subagent gets a prompt containing:
1. Repo path: `~/projects/<repo>`
2. Feature branch name and base branch name
3. Why it needs work (CI failures with log excerpts, or review comments with full text)
4. Explicit instructions:
   - `git fetch origin && git checkout <branch> && git rebase origin/<base>`
   - Read feedback (included in prompt)
   - Make fixes
   - Determine verification commands: check repo CLAUDE.md first, then parse `.github/workflows/*.yml`
   - Run verification commands. MUST see green output. If failing, fix and re-run. If stuck, report what's failing — do NOT close the bead.
   - Commit locally. Do NOT push.
   - Report what was done.

The subagent type should be `general-purpose` since it needs full tool access (Bash, Read, Edit, Write, Glob, Grep).

**Same-repo sequencing:** Only one subagent per repo at a time. Cross-repo subagents run in parallel. After a subagent finishes and the orchestrator closes its bead, `bd ready` surfaces the next bead in the chain.

#### Phase 5: Batch Confirm

Collect results from all subagents. Present summary:

```
## PR Update Summary — YYYY-MM-DD

### Ready to force-push:
1. ifit#21915 (branch-name → origin/test)
   - Rebased, addressed N review comments
   - Verified: npm test, npm run lint

### Stuck (needs manual attention):
2. ifit#21942 — couldn't fix: description of what failed

### Skipped (no action needed):
3. ifit#21941 — CI green, no change requests

Push all ready? [y/n]
```

Use `AskUserQuestion` with options:
- **Push all** — force-push all ready branches
- **Push selected** — let user pick which to push
- **Skip** — leave everything local

On push: `git -C ~/projects/<repo> push --force-with-lease origin <branch>` for each.

After pushing, close the epic: `bd close $EPIC_ID --db ~/projects/.beads/*.db`

---

### Task 3: Sync to dotfiles

After writing the command file, sync it into dotfiles and stow.

**Step 1: Run sync script**

Run: `~/dotfiles/sync-claude.sh`

**Step 2: Verify symlink**

Run: `ls -la ~/.claude/commands/update-prs.md`

Expected: Symlink pointing to `~/dotfiles/.claude/commands/update-prs.md`

---

### Task 4: Smoke test

Test the command by running `/update-prs` in a Claude Code session and verifying:

1. Phase 1 discovers the expected PRs (4 non-draft)
2. Phase 2 triage correctly classifies each
3. Phase 3 creates the beads DAG at `~/projects/.beads/`
4. Phase 4 dispatches subagents (verify at least one runs correctly)
5. Phase 5 presents the batch confirm

This is a manual test — run the command and observe behavior.
