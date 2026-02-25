# /update-prs Design

A Claude Code slash command that discovers your open GitHub PRs, determines which need work (failed CI, review comments), builds a beads dependency DAG to sequence same-repo PRs, dispatches subagents to rebase and fix each one, and batch-confirms force-pushes at the end.

## Decisions

- **Approach:** Beads-Light — create beads and deps directly with `bd create` / `bd dep add`, no formula/molecule layer
- **Beads location:** Centralized at `~/projects/.beads/`
- **Scope:** `org:ifit`, non-draft PRs, authored by you
- **Triggers:** PR needs work if CI is failing OR unresolved review comments exist
- **Same-repo ordering:** Oldest PR first, chained with `bd dep --blocks`
- **Cross-repo:** Independent, dispatched in parallel
- **Push policy:** Batch confirm — subagents do all work locally, present summary, user approves all force-pushes at once
- **Verification:** Subagents MUST run test/lint/build locally and see green before closing their bead. CLAUDE.md for commands first, parse GHA workflow YAML as fallback.
- **Delivery:** Slash command at `~/.claude/commands/update-prs.md`

## Phase 1: Discover

Query GitHub for open, non-draft PRs authored by you in the ifit org:

```
gh api "search/issues?q=is:pr+is:open+author:@me+org:ifit+draft:false" --jq ...
```

Extract per PR: repo name, PR number, title, head branch, base branch (`base.ref`).

## Phase 2: Triage

For each PR, fetch in parallel:

- **Check status:** `gh pr checks <owner>/<repo> --json` — are any checks failing?
- **Reviews:** `gh api repos/<owner>/<repo>/pulls/<number>/reviews` — any requesting changes?
- **Review comments:** `gh api repos/<owner>/<repo>/pulls/<number>/comments` — unresolved feedback?
- **Failed check logs:** If CI failed, fetch the relevant workflow run logs to include in the subagent prompt.

Classify each PR:
- **needs-work:** CI failing OR unresolved change-request reviews
- **skip:** CI green AND no actionable review comments

## Phase 3: Plan (Beads DAG)

Initialize beads at `~/projects` if needed (`bd init`).

1. Create an epic: `bd create "update-prs YYYY-MM-DD" --type epic`
2. For each needs-work PR, create a task bead as a child of the epic, storing metadata: repo, PR number, head branch, base branch, triage reason (failed CI / review comments), pre-fetched feedback
3. For skipped PRs, create a bead and immediately close it with a note
4. Wire same-repo dependencies: sort PRs by creation date (oldest first), chain them: `bd dep <older> --blocks <newer>`
5. Cross-repo PRs have no dependencies — all immediately ready

## Phase 4: Execute

Loop:

```
while bd ready --parent <epic-id> returns results:
    for each ready bead:
        dispatch subagent (one at a time per repo, parallel across repos)
    wait for subagent completion
    subagent closes bead on success (unblocks next in chain)
```

### Subagent Workflow

Each subagent receives a prompt containing: repo path, feature branch, base branch, triage reason, pre-fetched review comments and/or CI failure logs.

The subagent executes:

1. **Rebase onto base branch:**
   ```
   cd ~/projects/<repo>
   git fetch origin
   git checkout <feature-branch>
   git rebase origin/<base-branch>
   ```
   Resolve conflicts if any arise.

2. **Read the feedback** (already in the prompt):
   - If CI failed: the specific error output from the failed workflow steps
   - If review comments: the reviewer's requested changes

3. **Make fixes** to address the feedback.

4. **Verify locally — HARD GATE:**
   - Read the repo's CLAUDE.md for test/lint/build commands
   - If not found, parse `.github/workflows/*.yml` to extract the CI steps
   - Run the commands. See green output.
   - If they fail: fix and re-run. Iterate until passing.
   - If stuck after reasonable effort: leave the bead open with a comment explaining what failed. Do NOT close the bead. Do NOT lie about it passing.

5. **Commit locally** — do NOT push. The push happens in Phase 5.

6. **Close the bead** — only after local verification passes. Report what was done.

### Same-Repo Sequencing

For repos with multiple PRs (e.g., ifit with 3), beads dependencies enforce ordering:
- Only the oldest PR's bead is initially ready
- When its subagent closes it, the next bead becomes ready via `bd ready`
- Each subagent rebases onto `origin/<base-branch>` (not the previous PR's branch — PRs are independent)

## Phase 5: Batch Confirm

After all beads are closed (or stuck), present a summary:

```
PR Update Summary — YYYY-MM-DD
================================

Ready to force-push:
  1. ifit#21915 (BOMB-3100-branch → origin/test)
     - Rebased, addressed 3 review comments
     - Verified: npm test, npm run lint

  2. user-service#1391 (feature-branch → origin/test)
     - Rebased, addressed 1 review comment
     - Verified: npm test

Stuck (needs manual attention):
  3. ifit#21942 — test failure in xyz.test.js after fix attempt

Push all ready? [y/n]
```

On approval: `git push --force-with-lease` each branch.
On decline: changes stay local for manual inspection.

## Directory Mapping

Repos map to `~/projects/<repo-name>`. The command verifies the directory exists before dispatching a subagent. If a repo directory is missing, the bead is closed with an error note.

## Error Handling

- **Repo not found locally:** Close bead with note, continue with others
- **Rebase conflict subagent can't resolve:** Leave bead open, report the conflict
- **Tests won't pass:** Leave bead open with details of what's failing
- **No actionable feedback found:** Close bead with "no changes needed" note
- **GitHub API errors:** Abort early with clear error message

## Future Extensions

- Support additional orgs beyond `ifit`
- Promote to beads formula if the workflow stabilizes
- Option to auto-push without batch confirm for trusted repos
