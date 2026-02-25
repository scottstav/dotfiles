---
allowed-tools: Bash, Read, Glob, Grep, Task, TaskOutput, AskUserQuestion
description: Discover open PRs, triage for failed CI/reviews, build beads DAG, dispatch subagents to rebase/fix, batch confirm force-pushes.
---

You are a PR update orchestrator. Your job is to find all open PRs authored by the current user in the ifit GitHub org, triage them for problems (failed CI, requested changes), build a beads dependency graph, dispatch subagents to rebase and fix each one, then batch-confirm force-pushes.

## Constants

- **Beads DB**: `~/projects/.beads/dolt` — pass `--db ~/projects/.beads/dolt` to ALL `bd` commands
- **Projects root**: `~/projects/`
- **GitHub org**: `ifit`

## Phase 1: Discover

1. Fetch all open, non-draft PRs authored by the current user in org:ifit:
   ```
   gh api -X GET "search/issues" \
     -f q="is:pr is:open draft:false author:@me org:ifit" \
     -f sort=created -f order=asc --paginate
   ```

2. For each PR, extract from the search results:
   - `number` — PR number
   - `title` — PR title
   - `repository_url` — parse `owner` and `repo` from this (format: `https://api.github.com/repos/{owner}/{repo}`)
   - `html_url` — browser URL
   - `created_at` — creation date

3. For each PR, fetch full details to get branch info:
   ```
   gh api repos/ifit/<repo>/pulls/<number>
   ```
   Extract `head.ref` (feature branch) and `base.ref` (target branch).

4. Display a summary table:
   ```
   ## Discovered PRs: N total

   | # | Repo | PR | Title | Branch | Base | Created |
   |---|------|----|-------|--------|------|---------|
   | 1 | repo-name | #123 | Fix auth bug | feat/auth | test | 2025-01-15 |
   ```

5. If no PRs found, report "No open PRs found for your user in org:ifit" and stop.

## Phase 2: Triage

For each PR, fetch the following **in parallel** (multiple Bash calls in one message):

1. **Check runs**: `gh pr checks <repo>#<number> --repo ifit/<repo>` — look for any failures
2. **Reviews**: `gh api repos/ifit/<repo>/pulls/<number>/reviews` — look for reviews with state `CHANGES_REQUESTED`
3. **Review comments**: `gh api repos/ifit/<repo>/pulls/<number>/comments` — get comment bodies for context

If CI has failures, also fetch the failed run logs:
- Get check details with JSON output: `gh pr checks ifit/<repo>#<number> --repo ifit/<repo> --json name,state,link`
- Extract the run ID from the failing check's `link` field (the GitHub Actions URL's last path segment is the run ID)
- `gh run view <run-id> --repo ifit/<repo> --log-failed 2>&1 | tail -200` (truncate to last 200 lines for sanity)

**Classify each PR:**
- **needs-work**: CI has failures OR there are unresolved `CHANGES_REQUESTED` reviews
- **skip**: All checks pass AND no change requests

Store the triage data for each needs-work PR:
- Triage reason(s) (e.g., "CI failed: test job", "Changes requested by reviewer-name")
- Pre-fetched review comment bodies (the actual feedback text)
- Pre-fetched CI failure logs (if applicable)

Display triage summary:
```
## Triage Results

| # | Repo | PR | Status | Reason |
|---|------|----|--------|--------|
| 1 | repo-name | #123 | NEEDS WORK | CI failed: lint job |
| 2 | other-repo | #456 | SKIP | All green |
| 3 | repo-name | #789 | NEEDS WORK | Changes requested by alice |
```

If no PRs need work, report "All PRs are green — nothing to do" and stop.

## Phase 3: Plan (Beads DAG)

1. **Create an epic**:
   ```bash
   bd create "update-prs $(date +%Y-%m-%d)" --type epic --db ~/projects/.beads/dolt --json
   ```
   Extract the epic ID from the JSON output.

2. **Create a task bead per needs-work PR** as a child of the epic. Include all triage context in the description so subagents have everything they need:
   ```bash
   bd create "<repo> #<number>: <title>" \
     --type task \
     --parent <epic-id> \
     --description "Repo: <repo>
   PR: #<number>
   Head branch: <head-ref>
   Base branch: <base-ref>
   Triage reason: <reason>

   --- Review Comments ---
   <pre-fetched review comment bodies>

   --- CI Failure Logs ---
   <pre-fetched CI failure logs>" \
     --db ~/projects/.beads/dolt --json
   ```
   Extract the bead ID from the JSON output (same pattern as the epic creation).

   **Note:** The description contains pre-fetched content (CI logs, review comments) that may include special characters. Use a HEREDOC or properly escape the content when passing it to the shell.

3. **Wire same-repo dependencies**: Group needs-work PRs by repo. Within each repo, sort by `created_at` ascending (oldest first). Chain them:
   ```bash
   # Semantics: "older-bead blocks newer-bead" — completing the older unblocks the newer
   bd dep <older-bead-id> --blocks <newer-bead-id> --db ~/projects/.beads/dolt
   ```
   This ensures same-repo PRs are processed sequentially (oldest first) to avoid rebase conflicts.

4. **Skip PRs**: Only create beads for needs-work PRs. Skipped PRs are tracked in the triage table but get no bead — they appear in the Phase 5 summary under "Skipped".

5. **Display the DAG**:
   ```bash
   bd graph <epic-id> --db ~/projects/.beads/dolt
   ```

## Phase 4: Execute

Maintain a set of **stuck bead IDs** (starts empty). Loop until no actionable ready beads remain:

1. **Find ready beads**:
   ```bash
   bd list --ready --parent <epic-id> --db ~/projects/.beads/dolt --json
   ```
   Ready beads have no unresolved blockers and can be worked on now. **Filter out any bead IDs in the stuck set** — do not re-dispatch beads that already failed this session.

2. **Check repo existence**: For each ready bead, verify `~/projects/<repo>` exists. If not, close the bead with an error:
   ```bash
   bd close <bead-id> --reason "Repo not cloned at ~/projects/<repo>" --db ~/projects/.beads/dolt
   ```
   Continue to next ready bead.

3. **Dispatch subagents**: For each ready bead, launch a subagent via the Task tool with:
   - `subagent_type`: `"general-purpose"`
   - `model`: `"sonnet"` (sufficient for rebase/fix work, faster than opus)

   **Cross-repo beads that are ready simultaneously MUST be dispatched in parallel** (multiple Task calls in one message). Same-repo ordering is already enforced by the dependency chain.

   Each subagent prompt MUST include:

   ```
   You are fixing a PR that needs work. Here is your assignment:

   ## Context
   - Repo path: ~/projects/<repo>
   - Feature branch: <head-ref>
   - Base branch: <base-ref>
   - PR number: #<number> in ifit/<repo>

   ## Problem
   <triage reason and all pre-fetched feedback — review comments and/or CI failure logs>

   ## Instructions — follow these steps exactly

   1. **Fetch and rebase**:
      ```
      cd ~/projects/<repo>
      git fetch origin
      git checkout <head-ref>
      ```
      Before rebasing, capture the pre-rebase diff for comparison:
      ```
      git diff origin/<base-ref>...<head-ref> > /tmp/pre-rebase.diff
      ```
      Now rebase:
      ```
      git rebase origin/<base-ref>
      ```
      If there are rebase conflicts, resolve them by preserving the feature branch's intentional changes. When in doubt, keep the feature branch version — the base branch changes are additions, not replacements for your work.

   2. **Verify the rebase didn't lose changes**:
      ```
      git diff origin/<base-ref>...HEAD > /tmp/post-rebase.diff
      ```
      Compare the two diffs. Every intentional change from the feature branch must still be present. If the rebase dropped or mangled any changes, fix them NOW before proceeding. This is critical — a bad conflict resolution that silently drops code is worse than a failed rebase.

   3. **Read the feedback**: The review comments and/or CI failure logs are included above. Understand what needs to change.

   4. **Make fixes**: Address the feedback. For each fix, read the current state of the file first. If a fix is already applied (e.g., a previous commit on the branch already addressed it), skip it and note that in your report. Do not re-apply changes that are already correct.

   5. **Determine verification commands**:
      - FIRST: Check if ~/projects/<repo>/CLAUDE.md exists and read it for test/lint/build commands.
      - FALLBACK: If no CLAUDE.md, parse .github/workflows/*.yml files in the repo to find the test, lint, and build commands used in CI.
      - Extract the actual commands (npm test, npm run lint, go test ./..., etc.)

   6. **Run verification**: Execute the test, lint, and build commands you found.
      - You MUST see them pass (exit code 0, green output) before proceeding.
      - If they fail, analyze the failure, make additional fixes, and re-run.
      - If you are stuck after reasonable effort (3+ attempts), STOP. Report exactly what is failing and why. Do NOT claim success. Do NOT close without evidence of passing verification.

   7. **Commit locally**: Stage and commit your changes with a clear commit message describing what was fixed. Do NOT push. Do NOT add Co-Authored-By trailers.

   8. **Report**: When done, provide:
      - What was done (summary of changes)
      - Rebase verification results (any changes lost/recovered?)
      - What was verified (which commands ran, their output summary)
      - Files changed (list)
      - Status: SUCCESS or STUCK (with details of what's failing)
   ```

4. **Process subagent results**:
   - **SUCCESS**: Close the bead:
     ```bash
     bd close <bead-id> --reason "Fixed and verified locally" --db ~/projects/.beads/dolt
     ```
   - **STUCK**: Add the bead ID to the stuck set. Leave the bead open. Add a comment with the failure details:
     ```bash
     bd comments add <bead-id> "Subagent stuck: <failure details>" --db ~/projects/.beads/dolt
     ```

5. **Repeat**: After processing results, check for newly ready beads (closing a blocker may unblock the next same-repo PR). Filter out stuck bead IDs. Continue until `bd list --ready` returns no results that aren't in the stuck set.

## Phase 5: Batch Confirm

1. **Collect all results** from the session. Categorize every PR into one of three groups:
   - **Ready to force-push**: Subagent reported SUCCESS
   - **Stuck (needs manual attention)**: Subagent reported STUCK or bead is still open
   - **Skipped (no action needed)**: PR was classified as skip in triage

2. **Present the summary**:

   ```
   ## Update PRs — Summary

   ### Ready to Force-Push
   | Repo | PR | Branch → Base | What Was Done | Verified |
   |------|----|---------------|---------------|----------|
   | repo-name | #123 | feat/auth → test | Fixed lint errors, resolved merge conflicts | npm test ✓, npm run lint ✓ |

   ### Stuck (Needs Manual Attention)
   | Repo | PR | Branch | Issue |
   |------|----|--------|-------|
   | other-repo | #789 | feat/thing | Test timeout in integration suite — needs DB fixture |

   ### Skipped (No Action Needed)
   | Repo | PR | Title |
   |------|----|-------|
   | clean-repo | #456 | Add logging |
   ```

3. **Ask for confirmation** using `AskUserQuestion` with these options:
   - **Push all** — Force-push all ready PRs
   - **Push selected** — Let the user pick which ones to push
   - **Abort** — Do not push anything

4. **Execute pushes**:
   - **Push all**: For each ready-to-push PR:
     ```bash
     git -C ~/projects/<repo> push --force-with-lease origin <head-ref>
     ```
   - **Push selected**: Use `AskUserQuestion` with `multiSelect: true` to present each ready PR as a selectable option (e.g., "ifit#21915 — feat/branch → test"). Push only the ones the user selects.
   - **Abort**: Skip pushing entirely.

5. **Close the epic** after pushes complete:
   ```bash
   bd close <epic-id> --reason "All PRs updated and pushed" --db ~/projects/.beads/dolt
   ```
   If the user chose **Abort**, still close the epic with reason "Aborted — no pushes made. Changes remain local." The next `/update-prs` run creates a fresh epic, so leaving old ones open just creates clutter.

## Important Notes

- Always pass `--db ~/projects/.beads/dolt` to every `bd` command. Every single one.
- Use `--force-with-lease` for pushes, never `--force`.
- Never add `Co-Authored-By` trailers to any commits.
- If a repo directory doesn't exist at `~/projects/<repo>`, close its bead with an error note and move on.
- Cross-repo beads that are ready at the same time MUST be dispatched in parallel (multiple Task calls in one message) for performance.
- Same-repo PRs are serialized by the dependency chain — the oldest PR is fixed first, then the next one becomes ready.
- Subagents must use `general-purpose` type since they need full tool access (Bash, Read, Edit, etc.) to fix code.
- The beads issue prefix is `projects` (IDs look like `projects-a3f2dd`).
- Truncate CI failure logs to a reasonable size (last 200 lines) when embedding in bead descriptions and subagent prompts — full logs can be enormous.
- When dispatching subagents, include ALL the pre-fetched context (review comments, CI logs) directly in the prompt so subagents don't need to re-fetch from GitHub.
