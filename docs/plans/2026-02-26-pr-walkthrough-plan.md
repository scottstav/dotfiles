# PR Walkthrough Feature Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add an interactive "Walk through changes" action to `/review-queue` that shows filtered file-by-file changes with entry point tracing and Emacs integration.

**Architecture:** Enhance `pr-code-reviewer` agent to output structured walkthrough data (entry points, noise classification) alongside its existing human-readable output. Add a new "Walk through changes" action to `review-queue` that parses this data and offers interactive Emacs file opening.

**Tech Stack:** Claude Code agent/command markdown files, MCP GitHub tools, Bash (git, emacsclient)

---

### Task 1: Enhance pr-code-reviewer with entry point identification

**Files:**
- Modify: `~/.claude/agents/pr-code-reviewer.md` (symlinked from `~/dotfiles/.claude/agents/pr-code-reviewer.md`)

**Step 1: Add entry point instructions to the workflow section**

In `pr-code-reviewer.md`, after step 4 ("Analyze the diff thoroughly.") on line 14, add a new step 5:

```markdown
5. For each finding, identify the **entry point** — the API route, controller method, event handler, or public function that a human reviewer would start from to trace the code path containing the issue. If the finding is self-contained (e.g., a model schema issue), the entry point is the finding's own location.
```

**Step 2: Add noise file classification instructions**

After the "What to Look For" section (after line 37), add:

```markdown
**Noise File Classification:**

Classify each changed file as "noise" or "relevant":
- **Noise** (auto-filtered in walkthrough): test files (`*.test.*`, `*.spec.*`, `__tests__/`, `test/`), lock files (`package-lock.json`, `yarn.lock`, `pnpm-lock.yaml`), swagger/openapi specs (`swagger.*`, `openapi.*`), generated files (`*.generated.*`, `*.g.*`), README/docs/changelog (`README*`, `CHANGELOG*`, `docs/`), terraform `.tfvars` files, files with only whitespace/formatting changes, CI config (`.github/workflows/`, `.gitlab-ci.yml`, `Jenkinsfile`), Dockerfiles, linter configs (`.eslintrc*`, `.prettierrc*`)
- **Relevant**: everything else
```

**Step 3: Add structured output section**

After the existing "Summary" output format section (after line 54), and before the "Guidelines" section, add:

````markdown
## Walkthrough Data

After the human-readable review above, emit a fenced JSON block with structured data for the interactive walkthrough. This block is consumed programmatically by the review-queue command.

```json
{
  "prBranch": "feature/branch-name-from-pr-details",
  "findings": [
    {
      "id": 1,
      "severity": "CRITICAL",
      "file": "src/path/to/file.ts",
      "line": 42,
      "summary": "Short description of the issue",
      "entryPoint": {
        "file": "src/path/to/entry.ts",
        "line": 15,
        "label": "POST /api/resource"
      },
      "traceDescription": "Start at [entry point]. Follow [call chain]. The issue is at [location] because [reason]."
    }
  ],
  "noiseFiles": ["package-lock.json", "swagger.json"],
  "relevantFiles": [
    { "file": "src/path/to/file.ts", "changeType": "modified" }
  ]
}
```

Rules for the walkthrough data:
- `prBranch`: Extract the head branch name from the PR details (the branch to checkout locally).
- `findings`: One entry per finding from the review. `id` is sequential starting at 1.
- `entryPoint.label`: A short human-readable label (e.g., "POST /api/users", "UserService.create()", "main()").
- `traceDescription`: A 1-3 sentence walkthrough telling the reviewer how to manually trace from the entry point to the issue. Be specific about function names and call chains.
- If a finding has no external entry point (issue is self-contained), set `entryPoint` to the same file/line as the finding itself and set `label` to "(same file)".
- `noiseFiles`: List of changed files classified as noise.
- `relevantFiles`: List of changed files classified as relevant, with `changeType` being "added", "modified", "removed", or "renamed".
- If there are no findings, `findings` should be an empty array. Still populate `noiseFiles` and `relevantFiles`.
````

**Step 4: Commit**

```bash
cd ~/dotfiles && git add .claude/agents/pr-code-reviewer.md && git commit -m "feat(pr-reviewer): add entry point tracing and structured walkthrough output"
```

---

### Task 2: Add "Walk through changes" action to review-queue

**Files:**
- Modify: `~/.claude/commands/review-queue.md` (symlinked from `~/dotfiles/.claude/commands/review-queue.md`)

**Step 1: Update Step 2 to store code reviewer output**

In `review-queue.md`, in the Step 2 section (line 60-77), add an instruction after "present their outputs under clear headers":

```markdown
**Important:** Store the code reviewer agent's raw output in a variable — the walkthrough data JSON block will be needed if the user selects "Walk through changes" in Step 3.
```

**Step 2: Update Step 3 action menu to include "Walk through changes"**

Replace the action menu options block (lines 92-95) with:

```markdown
- **Approve** — Approve the PR (notes become your review comment)
- **Request changes** — Request changes (notes become your review comment)
- **Walk through changes** — Interactive file-by-file walkthrough with Emacs integration
- **Skip** — Move to the next PR without action
```

Note: This replaces "Generate test file" as the third option. "Generate test file" moves to being accessible via the "Other" free-form input (user types "test file" or "generate test file").

**Step 3: Add "test file" to the free-form input interpretation**

After the existing "Other" interpretation rules (lines 97-101), add a bullet:

```markdown
- If it says **"test file"** or **"generate test file"** — treat as Generate test file request. See Step 3c.
```

**Step 4: Add Step 3e — Walk through changes**

After the Step 3d section (after line 131), add the new section:

````markdown
### Step 3e: Walk through changes

When the user selects "Walk through changes":

1. **Parse walkthrough data** from the code reviewer's output. Find the `## Walkthrough Data` section and extract the JSON block.

2. **Display filtered file summary:**
   ```
   ### Changed Files ({N} relevant, {M} noise filtered out)

   Filtered out: {comma-separated noise file names}

   | # | File | Change | Findings |
   |---|------|--------|----------|
   | 1 | src/path/to/file.ts | modified | 1 CRITICAL |
   | 2 | src/path/to/other.ts | added | — |
   ```

   List only relevant files. Show finding counts per file with severity.

3. **Display numbered findings with entry points:**
   ```
   ### Findings

   1. [CRITICAL] file.ts:42 — Short description
      Entry: src/routes/resource.route.ts:15 (POST /api/resource)
      Trace: Start at POST route → service.create() → hook at :42

   2. [IMPORTANT] service.ts:88 — Short description
      Entry: src/routes/resource.route.ts:45 (PUT /api/resource/bulk)
      Trace: PUT route → service.bulkUpdate() → forEach at :88
   ```

4. **Ask which findings to open** using `AskUserQuestion`:
   - **Open all entry points** — Opens every finding's entry point in Emacs
   - **Pick specific** — User types finding numbers (e.g. "1,3") via "Other"
   - **Done** — Return to Step 3 action menu

5. **For each selected finding, execute the open sequence:**

   a. **Checkout PR branch** (only once per walkthrough, skip if already on it):
      ```bash
      cd ~/projects/{repo} && git fetch origin {prBranch} && git checkout {prBranch}
      ```
      If `~/projects/{repo}` doesn't exist, display a warning: `"Local repo not found at ~/projects/{repo}. Skipping Emacs open."` and skip the open steps.

   b. **Open entry point in Emacs:**
      ```bash
      emacsclient -c +{entryPoint.line} ~/projects/{repo}/{entryPoint.file}
      ```

   c. **Display trace description:**
      ```
      Opened {entryPoint.file}:{entryPoint.line} ({entryPoint.label})

      What to trace: {traceDescription}
      ```

   d. Repeat for each selected finding. Open each in a separate Emacs frame.

6. **Return to Step 3** — re-display notes and action menu. The user can walk through again, take notes, deep dive, or choose a final action.
````

**Step 5: Commit**

```bash
cd ~/dotfiles && git add .claude/commands/review-queue.md && git commit -m "feat(review-queue): add interactive 'Walk through changes' action with Emacs integration"
```

---

### Task 3: Run sync script

**Step 1: Sync dotfiles**

```bash
~/dotfiles/sync-claude.sh
```

This ensures the symlinks are up to date and changes are adopted.

**Step 2: Verify symlinks point to updated files**

```bash
cat ~/.claude/agents/pr-code-reviewer.md | grep "Walkthrough Data"
cat ~/.claude/commands/review-queue.md | grep "Walk through changes"
```

Both should return matches confirming the new content is live.

**Step 3: Commit any sync changes if needed**

```bash
cd ~/dotfiles && git status && git diff
```

If there are additional changes from the sync script, commit them.
