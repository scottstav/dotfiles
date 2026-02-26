# PR Walkthrough Feature Design

## Problem

The pr-code-reviewer agent reports issues abstractly (file:line + description) but doesn't give enough context to quickly understand or verify findings. The reviewer needs to:
- See which files actually matter (filtering out noise)
- Understand how to trace a finding from its entry point (API route, handler) to the issue
- Jump into the code at the right spot in Emacs to verify manually

## Solution

Two changes:

### 1. Enhanced pr-code-reviewer output

Add structured walkthrough data to the reviewer's output. The human-readable review stays identical (no regression). A new `## Walkthrough Data` section is appended with a fenced JSON block.

**Entry point identification:** For each finding, the reviewer traces back to the entry point a human would start from — the API route, controller method, event handler, or public function that triggers the code path containing the issue.

**Noise file classification:** Changed files are categorized:
- **Noise** (auto-filtered): test files (`*.test.*`, `*.spec.*`, `__tests__/`), lock files (`package-lock.json`, `yarn.lock`), swagger/openapi specs, generated files, README/docs/changelog, terraform `.tfvars`, whitespace-only changes, CI config files
- **Relevant**: everything else

**Structured output format:**

```json
{
  "findings": [
    {
      "id": 1,
      "severity": "CRITICAL|IMPORTANT|SUGGESTION",
      "file": "src/hooks/equipment.hook.ts",
      "line": 42,
      "summary": "Save hook doesn't validate equipment type before write",
      "entryPoint": {
        "file": "src/routes/equipment.route.ts",
        "line": 15,
        "description": "POST /api/equipment"
      },
      "traceDescription": "Start at the POST route handler. Follow the call to equipmentService.create(), which calls Equipment.save(). The pre-save hook at line 42 fires but doesn't check equipment.type, so invalid types get written."
    }
  ],
  "noiseFiles": ["package-lock.json", "swagger.json", "src/__tests__/equipment.test.ts"],
  "relevantFiles": [
    { "file": "src/hooks/equipment.hook.ts", "changeType": "modified" },
    { "file": "src/routes/equipment.route.ts", "changeType": "modified" }
  ]
}
```

If a finding has no external entry point (e.g., a model schema suggestion), `entryPoint` points to the finding's own file/line.

### 2. New "Walk through changes" action in review-queue

Added as a new option in the Step 3 action menu (alongside Approve, Request changes, Generate test file, Skip).

**Flow when selected:**

1. Parse the `## Walkthrough Data` JSON from the code reviewer's output.

2. Display filtered file summary:
   ```
   ### Changed Files (5 relevant, 3 noise filtered out)
   Filtered out: package-lock.json, swagger.json, equipment.test.ts

   | # | File | Change | Findings |
   |---|------|--------|----------|
   | 1 | src/hooks/equipment.hook.ts | modified | 1 CRITICAL |
   | 2 | src/routes/equipment.route.ts | modified | — |
   | 3 | src/services/equipment.service.ts | modified | 1 IMPORTANT |
   ```

3. Display numbered findings with entry points:
   ```
   ### Findings

   1. [CRITICAL] equipment.hook.ts:42 — Save hook doesn't validate equipment type
      Entry: src/routes/equipment.route.ts:15 (POST /api/equipment)
      Trace: POST route → equipmentService.create() → Equipment.save() → pre-save hook :42

   2. [IMPORTANT] equipment.service.ts:88 — Missing error handling on bulk update
      Entry: src/routes/equipment.route.ts:45 (PUT /api/equipment/bulk)
      Trace: PUT route → service.bulkUpdate() → forEach without try/catch :88
   ```

4. Ask which to open via `AskUserQuestion`:
   - **Open all entry points** — opens each finding's entry point in a new Emacs frame
   - **Pick specific ones** — user types numbers (e.g. "1,2")
   - **Done** — return to Step 3 action menu

5. For each selected finding:
   ```bash
   # Checkout PR branch (if not already on it)
   cd ~/projects/{repo} && git fetch origin {branch} && git checkout {branch}
   # Open entry point in Emacs
   emacsclient -c +{line} {file}
   ```

6. After opening, display the trace description:
   ```
   Opened src/routes/equipment.route.ts:15

   What to trace: Start at the POST route handler. Follow the call to
   equipmentService.create(), which calls Equipment.save(). The pre-save
   hook at line 42 fires but doesn't check equipment.type.
   ```

7. Return to Step 3 action menu (notes preserved, can walk through again).

## Local repo convention

Repos are assumed to be at `~/projects/{repo-name}/`. If the directory doesn't exist, inform the user and skip the Emacs open step.

## Files to modify

1. `~/.claude/agents/pr-code-reviewer.md` — Add entry point identification instructions, noise file classification, and structured JSON output section
2. `~/.claude/commands/review-queue.md` — Add "Walk through changes" as Step 3e action option with the interactive flow described above
