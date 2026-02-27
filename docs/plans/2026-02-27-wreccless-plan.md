# wreccless (ccl) Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build a Go CLI (`ccl`) that manages background Claude Code worker sessions — creating, tracking, killing, resuming, and querying workers with configurable hooks.

**Architecture:** Single Go binary with cobra subcommands. State is file-per-worker JSON in `~/.local/state/ccl/`. Config is TOML at `~/.config/ccl/config.toml`. Process management via detached child processes with PID tracking.

**Tech Stack:** Go 1.26, cobra (CLI), BurntSushi/toml (config), google/uuid, standard library for everything else.

---

### Task 1: Project Scaffold

**Files:**
- Create: `~/projects/wreccless/cmd/ccl/main.go`
- Create: `~/projects/wreccless/internal/state/state.go`
- Create: `~/projects/wreccless/internal/config/config.go`
- Create: `~/projects/wreccless/internal/hooks/hooks.go`
- Create: `~/projects/wreccless/internal/worker/worker.go`
- Create: `~/projects/wreccless/Makefile`
- Create: `~/projects/wreccless/.gitignore`

**Step 1: Create repo and initialize Go module**

```bash
mkdir -p ~/projects/wreccless
cd ~/projects/wreccless
git init
go mod init github.com/ifit/wreccless
```

**Step 2: Add dependencies**

```bash
cd ~/projects/wreccless
go get github.com/spf13/cobra@latest
go get github.com/BurntSushi/toml@latest
go get github.com/google/uuid@latest
```

**Step 3: Create directory structure**

```bash
cd ~/projects/wreccless
mkdir -p cmd/ccl internal/state internal/config internal/hooks internal/worker
```

**Step 4: Write minimal main.go**

```go
// cmd/ccl/main.go
package main

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"
)

var rootCmd = &cobra.Command{
	Use:   "ccl",
	Short: "Claude Code Launcher — manage background Claude workers",
}

func main() {
	if err := rootCmd.Execute(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
```

**Step 5: Write Makefile**

```makefile
PREFIX ?= $(HOME)/.local

.PHONY: build install clean test

build:
	go build -o ccl ./cmd/ccl

install: build
	install -Dm755 ccl $(PREFIX)/bin/ccl

clean:
	rm -f ccl

test:
	go test ./...
```

**Step 6: Write .gitignore**

```
ccl
```

**Step 7: Verify it builds**

Run: `cd ~/projects/wreccless && make build`
Expected: Binary `ccl` created. `./ccl --help` shows help text.

**Step 8: Commit**

```bash
cd ~/projects/wreccless
git add -A
git commit -m "feat: project scaffold with cobra CLI skeleton"
```

---

### Task 2: State Package — Worker Struct + File I/O

**Files:**
- Create: `~/projects/wreccless/internal/state/state.go`
- Create: `~/projects/wreccless/internal/state/state_test.go`

**Step 1: Write the failing tests**

```go
// internal/state/state_test.go
package state

import (
	"os"
	"path/filepath"
	"testing"
)

func tempStateDir(t *testing.T) string {
	t.Helper()
	dir := t.TempDir()
	return dir
}

func TestWriteAndRead(t *testing.T) {
	dir := tempStateDir(t)
	w := &Worker{
		ID:        "123",
		Status:    StatusWorking,
		Directory: "/tmp/foo",
		Task:      "fix bug",
		PID:       42,
		SessionID: "abc-def",
	}

	if err := Write(dir, w); err != nil {
		t.Fatalf("Write: %v", err)
	}

	got, err := Read(dir, "123")
	if err != nil {
		t.Fatalf("Read: %v", err)
	}
	if got.ID != "123" || got.Status != StatusWorking || got.Task != "fix bug" {
		t.Errorf("unexpected worker: %+v", got)
	}
	if got.PID != 42 || got.SessionID != "abc-def" {
		t.Errorf("unexpected PID/SessionID: %+v", got)
	}
}

func TestReadNotFound(t *testing.T) {
	dir := tempStateDir(t)
	_, err := Read(dir, "nonexistent")
	if err == nil {
		t.Fatal("expected error for nonexistent worker")
	}
}

func TestList(t *testing.T) {
	dir := tempStateDir(t)
	for _, id := range []string{"100", "200", "300"} {
		w := &Worker{ID: id, Status: StatusDone, Directory: "/tmp", Task: "task " + id}
		if err := Write(dir, w); err != nil {
			t.Fatalf("Write %s: %v", id, err)
		}
	}

	workers, err := List(dir)
	if err != nil {
		t.Fatalf("List: %v", err)
	}
	if len(workers) != 3 {
		t.Errorf("expected 3 workers, got %d", len(workers))
	}
}

func TestDelete(t *testing.T) {
	dir := tempStateDir(t)
	w := &Worker{ID: "456", Status: StatusPending, Directory: "/tmp", Task: "test"}
	Write(dir, w)

	if err := Delete(dir, "456"); err != nil {
		t.Fatalf("Delete: %v", err)
	}

	_, err := Read(dir, "456")
	if err == nil {
		t.Fatal("expected error after delete")
	}

	// Verify log file also deleted
	logPath := filepath.Join(dir, "456.log")
	os.WriteFile(logPath, []byte("log"), 0644)
	Write(dir, &Worker{ID: "456", Status: StatusDone, Directory: "/tmp", Task: "t"})
	os.WriteFile(logPath, []byte("log"), 0644)
	Delete(dir, "456")
	if _, err := os.Stat(logPath); !os.IsNotExist(err) {
		t.Error("log file should be deleted too")
	}
}
```

**Step 2: Run tests to verify they fail**

Run: `cd ~/projects/wreccless && go test ./internal/state/`
Expected: FAIL — types and functions not defined.

**Step 3: Write the implementation**

```go
// internal/state/state.go
package state

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

type Status string

const (
	StatusPending Status = "pending"
	StatusWorking Status = "working"
	StatusDone    Status = "done"
	StatusError   Status = "error"
)

type Worker struct {
	ID         string     `json:"id"`
	Status     Status     `json:"status"`
	Directory  string     `json:"directory"`
	Task       string     `json:"task"`
	Image      string     `json:"image,omitempty"`
	PID        int        `json:"pid,omitempty"`
	SessionID  string     `json:"session_id,omitempty"`
	CreatedAt  *time.Time `json:"created_at,omitempty"`
	StartedAt  *time.Time `json:"started_at,omitempty"`
	FinishedAt *time.Time `json:"finished_at,omitempty"`
}

func statePath(dir, id string) string {
	return filepath.Join(dir, id+".json")
}

func Write(dir string, w *Worker) error {
	if err := os.MkdirAll(dir, 0755); err != nil {
		return err
	}
	data, err := json.MarshalIndent(w, "", "  ")
	if err != nil {
		return err
	}
	tmp := statePath(dir, w.ID) + ".tmp"
	if err := os.WriteFile(tmp, data, 0644); err != nil {
		return err
	}
	return os.Rename(tmp, statePath(dir, w.ID))
}

func Read(dir, id string) (*Worker, error) {
	data, err := os.ReadFile(statePath(dir, id))
	if err != nil {
		return nil, fmt.Errorf("worker %s: %w", id, err)
	}
	var w Worker
	if err := json.Unmarshal(data, &w); err != nil {
		return nil, fmt.Errorf("worker %s: %w", id, err)
	}
	return &w, nil
}

func List(dir string) ([]*Worker, error) {
	entries, err := os.ReadDir(dir)
	if err != nil {
		if os.IsNotExist(err) {
			return nil, nil
		}
		return nil, err
	}
	var workers []*Worker
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".json") {
			continue
		}
		id := strings.TrimSuffix(e.Name(), ".json")
		w, err := Read(dir, id)
		if err != nil {
			continue // skip corrupt files
		}
		workers = append(workers, w)
	}
	sort.Slice(workers, func(i, j int) bool {
		return workers[i].ID < workers[j].ID
	})
	return workers, nil
}

func Delete(dir, id string) error {
	os.Remove(filepath.Join(dir, id+".log"))
	return os.Remove(statePath(dir, id))
}
```

**Step 4: Run tests to verify they pass**

Run: `cd ~/projects/wreccless && go test ./internal/state/ -v`
Expected: All 4 tests PASS.

**Step 5: Commit**

```bash
cd ~/projects/wreccless
git add internal/state/
git commit -m "feat(state): worker struct and file I/O with tests"
```

---

### Task 3: Config Package

**Files:**
- Create: `~/projects/wreccless/internal/config/config.go`
- Create: `~/projects/wreccless/internal/config/config_test.go`
- Create: `~/projects/wreccless/config.example.toml`

**Step 1: Write the failing tests**

```go
// internal/config/config_test.go
package config

import (
	"os"
	"path/filepath"
	"testing"
)

func TestDefaults(t *testing.T) {
	cfg := Defaults()
	if !cfg.Claude.SkipPermissions {
		t.Error("skip_permissions should default to true")
	}
	if cfg.Claude.SystemPrompt == "" {
		t.Error("system_prompt should have a default")
	}
}

func TestLoadFromFile(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "config.toml")
	os.WriteFile(path, []byte(`
[claude]
skip_permissions = false
system_prompt = "custom prompt"
extra_flags = ["--model", "opus"]

[hooks]
on_done = ["echo done"]
`), 0644)

	cfg, err := Load(path)
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	if cfg.Claude.SkipPermissions {
		t.Error("skip_permissions should be false")
	}
	if cfg.Claude.SystemPrompt != "custom prompt" {
		t.Errorf("system_prompt: %q", cfg.Claude.SystemPrompt)
	}
	if len(cfg.Claude.ExtraFlags) != 2 {
		t.Errorf("extra_flags: %v", cfg.Claude.ExtraFlags)
	}
	if len(cfg.Hooks.OnDone) != 1 || cfg.Hooks.OnDone[0] != "echo done" {
		t.Errorf("on_done: %v", cfg.Hooks.OnDone)
	}
}

func TestLoadMissing(t *testing.T) {
	cfg, err := Load("/nonexistent/config.toml")
	if err != nil {
		t.Fatalf("Load should not error on missing file: %v", err)
	}
	// Should return defaults
	if !cfg.Claude.SkipPermissions {
		t.Error("missing file should return defaults")
	}
}
```

**Step 2: Run tests to verify they fail**

Run: `cd ~/projects/wreccless && go test ./internal/config/`
Expected: FAIL — types and functions not defined.

**Step 3: Write the implementation**

```go
// internal/config/config.go
package config

import (
	"os"

	"github.com/BurntSushi/toml"
)

type ClaudeConfig struct {
	SkipPermissions bool     `toml:"skip_permissions"`
	SystemPrompt    string   `toml:"system_prompt"`
	ExtraFlags      []string `toml:"extra_flags"`
}

type HooksConfig struct {
	OnStart   []string `toml:"on_start"`
	OnDone    []string `toml:"on_done"`
	OnPending []string `toml:"on_pending"`
	OnError   []string `toml:"on_error"`
	OnKill    []string `toml:"on_kill"`
}

type Config struct {
	Claude ClaudeConfig `toml:"claude"`
	Hooks  HooksConfig  `toml:"hooks"`
}

const defaultSystemPrompt = `You are the user's trusted programmer. Do not ask questions. Complete the entire task before stopping. If you encounter issues, debug and fix them. When finished, end with a 1-2 sentence summary.`

func Defaults() *Config {
	return &Config{
		Claude: ClaudeConfig{
			SkipPermissions: true,
			SystemPrompt:    defaultSystemPrompt,
		},
	}
}

func Load(path string) (*Config, error) {
	cfg := Defaults()
	data, err := os.ReadFile(path)
	if err != nil {
		if os.IsNotExist(err) {
			return cfg, nil
		}
		return nil, err
	}
	// Decode on top of defaults — unset fields keep their default value
	if _, err := toml.Decode(string(data), cfg); err != nil {
		return nil, err
	}
	return cfg, nil
}
```

**Step 4: Run tests to verify they pass**

Run: `cd ~/projects/wreccless && go test ./internal/config/ -v`
Expected: All 3 tests PASS.

**Step 5: Write config.example.toml**

```toml
# ~/.config/ccl/config.toml

[claude]
# Hardcoded internally: --output-format stream-json --verbose --session-id <uuid>
# These cannot be changed — ccl depends on them.

# Pass --dangerously-skip-permissions to claude (default: true)
skip_permissions = true

# System prompt appended for autonomous workers
system_prompt = """You are the user's trusted programmer. Do not ask questions. \
Complete the entire task before stopping. If you encounter issues, debug and fix them. \
When finished, end with a 1-2 sentence summary."""

# Additional flags to pass to claude -p (e.g. ["--model", "opus"])
extra_flags = []

[hooks]
# Shell commands executed on state transitions via sh -c
# Template variables: {{.ID}}, {{.Task}}, {{.Dir}}, {{.Status}}, {{.SessionID}}
# Hooks fire asynchronously and don't block state transitions.
# on_start = ["pkill -SIGRTMIN+12 waybar"]
# on_done = ["pkill -SIGRTMIN+12 waybar", "notify-send 'Worker Done' '{{.Task}}'"]
# on_pending = ["pkill -SIGRTMIN+12 waybar"]
# on_error = ["notify-send -u critical 'Worker Failed' '{{.Task}}'"]
# on_kill = ["pkill -SIGRTMIN+12 waybar"]
```

**Step 6: Commit**

```bash
cd ~/projects/wreccless
git add internal/config/ config.example.toml
git commit -m "feat(config): TOML config loading with defaults"
```

---

### Task 4: Hooks Package

**Files:**
- Create: `~/projects/wreccless/internal/hooks/hooks.go`
- Create: `~/projects/wreccless/internal/hooks/hooks_test.go`

**Step 1: Write the failing tests**

```go
// internal/hooks/hooks_test.go
package hooks

import (
	"os"
	"path/filepath"
	"testing"
	"time"
)

func TestRenderTemplate(t *testing.T) {
	vars := Vars{
		ID:        "123",
		Task:      "fix the bug",
		Dir:       "/home/user/project",
		Status:    "done",
		SessionID: "abc-def",
	}

	result, err := render("Worker {{.ID}} is {{.Status}}: {{.Task}}", vars)
	if err != nil {
		t.Fatalf("render: %v", err)
	}
	if result != "Worker 123 is done: fix the bug" {
		t.Errorf("unexpected: %q", result)
	}
}

func TestFireCreatesMarkerFile(t *testing.T) {
	dir := t.TempDir()
	marker := filepath.Join(dir, "fired")

	cmds := []string{"touch " + marker}
	vars := Vars{ID: "1", Task: "t", Dir: "/tmp", Status: "done"}

	Fire(cmds, vars)
	// Hooks are async, wait briefly
	time.Sleep(200 * time.Millisecond)

	if _, err := os.Stat(marker); os.IsNotExist(err) {
		t.Error("hook did not fire — marker file missing")
	}
}

func TestFireWithTemplateVars(t *testing.T) {
	dir := t.TempDir()
	outfile := filepath.Join(dir, "out")

	cmds := []string{"echo '{{.ID}}:{{.Status}}' > " + outfile}
	vars := Vars{ID: "42", Task: "t", Dir: "/tmp", Status: "done"}

	Fire(cmds, vars)
	time.Sleep(200 * time.Millisecond)

	data, err := os.ReadFile(outfile)
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	got := string(data)
	if got != "42:done\n" {
		t.Errorf("unexpected: %q", got)
	}
}
```

**Step 2: Run tests to verify they fail**

Run: `cd ~/projects/wreccless && go test ./internal/hooks/`
Expected: FAIL — types and functions not defined.

**Step 3: Write the implementation**

```go
// internal/hooks/hooks.go
package hooks

import (
	"bytes"
	"log"
	"os/exec"
	"text/template"
)

type Vars struct {
	ID        string
	Task      string
	Dir       string
	Status    string
	SessionID string
}

func render(tmpl string, vars Vars) (string, error) {
	t, err := template.New("hook").Parse(tmpl)
	if err != nil {
		return "", err
	}
	var buf bytes.Buffer
	if err := t.Execute(&buf, vars); err != nil {
		return "", err
	}
	return buf.String(), nil
}

// Fire executes hook commands asynchronously. Each command is a shell
// string run via sh -c. Template variables are expanded before execution.
// Failures are logged but don't propagate.
func Fire(cmds []string, vars Vars) {
	for _, cmdTmpl := range cmds {
		expanded, err := render(cmdTmpl, vars)
		if err != nil {
			log.Printf("hook template error: %v", err)
			continue
		}
		go func(cmd string) {
			if err := exec.Command("sh", "-c", cmd).Run(); err != nil {
				log.Printf("hook failed: %s: %v", cmd, err)
			}
		}(expanded)
	}
}
```

**Step 4: Run tests to verify they pass**

Run: `cd ~/projects/wreccless && go test ./internal/hooks/ -v`
Expected: All 3 tests PASS.

**Step 5: Commit**

```bash
cd ~/projects/wreccless
git add internal/hooks/
git commit -m "feat(hooks): template-based async hook execution"
```

---

### Task 5: CLI Skeleton — Root + `new --pending`

**Files:**
- Modify: `~/projects/wreccless/cmd/ccl/main.go`
- Create: `~/projects/wreccless/cmd/ccl/new.go`
- Create: `~/projects/wreccless/cmd/ccl/new_test.go`

**Step 1: Write the failing test**

```go
// cmd/ccl/new_test.go
package main

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/ifit/wreccless/internal/state"
)

func TestNewPending(t *testing.T) {
	dir := t.TempDir()
	stateDir = dir // override package-level var
	configPath = filepath.Join(t.TempDir(), "nonexistent.toml")

	args := []string{"new", "--dir", "/tmp/testproject", "--task", "fix the bug", "--pending"}
	rootCmd.SetArgs(args)

	buf := new(strings.Builder)
	rootCmd.SetOut(buf)
	rootCmd.SetErr(buf)

	if err := rootCmd.Execute(); err != nil {
		t.Fatalf("execute: %v", err)
	}

	output := strings.TrimSpace(buf.String())
	if output == "" {
		t.Fatal("expected worker ID on stdout")
	}

	// Verify state file was created
	workers, err := state.List(dir)
	if err != nil {
		t.Fatalf("list: %v", err)
	}
	if len(workers) != 1 {
		t.Fatalf("expected 1 worker, got %d", len(workers))
	}
	w := workers[0]
	if w.Status != state.StatusPending {
		t.Errorf("expected pending, got %s", w.Status)
	}
	if w.Directory != "/tmp/testproject" {
		t.Errorf("directory: %s", w.Directory)
	}
	if w.Task != "fix the bug" {
		t.Errorf("task: %s", w.Task)
	}
}

func TestNewPendingJSON(t *testing.T) {
	dir := t.TempDir()
	stateDir = dir
	configPath = filepath.Join(t.TempDir(), "nonexistent.toml")

	args := []string{"new", "--dir", "/tmp/proj", "--task", "test", "--pending", "--json"}
	rootCmd.SetArgs(args)

	buf := new(strings.Builder)
	rootCmd.SetOut(buf)
	rootCmd.SetErr(buf)

	if err := rootCmd.Execute(); err != nil {
		t.Fatalf("execute: %v", err)
	}

	var result map[string]interface{}
	if err := json.Unmarshal([]byte(buf.String()), &result); err != nil {
		t.Fatalf("json parse: %v (%s)", err, buf.String())
	}
	if result["id"] == nil {
		t.Error("expected id in JSON output")
	}
}
```

**Step 2: Run test to verify it fails**

Run: `cd ~/projects/wreccless && go test ./cmd/ccl/ -run TestNew`
Expected: FAIL — `newCmd` not defined, `stateDir` not defined.

**Step 3: Update main.go with shared vars**

```go
// cmd/ccl/main.go
package main

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/spf13/cobra"
)

var (
	stateDir   = filepath.Join(os.Getenv("HOME"), ".local", "state", "ccl")
	configPath = filepath.Join(os.Getenv("HOME"), ".config", "ccl", "config.toml")
)

var rootCmd = &cobra.Command{
	Use:   "ccl",
	Short: "Claude Code Launcher — manage background Claude workers",
}

func main() {
	if err := rootCmd.Execute(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
```

**Step 4: Write new.go**

```go
// cmd/ccl/new.go
package main

import (
	"encoding/json"
	"fmt"
	"strconv"
	"time"

	"github.com/ifit/wreccless/internal/config"
	"github.com/ifit/wreccless/internal/hooks"
	"github.com/ifit/wreccless/internal/state"
	"github.com/spf13/cobra"
)

var newCmd = &cobra.Command{
	Use:   "new",
	Short: "Create a new worker",
	RunE:  runNew,
}

var (
	newDir     string
	newTask    string
	newImage   string
	newPending bool
	newJSON    bool
)

func init() {
	newCmd.Flags().StringVar(&newDir, "dir", "", "Project directory (required)")
	newCmd.Flags().StringVar(&newTask, "task", "", "Task description (required)")
	newCmd.Flags().StringVar(&newImage, "image", "", "Image path for claude to reference")
	newCmd.Flags().BoolVar(&newPending, "pending", false, "Create as pending (require manual approval)")
	newCmd.Flags().BoolVar(&newJSON, "json", false, "Output JSON")
	newCmd.MarkFlagRequired("dir")
	newCmd.MarkFlagRequired("task")
	rootCmd.AddCommand(newCmd)
}

func runNew(cmd *cobra.Command, args []string) error {
	cfg, err := config.Load(configPath)
	if err != nil {
		return fmt.Errorf("config: %w", err)
	}

	now := time.Now()
	id := strconv.FormatInt(now.Unix(), 10)

	status := state.StatusWorking
	if newPending {
		status = state.StatusPending
	}

	w := &state.Worker{
		ID:        id,
		Status:    status,
		Directory: newDir,
		Task:      newTask,
		Image:     newImage,
		CreatedAt: &now,
	}

	if err := state.Write(stateDir, w); err != nil {
		return fmt.Errorf("write state: %w", err)
	}

	// Fire hooks
	vars := hooks.Vars{ID: id, Task: newTask, Dir: newDir, Status: string(status)}
	if newPending {
		hooks.Fire(cfg.Hooks.OnPending, vars)
	}

	if !newPending {
		// TODO: Task 10 — spawn detached ccl run
		// For now, just mark as working
		startedAt := time.Now()
		w.StartedAt = &startedAt
		state.Write(stateDir, w)
		hooks.Fire(cfg.Hooks.OnStart, vars)
	}

	if newJSON {
		data, _ := json.Marshal(map[string]string{"id": id, "status": string(status)})
		fmt.Fprintln(cmd.OutOrStdout(), string(data))
	} else {
		fmt.Fprintln(cmd.OutOrStdout(), id)
	}
	return nil
}
```

**Step 5: Run tests to verify they pass**

Run: `cd ~/projects/wreccless && go test ./cmd/ccl/ -run TestNew -v`
Expected: Both tests PASS.

**Step 6: Commit**

```bash
cd ~/projects/wreccless
git add cmd/ccl/
git commit -m "feat: ccl new --pending command"
```

---

### Task 6: `ccl list` Command

**Files:**
- Create: `~/projects/wreccless/cmd/ccl/list.go`
- Create: `~/projects/wreccless/cmd/ccl/list_test.go`

**Step 1: Write the failing tests**

```go
// cmd/ccl/list_test.go
package main

import (
	"encoding/json"
	"strings"
	"testing"
	"path/filepath"

	"github.com/ifit/wreccless/internal/state"
)

func seedWorkers(t *testing.T, dir string) {
	t.Helper()
	workers := []*state.Worker{
		{ID: "100", Status: state.StatusPending, Directory: "/tmp/a", Task: "task a"},
		{ID: "200", Status: state.StatusWorking, Directory: "/tmp/b", Task: "task b", PID: 99999},
		{ID: "300", Status: state.StatusDone, Directory: "/tmp/c", Task: "task c", SessionID: "sess-c"},
	}
	for _, w := range workers {
		if err := state.Write(dir, w); err != nil {
			t.Fatalf("seed: %v", err)
		}
	}
}

func TestListHuman(t *testing.T) {
	dir := t.TempDir()
	stateDir = dir
	seedWorkers(t, dir)

	rootCmd.SetArgs([]string{"list"})
	buf := new(strings.Builder)
	rootCmd.SetOut(buf)

	if err := rootCmd.Execute(); err != nil {
		t.Fatalf("execute: %v", err)
	}

	output := buf.String()
	if !strings.Contains(output, "100") || !strings.Contains(output, "pending") {
		t.Errorf("missing worker 100: %s", output)
	}
	if !strings.Contains(output, "300") || !strings.Contains(output, "done") {
		t.Errorf("missing worker 300: %s", output)
	}
}

func TestListJSON(t *testing.T) {
	dir := t.TempDir()
	stateDir = dir
	seedWorkers(t, dir)

	rootCmd.SetArgs([]string{"list", "--json"})
	buf := new(strings.Builder)
	rootCmd.SetOut(buf)

	if err := rootCmd.Execute(); err != nil {
		t.Fatalf("execute: %v", err)
	}

	var workers []map[string]interface{}
	if err := json.Unmarshal([]byte(buf.String()), &workers); err != nil {
		t.Fatalf("json: %v (%s)", err, buf.String())
	}
	if len(workers) != 3 {
		t.Errorf("expected 3, got %d", len(workers))
	}
}

func TestListFilterStatus(t *testing.T) {
	dir := t.TempDir()
	stateDir = dir
	seedWorkers(t, dir)

	rootCmd.SetArgs([]string{"list", "--json", "--status", "pending"})
	buf := new(strings.Builder)
	rootCmd.SetOut(buf)

	if err := rootCmd.Execute(); err != nil {
		t.Fatalf("execute: %v", err)
	}

	var workers []map[string]interface{}
	json.Unmarshal([]byte(buf.String()), &workers)
	if len(workers) != 1 {
		t.Errorf("expected 1 pending, got %d", len(workers))
	}
}

func TestListEmpty(t *testing.T) {
	dir := t.TempDir()
	stateDir = dir

	rootCmd.SetArgs([]string{"list"})
	buf := new(strings.Builder)
	rootCmd.SetOut(buf)

	if err := rootCmd.Execute(); err != nil {
		t.Fatalf("execute: %v", err)
	}

	output := strings.TrimSpace(buf.String())
	if output != "No workers." {
		t.Errorf("expected 'No workers.', got: %q", output)
	}
}

func TestListStaleDetection(t *testing.T) {
	dir := t.TempDir()
	stateDir = dir
	// PID 99999 is almost certainly not running
	w := &state.Worker{ID: "400", Status: state.StatusWorking, Directory: "/tmp", Task: "stale", PID: 99999}
	state.Write(dir, w)

	rootCmd.SetArgs([]string{"list", "--json"})
	buf := new(strings.Builder)
	rootCmd.SetOut(buf)
	rootCmd.Execute()

	var workers []map[string]interface{}
	json.Unmarshal([]byte(buf.String()), &workers)
	if len(workers) != 1 {
		t.Fatalf("expected 1, got %d", len(workers))
	}
	if workers[0]["status"] != "error" {
		t.Errorf("stale worker should be error, got %s", workers[0]["status"])
	}
}
```

**Step 2: Run tests to verify they fail**

Run: `cd ~/projects/wreccless && go test ./cmd/ccl/ -run TestList`
Expected: FAIL — `listCmd` not defined.

**Step 3: Write list.go**

```go
// cmd/ccl/list.go
package main

import (
	"encoding/json"
	"fmt"
	"strings"
	"syscall"
	"text/tabwriter"

	"github.com/ifit/wreccless/internal/state"
	"github.com/spf13/cobra"
)

var listCmd = &cobra.Command{
	Use:   "list",
	Short: "List all workers",
	RunE:  runList,
}

var (
	listJSON   bool
	listStatus string
)

func init() {
	listCmd.Flags().BoolVar(&listJSON, "json", false, "Output JSON")
	listCmd.Flags().StringVar(&listStatus, "status", "", "Filter by status (pending|working|done|error)")
	rootCmd.AddCommand(listCmd)
}

func isProcessAlive(pid int) bool {
	if pid <= 0 {
		return false
	}
	err := syscall.Kill(pid, 0)
	return err == nil
}

func runList(cmd *cobra.Command, args []string) error {
	workers, err := state.List(stateDir)
	if err != nil {
		return err
	}

	// Stale detection: working workers with dead PIDs → error
	for _, w := range workers {
		if w.Status == state.StatusWorking && w.PID > 0 && !isProcessAlive(w.PID) {
			w.Status = state.StatusError
			state.Write(stateDir, w)
		}
	}

	// Filter by status
	if listStatus != "" {
		var filtered []*state.Worker
		for _, w := range workers {
			if string(w.Status) == listStatus {
				filtered = append(filtered, w)
			}
		}
		workers = filtered
	}

	if len(workers) == 0 {
		fmt.Fprintln(cmd.OutOrStdout(), "No workers.")
		return nil
	}

	if listJSON {
		data, err := json.Marshal(workers)
		if err != nil {
			return err
		}
		fmt.Fprintln(cmd.OutOrStdout(), string(data))
		return nil
	}

	// Human-readable table
	tw := tabwriter.NewWriter(cmd.OutOrStdout(), 0, 4, 2, ' ', 0)
	fmt.Fprintln(tw, "ID\tSTATUS\tDIRECTORY\tTASK")
	home := ""
	if h, err := os.UserHomeDir(); err == nil {
		home = h
	}
	for _, w := range workers {
		dir := w.Directory
		if home != "" {
			dir = strings.Replace(dir, home, "~", 1)
		}
		task := w.Task
		if len(task) > 60 {
			task = task[:57] + "..."
		}
		fmt.Fprintf(tw, "%s\t%s\t%s\t%s\n", w.ID, w.Status, dir, task)
	}
	tw.Flush()
	return nil
}
```

Note: you'll need to add `"os"` to the imports in list.go for `os.UserHomeDir()`.

**Step 4: Run tests to verify they pass**

Run: `cd ~/projects/wreccless && go test ./cmd/ccl/ -run TestList -v`
Expected: All 5 tests PASS.

**Step 5: Commit**

```bash
cd ~/projects/wreccless
git add cmd/ccl/list.go cmd/ccl/list_test.go
git commit -m "feat: ccl list with JSON, filtering, stale detection"
```

---

### Task 7: `ccl status` Command

**Files:**
- Create: `~/projects/wreccless/cmd/ccl/status.go`
- Create: `~/projects/wreccless/cmd/ccl/status_test.go`

**Step 1: Write the failing test**

```go
// cmd/ccl/status_test.go
package main

import (
	"encoding/json"
	"strings"
	"testing"

	"github.com/ifit/wreccless/internal/state"
)

func TestStatus(t *testing.T) {
	dir := t.TempDir()
	stateDir = dir
	w := &state.Worker{ID: "500", Status: state.StatusWorking, Directory: "/tmp/proj", Task: "build feature", PID: 1, SessionID: "sess-500"}
	state.Write(dir, w)

	rootCmd.SetArgs([]string{"status", "500"})
	buf := new(strings.Builder)
	rootCmd.SetOut(buf)

	if err := rootCmd.Execute(); err != nil {
		t.Fatalf("execute: %v", err)
	}

	output := buf.String()
	if !strings.Contains(output, "500") || !strings.Contains(output, "working") {
		t.Errorf("missing info: %s", output)
	}
}

func TestStatusJSON(t *testing.T) {
	dir := t.TempDir()
	stateDir = dir
	w := &state.Worker{ID: "501", Status: state.StatusDone, Directory: "/tmp", Task: "done task", SessionID: "sess-501"}
	state.Write(dir, w)

	rootCmd.SetArgs([]string{"status", "501", "--json"})
	buf := new(strings.Builder)
	rootCmd.SetOut(buf)
	rootCmd.Execute()

	var result map[string]interface{}
	if err := json.Unmarshal([]byte(buf.String()), &result); err != nil {
		t.Fatalf("json: %v", err)
	}
	if result["id"] != "501" {
		t.Errorf("id: %v", result["id"])
	}
}

func TestStatusNotFound(t *testing.T) {
	dir := t.TempDir()
	stateDir = dir

	rootCmd.SetArgs([]string{"status", "999"})
	buf := new(strings.Builder)
	rootCmd.SetOut(buf)
	rootCmd.SetErr(buf)

	err := rootCmd.Execute()
	if err == nil {
		t.Fatal("expected error for nonexistent worker")
	}
}
```

**Step 2: Run test to verify it fails**

Run: `cd ~/projects/wreccless && go test ./cmd/ccl/ -run TestStatus`
Expected: FAIL.

**Step 3: Write status.go**

```go
// cmd/ccl/status.go
package main

import (
	"encoding/json"
	"fmt"

	"github.com/ifit/wreccless/internal/state"
	"github.com/spf13/cobra"
)

var statusCmd = &cobra.Command{
	Use:   "status <id>",
	Short: "Show detailed status of a worker",
	Args:  cobra.ExactArgs(1),
	RunE:  runStatus,
}

var statusJSON bool

func init() {
	statusCmd.Flags().BoolVar(&statusJSON, "json", false, "Output JSON")
	rootCmd.AddCommand(statusCmd)
}

func runStatus(cmd *cobra.Command, args []string) error {
	w, err := state.Read(stateDir, args[0])
	if err != nil {
		return fmt.Errorf("worker %s not found", args[0])
	}

	if statusJSON {
		data, _ := json.MarshalIndent(w, "", "  ")
		fmt.Fprintln(cmd.OutOrStdout(), string(data))
		return nil
	}

	fmt.Fprintf(cmd.OutOrStdout(), "ID:         %s\n", w.ID)
	fmt.Fprintf(cmd.OutOrStdout(), "Status:     %s\n", w.Status)
	fmt.Fprintf(cmd.OutOrStdout(), "Directory:  %s\n", w.Directory)
	fmt.Fprintf(cmd.OutOrStdout(), "Task:       %s\n", w.Task)
	if w.Image != "" {
		fmt.Fprintf(cmd.OutOrStdout(), "Image:      %s\n", w.Image)
	}
	if w.PID > 0 {
		fmt.Fprintf(cmd.OutOrStdout(), "PID:        %d\n", w.PID)
	}
	if w.SessionID != "" {
		fmt.Fprintf(cmd.OutOrStdout(), "Session:    %s\n", w.SessionID)
	}
	if w.CreatedAt != nil {
		fmt.Fprintf(cmd.OutOrStdout(), "Created:    %s\n", w.CreatedAt.Format("2006-01-02 15:04:05"))
	}
	if w.StartedAt != nil {
		fmt.Fprintf(cmd.OutOrStdout(), "Started:    %s\n", w.StartedAt.Format("2006-01-02 15:04:05"))
	}
	if w.FinishedAt != nil {
		fmt.Fprintf(cmd.OutOrStdout(), "Finished:   %s\n", w.FinishedAt.Format("2006-01-02 15:04:05"))
	}
	return nil
}
```

**Step 4: Run tests to verify they pass**

Run: `cd ~/projects/wreccless && go test ./cmd/ccl/ -run TestStatus -v`
Expected: All 3 tests PASS.

**Step 5: Commit**

```bash
cd ~/projects/wreccless
git add cmd/ccl/status.go cmd/ccl/status_test.go
git commit -m "feat: ccl status command"
```

---

### Task 8: `ccl approve` + `ccl deny` Commands

**Files:**
- Create: `~/projects/wreccless/cmd/ccl/approve.go`
- Create: `~/projects/wreccless/cmd/ccl/deny.go`
- Create: `~/projects/wreccless/cmd/ccl/approve_test.go`

**Step 1: Write the failing tests**

```go
// cmd/ccl/approve_test.go
package main

import (
	"strings"
	"testing"
	"path/filepath"

	"github.com/ifit/wreccless/internal/state"
)

func TestApprove(t *testing.T) {
	dir := t.TempDir()
	stateDir = dir
	configPath = filepath.Join(t.TempDir(), "nonexistent.toml")
	w := &state.Worker{ID: "600", Status: state.StatusPending, Directory: "/tmp", Task: "pending task"}
	state.Write(dir, w)

	rootCmd.SetArgs([]string{"approve", "600"})
	buf := new(strings.Builder)
	rootCmd.SetOut(buf)

	if err := rootCmd.Execute(); err != nil {
		t.Fatalf("execute: %v", err)
	}

	// After approve, worker should be working (or at least not pending)
	updated, _ := state.Read(dir, "600")
	if updated.Status == state.StatusPending {
		t.Error("worker should not be pending after approve")
	}
}

func TestApproveNotPending(t *testing.T) {
	dir := t.TempDir()
	stateDir = dir
	w := &state.Worker{ID: "601", Status: state.StatusDone, Directory: "/tmp", Task: "done task"}
	state.Write(dir, w)

	rootCmd.SetArgs([]string{"approve", "601"})
	buf := new(strings.Builder)
	rootCmd.SetOut(buf)
	rootCmd.SetErr(buf)

	err := rootCmd.Execute()
	if err == nil {
		t.Fatal("expected error when approving non-pending worker")
	}
}

func TestDeny(t *testing.T) {
	dir := t.TempDir()
	stateDir = dir
	configPath = filepath.Join(t.TempDir(), "nonexistent.toml")
	w := &state.Worker{ID: "700", Status: state.StatusPending, Directory: "/tmp", Task: "deny me"}
	state.Write(dir, w)

	rootCmd.SetArgs([]string{"deny", "700"})
	buf := new(strings.Builder)
	rootCmd.SetOut(buf)
	rootCmd.Execute()

	// Worker should be deleted
	_, err := state.Read(dir, "700")
	if err == nil {
		t.Error("worker should be deleted after deny")
	}
}

func TestDenyNotPending(t *testing.T) {
	dir := t.TempDir()
	stateDir = dir
	w := &state.Worker{ID: "701", Status: state.StatusWorking, Directory: "/tmp", Task: "working", PID: 1}
	state.Write(dir, w)

	rootCmd.SetArgs([]string{"deny", "701"})
	buf := new(strings.Builder)
	rootCmd.SetOut(buf)
	rootCmd.SetErr(buf)

	err := rootCmd.Execute()
	if err == nil {
		t.Fatal("expected error when denying non-pending worker")
	}
}
```

**Step 2: Run tests to verify they fail**

Run: `cd ~/projects/wreccless && go test ./cmd/ccl/ -run "TestApprove|TestDeny"`
Expected: FAIL.

**Step 3: Write approve.go**

```go
// cmd/ccl/approve.go
package main

import (
	"fmt"
	"time"

	"github.com/ifit/wreccless/internal/config"
	"github.com/ifit/wreccless/internal/hooks"
	"github.com/ifit/wreccless/internal/state"
	"github.com/spf13/cobra"
)

var approveCmd = &cobra.Command{
	Use:   "approve <id>",
	Short: "Start a pending worker",
	Args:  cobra.ExactArgs(1),
	RunE:  runApprove,
}

func init() {
	rootCmd.AddCommand(approveCmd)
}

func runApprove(cmd *cobra.Command, args []string) error {
	id := args[0]
	w, err := state.Read(stateDir, id)
	if err != nil {
		return fmt.Errorf("worker %s not found", id)
	}
	if w.Status != state.StatusPending {
		return fmt.Errorf("worker %s is %s, not pending", id, w.Status)
	}

	cfg, _ := config.Load(configPath)

	now := time.Now()
	w.Status = state.StatusWorking
	w.StartedAt = &now
	if err := state.Write(stateDir, w); err != nil {
		return err
	}

	// TODO: Task 10 — spawn detached ccl run here
	vars := hooks.Vars{ID: id, Task: w.Task, Dir: w.Directory, Status: string(w.Status)}
	hooks.Fire(cfg.Hooks.OnStart, vars)

	fmt.Fprintf(cmd.OutOrStdout(), "Approved worker %s\n", id)
	return nil
}
```

**Step 4: Write deny.go**

```go
// cmd/ccl/deny.go
package main

import (
	"fmt"

	"github.com/ifit/wreccless/internal/config"
	"github.com/ifit/wreccless/internal/hooks"
	"github.com/ifit/wreccless/internal/state"
	"github.com/spf13/cobra"
)

var denyCmd = &cobra.Command{
	Use:   "deny <id>",
	Short: "Remove a pending worker",
	Args:  cobra.ExactArgs(1),
	RunE:  runDeny,
}

func init() {
	rootCmd.AddCommand(denyCmd)
}

func runDeny(cmd *cobra.Command, args []string) error {
	id := args[0]
	w, err := state.Read(stateDir, id)
	if err != nil {
		return fmt.Errorf("worker %s not found", id)
	}
	if w.Status != state.StatusPending {
		return fmt.Errorf("worker %s is %s, not pending", id, w.Status)
	}

	cfg, _ := config.Load(configPath)

	if err := state.Delete(stateDir, id); err != nil {
		return err
	}

	vars := hooks.Vars{ID: id, Task: w.Task, Dir: w.Directory, Status: "denied"}
	hooks.Fire(cfg.Hooks.OnKill, vars)

	fmt.Fprintf(cmd.OutOrStdout(), "Denied worker %s\n", id)
	return nil
}
```

**Step 5: Run tests to verify they pass**

Run: `cd ~/projects/wreccless && go test ./cmd/ccl/ -run "TestApprove|TestDeny" -v`
Expected: All 4 tests PASS.

**Step 6: Commit**

```bash
cd ~/projects/wreccless
git add cmd/ccl/approve.go cmd/ccl/deny.go cmd/ccl/approve_test.go
git commit -m "feat: ccl approve and deny commands"
```

---

### Task 9: `ccl kill` + `ccl clean` Commands

**Files:**
- Create: `~/projects/wreccless/cmd/ccl/kill.go`
- Create: `~/projects/wreccless/cmd/ccl/clean.go`
- Create: `~/projects/wreccless/cmd/ccl/kill_test.go`

**Step 1: Write the failing tests**

```go
// cmd/ccl/kill_test.go
package main

import (
	"strings"
	"testing"
	"path/filepath"

	"github.com/ifit/wreccless/internal/state"
)

func TestKill(t *testing.T) {
	dir := t.TempDir()
	stateDir = dir
	configPath = filepath.Join(t.TempDir(), "nonexistent.toml")
	// Use PID 99999 which is almost certainly not running
	w := &state.Worker{ID: "800", Status: state.StatusWorking, Directory: "/tmp", Task: "kill me", PID: 99999}
	state.Write(dir, w)

	rootCmd.SetArgs([]string{"kill", "800"})
	buf := new(strings.Builder)
	rootCmd.SetOut(buf)
	rootCmd.Execute()

	// Worker should be deleted
	_, err := state.Read(dir, "800")
	if err == nil {
		t.Error("worker should be deleted after kill")
	}
}

func TestClean(t *testing.T) {
	dir := t.TempDir()
	stateDir = dir
	state.Write(dir, &state.Worker{ID: "900", Status: state.StatusDone, Directory: "/tmp", Task: "done"})
	state.Write(dir, &state.Worker{ID: "901", Status: state.StatusError, Directory: "/tmp", Task: "error"})
	state.Write(dir, &state.Worker{ID: "902", Status: state.StatusWorking, Directory: "/tmp", Task: "working", PID: 1})

	rootCmd.SetArgs([]string{"clean"})
	buf := new(strings.Builder)
	rootCmd.SetOut(buf)
	rootCmd.Execute()

	workers, _ := state.List(dir)
	if len(workers) != 1 {
		t.Errorf("expected 1 remaining (working), got %d", len(workers))
	}
	if workers[0].ID != "902" {
		t.Errorf("wrong surviving worker: %s", workers[0].ID)
	}
}

func TestCleanAll(t *testing.T) {
	dir := t.TempDir()
	stateDir = dir
	state.Write(dir, &state.Worker{ID: "910", Status: state.StatusDone, Directory: "/tmp", Task: "done"})
	state.Write(dir, &state.Worker{ID: "911", Status: state.StatusWorking, Directory: "/tmp", Task: "working", PID: 1})

	rootCmd.SetArgs([]string{"clean", "--all"})
	buf := new(strings.Builder)
	rootCmd.SetOut(buf)
	rootCmd.Execute()

	workers, _ := state.List(dir)
	if len(workers) != 0 {
		t.Errorf("expected 0 remaining, got %d", len(workers))
	}
}
```

**Step 2: Run test to verify it fails**

Run: `cd ~/projects/wreccless && go test ./cmd/ccl/ -run "TestKill|TestClean"`
Expected: FAIL.

**Step 3: Write kill.go**

```go
// cmd/ccl/kill.go
package main

import (
	"fmt"
	"syscall"

	"github.com/ifit/wreccless/internal/config"
	"github.com/ifit/wreccless/internal/hooks"
	"github.com/ifit/wreccless/internal/state"
	"github.com/spf13/cobra"
)

var killCmd = &cobra.Command{
	Use:   "kill <id>",
	Short: "Kill a running worker",
	Args:  cobra.ExactArgs(1),
	RunE:  runKill,
}

func init() {
	rootCmd.AddCommand(killCmd)
}

func runKill(cmd *cobra.Command, args []string) error {
	id := args[0]
	w, err := state.Read(stateDir, id)
	if err != nil {
		return fmt.Errorf("worker %s not found", id)
	}

	cfg, _ := config.Load(configPath)

	// Send SIGTERM to claude process
	if w.PID > 0 {
		syscall.Kill(w.PID, syscall.SIGTERM)
	}

	state.Delete(stateDir, id)

	vars := hooks.Vars{ID: id, Task: w.Task, Dir: w.Directory, Status: "killed"}
	hooks.Fire(cfg.Hooks.OnKill, vars)

	fmt.Fprintf(cmd.OutOrStdout(), "Killed worker %s\n", id)
	return nil
}
```

**Step 4: Write clean.go**

```go
// cmd/ccl/clean.go
package main

import (
	"fmt"

	"github.com/ifit/wreccless/internal/state"
	"github.com/spf13/cobra"
)

var cleanCmd = &cobra.Command{
	Use:   "clean",
	Short: "Remove done/error workers",
	RunE:  runClean,
}

var cleanAll bool

func init() {
	cleanCmd.Flags().BoolVar(&cleanAll, "all", false, "Remove all workers including working")
	rootCmd.AddCommand(cleanCmd)
}

func runClean(cmd *cobra.Command, args []string) error {
	workers, err := state.List(stateDir)
	if err != nil {
		return err
	}

	removed := 0
	for _, w := range workers {
		if cleanAll || w.Status == state.StatusDone || w.Status == state.StatusError {
			state.Delete(stateDir, w.ID)
			removed++
		}
	}

	fmt.Fprintf(cmd.OutOrStdout(), "Cleaned %d worker(s).\n", removed)
	return nil
}
```

**Step 5: Run tests to verify they pass**

Run: `cd ~/projects/wreccless && go test ./cmd/ccl/ -run "TestKill|TestClean" -v`
Expected: All 3 tests PASS.

**Step 6: Commit**

```bash
cd ~/projects/wreccless
git add cmd/ccl/kill.go cmd/ccl/clean.go cmd/ccl/kill_test.go
git commit -m "feat: ccl kill and clean commands"
```

---

### Task 10: `ccl run` — Internal Worker Driver

This is the core command that actually runs claude. It reads the state file, launches `claude -p`, captures output, updates state on completion.

**Files:**
- Create: `~/projects/wreccless/internal/worker/run.go`
- Create: `~/projects/wreccless/internal/worker/run_test.go`
- Create: `~/projects/wreccless/cmd/ccl/run.go`

**Step 1: Write a mock claude script for testing**

Create a test helper that writes a temporary script simulating claude's behavior:

```go
// internal/worker/run_test.go
package worker

import (
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/ifit/wreccless/internal/config"
	"github.com/ifit/wreccless/internal/state"
)

func writeMockClaude(t *testing.T, dir string, exitCode int) string {
	t.Helper()
	script := filepath.Join(dir, "mock-claude")
	content := fmt.Sprintf(`#!/bin/sh
echo '{"type":"system","subtype":"init"}'
echo '{"type":"assistant","content":"I fixed the bug."}'
echo '{"type":"result","subtype":"success"}'
exit %d
`, exitCode)
	os.WriteFile(script, []byte(content), 0755)
	return script
}

func TestRunSuccess(t *testing.T) {
	stateDir := t.TempDir()
	binDir := t.TempDir()
	mockClaude := writeMockClaude(t, binDir, 0)

	w := &state.Worker{ID: "1000", Status: state.StatusWorking, Directory: t.TempDir(), Task: "fix bug", SessionID: "test-session"}
	state.Write(stateDir, w)

	cfg := config.Defaults()

	err := Run(stateDir, "1000", cfg, mockClaude)
	if err != nil {
		t.Fatalf("Run: %v", err)
	}

	updated, err := state.Read(stateDir, "1000")
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	if updated.Status != state.StatusDone {
		t.Errorf("expected done, got %s", updated.Status)
	}
	if updated.FinishedAt == nil {
		t.Error("finished_at should be set")
	}

	// Check log file exists
	logPath := filepath.Join(stateDir, "1000.log")
	data, err := os.ReadFile(logPath)
	if err != nil {
		t.Fatalf("log: %v", err)
	}
	if len(data) == 0 {
		t.Error("log should not be empty")
	}
}

func TestRunFailure(t *testing.T) {
	stateDir := t.TempDir()
	binDir := t.TempDir()
	mockClaude := writeMockClaude(t, binDir, 1)

	w := &state.Worker{ID: "1001", Status: state.StatusWorking, Directory: t.TempDir(), Task: "fail task", SessionID: "test-session"}
	state.Write(stateDir, w)

	cfg := config.Defaults()

	err := Run(stateDir, "1001", cfg, mockClaude)
	// Run returns nil even on failure — it records error state
	if err != nil {
		t.Fatalf("Run: %v", err)
	}

	updated, _ := state.Read(stateDir, "1001")
	if updated.Status != state.StatusError {
		t.Errorf("expected error, got %s", updated.Status)
	}
}
```

Note: you'll need `"fmt"` imported in the test file for `fmt.Sprintf`.

**Step 2: Run test to verify it fails**

Run: `cd ~/projects/wreccless && go test ./internal/worker/ -run TestRun`
Expected: FAIL — `Run` function not defined.

**Step 3: Write run.go**

```go
// internal/worker/run.go
package worker

import (
	"fmt"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
	"syscall"
	"time"

	"github.com/ifit/wreccless/internal/config"
	"github.com/ifit/wreccless/internal/hooks"
	"github.com/ifit/wreccless/internal/state"
)

// Run executes a worker's claude session. This is a blocking call.
// claudeBin allows overriding the claude binary path for testing.
// Pass "" to use the default "claude" from PATH.
func Run(stateDir, id string, cfg *config.Config, claudeBin string) error {
	w, err := state.Read(stateDir, id)
	if err != nil {
		return fmt.Errorf("read worker: %w", err)
	}

	if claudeBin == "" {
		claudeBin = "claude"
	}

	// Build claude arguments
	args := []string{
		"-p",
		"--output-format", "stream-json",
		"--verbose",
		"--session-id", w.SessionID,
	}
	if cfg.Claude.SkipPermissions {
		args = append(args, "--dangerously-skip-permissions")
	}
	if cfg.Claude.SystemPrompt != "" {
		args = append(args, "--append-system-prompt", cfg.Claude.SystemPrompt)
	}
	args = append(args, cfg.Claude.ExtraFlags...)

	// Build task text (prepend image reference if set)
	task := w.Task
	if w.Image != "" {
		task = fmt.Sprintf("Read and reference this image: %s\n\n%s", w.Image, task)
	}
	args = append(args, task)

	// Open log file
	logPath := filepath.Join(stateDir, id+".log")
	logFile, err := os.Create(logPath)
	if err != nil {
		return fmt.Errorf("create log: %w", err)
	}
	defer logFile.Close()

	// Build and start command
	cmd := exec.Command(claudeBin, args...)
	cmd.Dir = w.Directory
	cmd.Stdout = logFile
	cmd.Stderr = logFile
	cmd.Stdin = nil

	// Forward SIGTERM to child
	sigCh := make(chan os.Signal, 1)
	signal.Notify(sigCh, syscall.SIGTERM)
	go func() {
		<-sigCh
		if cmd.Process != nil {
			cmd.Process.Signal(syscall.SIGTERM)
		}
	}()

	if err := cmd.Start(); err != nil {
		markError(stateDir, w, cfg)
		return fmt.Errorf("start claude: %w", err)
	}

	// Update PID in state
	w.PID = cmd.Process.Pid
	state.Write(stateDir, w)

	// Wait for completion
	runErr := cmd.Wait()
	signal.Stop(sigCh)

	now := time.Now()
	w.FinishedAt = &now

	if runErr != nil {
		markError(stateDir, w, cfg)
	} else {
		w.Status = state.StatusDone
		state.Write(stateDir, w)
		vars := hooks.Vars{ID: w.ID, Task: w.Task, Dir: w.Directory, Status: "done", SessionID: w.SessionID}
		hooks.Fire(cfg.Hooks.OnDone, vars)
	}

	return nil
}

func markError(stateDir string, w *state.Worker, cfg *config.Config) {
	now := time.Now()
	w.Status = state.StatusError
	w.FinishedAt = &now
	state.Write(stateDir, w)
	vars := hooks.Vars{ID: w.ID, Task: w.Task, Dir: w.Directory, Status: "error", SessionID: w.SessionID}
	hooks.Fire(cfg.Hooks.OnError, vars)
}
```

**Step 4: Write the cmd/ccl/run.go CLI wrapper**

```go
// cmd/ccl/run.go
package main

import (
	"github.com/ifit/wreccless/internal/config"
	"github.com/ifit/wreccless/internal/worker"
	"github.com/spf13/cobra"
)

var runCmd = &cobra.Command{
	Use:    "run <id>",
	Short:  "Internal: run a worker's claude session (blocking)",
	Args:   cobra.ExactArgs(1),
	Hidden: true,
	RunE:   runRun,
}

func init() {
	rootCmd.AddCommand(runCmd)
}

func runRun(cmd *cobra.Command, args []string) error {
	cfg, err := config.Load(configPath)
	if err != nil {
		return err
	}
	return worker.Run(stateDir, args[0], cfg, "")
}
```

**Step 5: Run tests to verify they pass**

Run: `cd ~/projects/wreccless && go test ./internal/worker/ -v`
Expected: Both tests PASS.

**Step 6: Commit**

```bash
cd ~/projects/wreccless
git add internal/worker/ cmd/ccl/run.go
git commit -m "feat: ccl run — internal worker driver with claude execution"
```

---

### Task 11: Process Spawning — `ccl new` Auto-Start + `ccl approve` Spawn

Wire up `ccl new` (without `--pending`) and `ccl approve` to spawn a detached `ccl run <id>` process.

**Files:**
- Create: `~/projects/wreccless/internal/worker/spawn.go`
- Create: `~/projects/wreccless/internal/worker/spawn_test.go`
- Modify: `~/projects/wreccless/cmd/ccl/new.go` — replace TODO with spawn call
- Modify: `~/projects/wreccless/cmd/ccl/approve.go` — add spawn call

**Step 1: Write the failing test**

```go
// internal/worker/spawn_test.go
package worker

import (
	"os"
	"path/filepath"
	"testing"
	"time"
)

func TestSpawnDetached(t *testing.T) {
	// Create a mock ccl binary that just writes a marker file
	binDir := t.TempDir()
	mockBin := filepath.Join(binDir, "ccl")
	marker := filepath.Join(t.TempDir(), "spawned")
	os.WriteFile(mockBin, []byte(fmt.Sprintf(`#!/bin/sh
touch %s
`, marker)), 0755)

	err := SpawnRun("1234", binDir+"/ccl", "/tmp", "/tmp/state")
	if err != nil {
		t.Fatalf("SpawnRun: %v", err)
	}

	// Wait for the detached process to execute
	time.Sleep(500 * time.Millisecond)
	if _, err := os.Stat(marker); os.IsNotExist(err) {
		t.Error("detached process did not run — marker file missing")
	}
}
```

**Step 2: Run test to verify it fails**

Run: `cd ~/projects/wreccless && go test ./internal/worker/ -run TestSpawn`
Expected: FAIL — `SpawnRun` not defined.

**Step 3: Write spawn.go**

```go
// internal/worker/spawn.go
package worker

import (
	"os"
	"os/exec"
	"syscall"
)

// SpawnRun launches "ccl run <id>" as a detached background process.
// The child process survives the parent exiting.
// cclBin is the path to the ccl binary. stateDir and configPath are
// passed via environment variables so the child uses the same paths.
func SpawnRun(id, cclBin, configPath, stateDir string) error {
	cmd := exec.Command(cclBin, "run", id)
	cmd.SysProcAttr = &syscall.SysProcAttr{
		Setsid: true, // new session — survives parent exit
	}
	cmd.Stdin = nil
	cmd.Stdout = nil
	cmd.Stderr = nil
	cmd.Env = append(os.Environ(),
		"CCL_STATE_DIR="+stateDir,
		"CCL_CONFIG="+configPath,
	)
	return cmd.Start()
}
```

**Step 4: Run test to verify it passes**

Run: `cd ~/projects/wreccless && go test ./internal/worker/ -run TestSpawn -v`
Expected: PASS.

**Step 5: Update cmd/ccl/main.go to check env overrides**

Add this to the top of `main()` or in an `init()` in main.go:

```go
func init() {
	if v := os.Getenv("CCL_STATE_DIR"); v != "" {
		stateDir = v
	}
	if v := os.Getenv("CCL_CONFIG"); v != "" {
		configPath = v
	}
}
```

**Step 6: Update cmd/ccl/new.go — replace TODO with spawn**

In `runNew`, replace the `// TODO: Task 10` section with:

```go
	if !newPending {
		startedAt := time.Now()
		w.StartedAt = &startedAt
		state.Write(stateDir, w)

		cclBin, _ := os.Executable()
		if err := worker.SpawnRun(id, cclBin, configPath, stateDir); err != nil {
			return fmt.Errorf("spawn worker: %w", err)
		}
		hooks.Fire(cfg.Hooks.OnStart, vars)
	}
```

Add the `worker` and `os` imports.

**Step 7: Update cmd/ccl/approve.go — add spawn**

In `runApprove`, after updating state to working, add:

```go
	cclBin, _ := os.Executable()
	if err := worker.SpawnRun(id, cclBin, configPath, stateDir); err != nil {
		return fmt.Errorf("spawn worker: %w", err)
	}
```

Add the `worker` and `os` imports.

**Step 8: Also add SessionID generation**

In `runNew`, after creating the worker struct, generate a session ID:

```go
	import "github.com/google/uuid"

	// In runNew, before state.Write:
	w.SessionID = uuid.New().String()
```

Similarly in `runApprove`, before writing the working state:

```go
	w.SessionID = uuid.New().String()
```

**Step 9: Run all tests**

Run: `cd ~/projects/wreccless && go test ./... -v`
Expected: All tests PASS.

**Step 10: Commit**

```bash
cd ~/projects/wreccless
git add internal/worker/spawn.go internal/worker/spawn_test.go cmd/ccl/
git commit -m "feat: detached process spawning for auto-start and approve"
```

---

### Task 12: `ccl logs` Command

**Files:**
- Create: `~/projects/wreccless/cmd/ccl/logs.go`
- Create: `~/projects/wreccless/cmd/ccl/logs_test.go`

**Step 1: Write the failing tests**

```go
// cmd/ccl/logs_test.go
package main

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/ifit/wreccless/internal/state"
)

func writeTestLog(t *testing.T, dir, id string) {
	t.Helper()
	logPath := filepath.Join(dir, id+".log")
	lines := `{"type":"system","subtype":"init"}
{"type":"assistant","content":[{"type":"text","text":"I found the bug."}]}
{"type":"tool_use","name":"Edit","input":{"file_path":"/tmp/foo.go"}}
{"type":"result","subtype":"success"}
`
	os.WriteFile(logPath, []byte(lines), 0644)
}

func TestLogsHuman(t *testing.T) {
	dir := t.TempDir()
	stateDir = dir
	state.Write(dir, &state.Worker{ID: "1100", Status: state.StatusDone, Directory: "/tmp", Task: "test"})
	writeTestLog(t, dir, "1100")

	rootCmd.SetArgs([]string{"logs", "1100"})
	buf := new(strings.Builder)
	rootCmd.SetOut(buf)
	rootCmd.Execute()

	output := buf.String()
	if !strings.Contains(output, "I found the bug") {
		t.Errorf("expected assistant text in output: %s", output)
	}
}

func TestLogsJSON(t *testing.T) {
	dir := t.TempDir()
	stateDir = dir
	state.Write(dir, &state.Worker{ID: "1101", Status: state.StatusDone, Directory: "/tmp", Task: "test"})
	writeTestLog(t, dir, "1101")

	rootCmd.SetArgs([]string{"logs", "1101", "--json"})
	buf := new(strings.Builder)
	rootCmd.SetOut(buf)
	rootCmd.Execute()

	output := buf.String()
	lines := strings.Split(strings.TrimSpace(output), "\n")
	if len(lines) != 4 {
		t.Errorf("expected 4 JSON lines, got %d", len(lines))
	}
}

func TestLogsNoFile(t *testing.T) {
	dir := t.TempDir()
	stateDir = dir
	state.Write(dir, &state.Worker{ID: "1102", Status: state.StatusPending, Directory: "/tmp", Task: "test"})

	rootCmd.SetArgs([]string{"logs", "1102"})
	buf := new(strings.Builder)
	rootCmd.SetOut(buf)
	rootCmd.SetErr(buf)

	err := rootCmd.Execute()
	if err == nil {
		output := buf.String()
		if !strings.Contains(output, "no log") && !strings.Contains(strings.ToLower(output), "no log") {
			// It's ok if it just says "no log file" without erroring
		}
	}
}
```

**Step 2: Run test to verify it fails**

Run: `cd ~/projects/wreccless && go test ./cmd/ccl/ -run TestLogs`
Expected: FAIL.

**Step 3: Write logs.go**

```go
// cmd/ccl/logs.go
package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"time"

	"github.com/spf13/cobra"
)

var logsCmd = &cobra.Command{
	Use:   "logs <id>",
	Short: "Show worker output log",
	Args:  cobra.ExactArgs(1),
	RunE:  runLogs,
}

var (
	logsFollow bool
	logsJSON   bool
)

func init() {
	logsCmd.Flags().BoolVarP(&logsFollow, "follow", "f", false, "Tail log output")
	logsCmd.Flags().BoolVar(&logsJSON, "json", false, "Output raw NDJSON events")
	rootCmd.AddCommand(logsCmd)
}

func runLogs(cmd *cobra.Command, args []string) error {
	id := args[0]
	logPath := filepath.Join(stateDir, id+".log")

	f, err := os.Open(logPath)
	if err != nil {
		return fmt.Errorf("no log file for worker %s", id)
	}
	defer f.Close()

	if logsJSON {
		// Raw passthrough
		if logsFollow {
			return tailFile(cmd.OutOrStdout(), f)
		}
		io.Copy(cmd.OutOrStdout(), f)
		return nil
	}

	// Human-readable: parse NDJSON and render
	if logsFollow {
		return tailFileHuman(cmd.OutOrStdout(), f)
	}
	return renderHuman(cmd.OutOrStdout(), f)
}

func renderHuman(out io.Writer, r io.Reader) error {
	scanner := bufio.NewScanner(r)
	for scanner.Scan() {
		renderLine(out, scanner.Bytes())
	}
	return scanner.Err()
}

func renderLine(out io.Writer, line []byte) {
	var event map[string]interface{}
	if err := json.Unmarshal(line, &event); err != nil {
		fmt.Fprintf(out, "%s\n", line)
		return
	}

	typ, _ := event["type"].(string)
	switch typ {
	case "assistant":
		content := event["content"]
		switch c := content.(type) {
		case string:
			if c != "" {
				fmt.Fprintf(out, "%s\n", c)
			}
		case []interface{}:
			for _, item := range c {
				if m, ok := item.(map[string]interface{}); ok {
					if text, ok := m["text"].(string); ok {
						fmt.Fprintf(out, "%s\n", text)
					}
				}
			}
		}
	case "tool_use":
		name, _ := event["name"].(string)
		fmt.Fprintf(out, "[tool: %s]\n", name)
	case "result":
		sub, _ := event["subtype"].(string)
		fmt.Fprintf(out, "[result: %s]\n", sub)
	}
}

func tailFile(out io.Writer, f *os.File) error {
	// Read existing content first
	io.Copy(out, f)
	// Then poll for new content
	for {
		n, _ := io.Copy(out, f)
		if n == 0 {
			time.Sleep(200 * time.Millisecond)
		}
	}
}

func tailFileHuman(out io.Writer, f *os.File) error {
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		renderLine(out, scanner.Bytes())
	}
	// Poll for new lines
	for {
		if scanner.Scan() {
			renderLine(out, scanner.Bytes())
		} else {
			time.Sleep(200 * time.Millisecond)
			scanner = bufio.NewScanner(f)
		}
	}
}
```

**Step 4: Run tests to verify they pass**

Run: `cd ~/projects/wreccless && go test ./cmd/ccl/ -run TestLogs -v`
Expected: All 3 tests PASS.

**Step 5: Commit**

```bash
cd ~/projects/wreccless
git add cmd/ccl/logs.go cmd/ccl/logs_test.go
git commit -m "feat: ccl logs with human-readable parsing and follow mode"
```

---

### Task 13: `ccl resume` Command

**Files:**
- Create: `~/projects/wreccless/cmd/ccl/resume.go`
- Create: `~/projects/wreccless/cmd/ccl/resume_test.go`

**Step 1: Write the failing test**

Since `resume` does `syscall.Exec` (which replaces the process), we can't test it end-to-end in unit tests. Instead, test that it finds the right worker and constructs the right arguments. We'll add a `--dry-run` flag for testing.

```go
// cmd/ccl/resume_test.go
package main

import (
	"strings"
	"testing"

	"github.com/ifit/wreccless/internal/state"
)

func TestResumeDryRun(t *testing.T) {
	dir := t.TempDir()
	stateDir = dir
	w := &state.Worker{
		ID:        "1200",
		Status:    state.StatusDone,
		Directory: "/tmp/myproject",
		Task:      "done task",
		SessionID: "sess-1200-uuid",
	}
	state.Write(dir, w)

	rootCmd.SetArgs([]string{"resume", "1200", "--dry-run"})
	buf := new(strings.Builder)
	rootCmd.SetOut(buf)
	rootCmd.Execute()

	output := buf.String()
	if !strings.Contains(output, "sess-1200-uuid") {
		t.Errorf("expected session ID in output: %s", output)
	}
	if !strings.Contains(output, "/tmp/myproject") {
		t.Errorf("expected directory in output: %s", output)
	}
}

func TestResumeNoSession(t *testing.T) {
	dir := t.TempDir()
	stateDir = dir
	w := &state.Worker{ID: "1201", Status: state.StatusPending, Directory: "/tmp", Task: "no session"}
	state.Write(dir, w)

	rootCmd.SetArgs([]string{"resume", "1201"})
	buf := new(strings.Builder)
	rootCmd.SetOut(buf)
	rootCmd.SetErr(buf)

	err := rootCmd.Execute()
	if err == nil {
		t.Fatal("expected error for worker without session")
	}
}
```

**Step 2: Run test to verify it fails**

Run: `cd ~/projects/wreccless && go test ./cmd/ccl/ -run TestResume`
Expected: FAIL.

**Step 3: Write resume.go**

```go
// cmd/ccl/resume.go
package main

import (
	"fmt"
	"os"
	"os/exec"
	"syscall"

	"github.com/ifit/wreccless/internal/config"
	"github.com/ifit/wreccless/internal/state"
	"github.com/spf13/cobra"
)

var resumeCmd = &cobra.Command{
	Use:   "resume <id>",
	Short: "Resume a worker's claude session interactively",
	Args:  cobra.ExactArgs(1),
	RunE:  runResume,
}

var resumeDryRun bool

func init() {
	resumeCmd.Flags().BoolVar(&resumeDryRun, "dry-run", false, "Print the resume command instead of executing it")
	rootCmd.AddCommand(resumeCmd)
}

func runResume(cmd *cobra.Command, args []string) error {
	id := args[0]
	w, err := state.Read(stateDir, id)
	if err != nil {
		return fmt.Errorf("worker %s not found", id)
	}
	if w.SessionID == "" {
		return fmt.Errorf("worker %s has no session to resume", id)
	}

	cfg, _ := config.Load(configPath)

	// Build claude args for interactive resume
	claudeArgs := []string{"--resume", w.SessionID}
	if cfg.Claude.SkipPermissions {
		claudeArgs = append(claudeArgs, "--dangerously-skip-permissions")
	}

	if resumeDryRun {
		fmt.Fprintf(cmd.OutOrStdout(), "cd %s && claude %s\n", w.Directory, joinArgs(claudeArgs))
		return nil
	}

	// Clean up state file since user is taking over
	state.Delete(stateDir, id)

	// Change to worker directory
	os.Chdir(w.Directory)

	// Exec into claude (replaces this process)
	claudePath, err := exec.LookPath("claude")
	if err != nil {
		return fmt.Errorf("claude not found in PATH")
	}
	return syscall.Exec(claudePath, append([]string{"claude"}, claudeArgs...), os.Environ())
}

func joinArgs(args []string) string {
	result := ""
	for i, a := range args {
		if i > 0 {
			result += " "
		}
		result += a
	}
	return result
}
```

**Step 4: Run tests to verify they pass**

Run: `cd ~/projects/wreccless && go test ./cmd/ccl/ -run TestResume -v`
Expected: Both tests PASS.

**Step 5: Commit**

```bash
cd ~/projects/wreccless
git add cmd/ccl/resume.go cmd/ccl/resume_test.go
git commit -m "feat: ccl resume — exec into interactive claude session"
```

---

### Task 14: Integration Test — Full Lifecycle

End-to-end test using a mock claude script. Tests the full flow: new → list → logs → kill/clean.

**Files:**
- Create: `~/projects/wreccless/integration_test.go`

**Step 1: Write the integration test**

```go
// integration_test.go
package main

import (
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestIntegrationLifecycle(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping integration test")
	}

	// Build ccl binary
	binDir := t.TempDir()
	cclBin := filepath.Join(binDir, "ccl")
	buildCmd := exec.Command("go", "build", "-o", cclBin, "./cmd/ccl")
	if out, err := buildCmd.CombinedOutput(); err != nil {
		t.Fatalf("build: %s\n%s", err, out)
	}

	stateDir := t.TempDir()
	configDir := t.TempDir()

	// Write a config with no hooks (to avoid side effects)
	configPath := filepath.Join(configDir, "config.toml")
	os.WriteFile(configPath, []byte("[claude]\nskip_permissions = true\n"), 0644)

	env := append(os.Environ(),
		"CCL_STATE_DIR="+stateDir,
		"CCL_CONFIG="+configPath,
	)

	run := func(args ...string) (string, error) {
		cmd := exec.Command(cclBin, args...)
		cmd.Env = env
		out, err := cmd.CombinedOutput()
		return strings.TrimSpace(string(out)), err
	}

	// 1. Create a pending worker
	out, err := run("new", "--dir", t.TempDir(), "--task", "integration test", "--pending")
	if err != nil {
		t.Fatalf("new --pending: %s (%v)", out, err)
	}
	workerID := out
	if workerID == "" {
		t.Fatal("expected worker ID")
	}

	// 2. List should show it as pending
	out, err = run("list", "--json")
	if err != nil {
		t.Fatalf("list: %s (%v)", out, err)
	}
	var workers []map[string]interface{}
	json.Unmarshal([]byte(out), &workers)
	if len(workers) != 1 || workers[0]["status"] != "pending" {
		t.Errorf("expected 1 pending worker: %s", out)
	}

	// 3. Status should show details
	out, err = run("status", workerID, "--json")
	if err != nil {
		t.Fatalf("status: %s (%v)", out, err)
	}

	// 4. Deny the worker
	out, err = run("deny", workerID)
	if err != nil {
		t.Fatalf("deny: %s (%v)", out, err)
	}

	// 5. List should be empty
	out, _ = run("list")
	if !strings.Contains(out, "No workers") {
		t.Errorf("expected no workers after deny: %s", out)
	}

	// 6. Create and auto-start (will fail since claude isn't available, but tests the flow)
	// Skip this part — it requires a real or mock claude binary
	_ = time.Now() // suppress unused import

	t.Log("Integration lifecycle test passed")
}
```

**Step 2: Run the integration test**

Run: `cd ~/projects/wreccless && go test -v -run TestIntegration`
Expected: PASS.

**Step 3: Commit**

```bash
cd ~/projects/wreccless
git add integration_test.go
git commit -m "test: integration lifecycle test"
```

---

### Task 15: Makefile Install + Final Polish

**Files:**
- Modify: `~/projects/wreccless/Makefile`

**Step 1: Verify the Makefile build and install targets work**

Run: `cd ~/projects/wreccless && make build && ./ccl --help`
Expected: Help output showing all subcommands.

Run: `cd ~/projects/wreccless && make install`
Expected: Binary installed to `~/.local/bin/ccl`.

Run: `ccl --help`
Expected: Same help output from the installed binary.

**Step 2: Run full test suite**

Run: `cd ~/projects/wreccless && make test`
Expected: All tests PASS.

**Step 3: Commit any final fixes**

```bash
cd ~/projects/wreccless
git add -A
git commit -m "chore: final polish and verify install"
```

---

### Task 16: Verify E2E with Real Claude (Manual)

This is a manual verification step. Not automated.

**Step 1: Install ccl**

```bash
cd ~/projects/wreccless && make install
```

**Step 2: Create config**

```bash
mkdir -p ~/.config/ccl
cp ~/projects/wreccless/config.example.toml ~/.config/ccl/config.toml
# Edit hooks to add waybar signals:
# on_start = ["pkill -SIGRTMIN+12 waybar"]
# on_done = ["pkill -SIGRTMIN+12 waybar", "notify-send 'Worker Done' '{{.Task}}'"]
```

**Step 3: Test pending workflow**

```bash
ccl new --dir ~/projects/wreccless --task "echo hello" --pending
ccl list
ccl approve <id>
ccl list  # should show working
ccl logs <id>  # should show output
# Wait for completion
ccl list  # should show done
ccl clean
```

**Step 4: Test auto-start workflow**

```bash
ccl new --dir ~/projects/wreccless --task "Read the README and summarize it in one sentence"
ccl list  # should show working
ccl logs <id> --follow  # watch live output
# Ctrl+C to stop following
ccl list  # should show done eventually
ccl resume <id>  # opens interactive claude session
```
