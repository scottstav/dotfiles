# System-Wide Color Theming Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build a template-based theme engine that reads TOML theme files and generates configs for all desktop apps, with a CLI and fzf picker to switch themes on the fly.

**Architecture:** Python script reads `~/.config/themes/<name>.toml`, resolves roles/derivations, renders `string.Template` files for each app, writes output to real config paths, and reloads services. Theme TOML defines ~25 color values. Emacs handled by specifying an existing theme name + optional face overrides, written to `~/.emacs.d/theme.el`.

**Tech Stack:** Python 3.14 (tomllib, string.Template, subprocess, colorsys), Bash for CLI wrapper, fzf for picker.

**Design doc:** `docs/plans/2026-02-25-color-theming-design.md`

---

### Task 1: Create the Ocean theme TOML (extract current colors)

**Files:**
- Create: `.config/themes/ocean.toml`

**Step 1: Create the theme file**

Extract all current hardcoded colors into the first theme TOML:

```toml
[meta]
name = "Ocean"
style = "dark"

[palette]
background    = "#0a0e14"
surface       = "#1a1e24"
foreground    = "#e0e0e0"
dim           = "#6e7681"
border        = "#3a3a4a"

[hues]
red           = "#dc143c"
orange        = "#ffa500"
yellow        = "#e5c07b"
green         = "#00ff7f"
cyan          = "#00bfff"
blue          = "#1e90ff"
magenta       = "#9370db"

[roles]
accent        = "blue"
accent_bright = "cyan"
error         = "red"
warning       = "orange"
success       = "green"

[emacs]
theme = "deeper-blue"

[emacs.faces]
mode-line-active-bg   = "#353550"
mode-line-active-fg   = "#b0b0c0"
mode-line-inactive-bg = "#2a2a3a"
mode-line-inactive-fg = "#7a7a8a"

[terminal]
# Override specific terminal colors that don't match hues exactly
black         = "#0a0e14"
white         = "#c0c0c0"
bright_black  = "#404040"
bright_red    = "#ff4060"
bright_green  = "#50ffa0"
bright_yellow = "#ffcc44"
bright_blue   = "#89cff0"
bright_magenta = "#b19cd9"
bright_cyan   = "#40d0ff"
bright_white  = "#ffffff"
```

**Step 2: Commit**

```bash
git add .config/themes/ocean.toml
git commit -m "add ocean theme TOML (extract current colors)"
```

---

### Task 2: Build the core theme engine

**Files:**
- Create: `.local/share/theme-engine/engine.py`

**Step 1: Write the engine**

The engine must:

1. **Load TOML** — read a theme file with `tomllib`
2. **Resolve roles** — if a role value matches a hue name, replace with that hue's hex value
3. **Derive terminal colors** — for any terminal color not explicitly set:
   - `black` = palette.background, `white` = palette.foreground
   - `red/green/yellow/blue/magenta/cyan` = matching hue (yellow = hues.yellow, not hues.orange)
   - `bright_*` = lightened ~20% variant (use `colorsys` HLS: increase lightness by 0.2, clamp to 1.0)
   - `bright_black` = palette.dim, `bright_white` = `#ffffff`
4. **Build variable namespace** — flat dict with every variable in three formats:
   - `name` = `#hex` (e.g., `accent` = `#1e90ff`)
   - `name_bare` = `hex` without `#` (e.g., `accent_bare` = `1e90ff`)
   - `name_rgba` = `r, g, b` decimal (e.g., `accent_rgba` = `30, 144, 255`)
   - `name_r`, `name_g`, `name_b` = individual decimal components
   - Terminal colors keyed as `terminal_red`, `terminal_bright_blue`, etc. (also with `_bare`, `_rgba` suffixes)
5. **Render templates** — for each `.tmpl` file in the templates dir, render with `string.Template.safe_substitute()` and write to the target path
6. **Generate `~/.emacs.d/theme.el`** — not a template, built programmatically from `[emacs]` section
7. **Update current symlink** — `~/.config/themes/current` -> the applied theme file
8. **Reload services** — run reload commands for each app

Template-to-target path mapping (hardcoded in engine):

```python
TEMPLATE_MAP = {
    "foot.ini.tmpl":              "~/.config/foot/foot.ini",
    "hyprland-colors.conf.tmpl":  "~/.config/hypr/hyprland-colors.conf",
    "hyprlock-colors.conf.tmpl":  "~/.config/hypr/hyprlock-colors.conf",
    "waybar-style.css.tmpl":      "~/.config/waybar/style.css",
    "swaync-style.css.tmpl":      "~/.config/swaync/style.css",
    "fzf-theme.sh.tmpl":          "~/.local/share/fzf-picker/theme.sh",
    "fuzzel.ini.tmpl":            "~/.config/fuzzel/fuzzel.ini",
    "claude-ask-colors.tmpl":     "~/.local/share/claude-ask/input.py",
}
```

Note on claude-ask: Since colors are embedded in a larger Python file (just 3 lines at lines 422-424), use a targeted sed-style replacement on the existing file rather than templating the whole thing. The engine should read `input.py`, find the `style_defs` dict lines, and replace the hex values.

Reload commands (run in order):

```python
RELOAD_COMMANDS = [
    ["hyprctl", "reload"],
    ["killall", "-SIGUSR2", "waybar"],
    ["swaync-client", "-rs"],
    ["emacsclient", "--eval", '(load "~/.emacs.d/theme.el" nil t)'],
    ["systemctl", "--user", "restart", "claude-ask"],
]
```

**Step 2: Verify engine loads ocean.toml and resolves correctly**

```bash
cd ~/.local/share/theme-engine
python3 -c "
from engine import load_theme, build_namespace
theme = load_theme('ocean')
ns = build_namespace(theme)
assert ns['accent'] == '#1e90ff'
assert ns['accent_bare'] == '1e90ff'
assert ns['terminal_red'] == '#dc143c'
assert ns['terminal_bright_blue_bare'] == '89cff0'
print('All assertions passed')
"
```

**Step 3: Commit**

```bash
git add .local/share/theme-engine/engine.py
git commit -m "add theme engine core: TOML loading, resolution, rendering"
```

---

### Task 3: Create templates for Foot and Hyprland

**Files:**
- Create: `.local/share/theme-engine/templates/foot.ini.tmpl`
- Create: `.local/share/theme-engine/templates/hyprland-colors.conf.tmpl`
- Create: `.local/share/theme-engine/templates/hyprlock-colors.conf.tmpl`
- Modify: `.config/hypr/hyprland.conf:64-65,81,237-238` — replace inline colors with variables, add `source` line

**Step 1: Create foot template**

```ini
[main]
font=Iosevka:size=11
pad=12x12

[colors]
alpha=0.85
background=${background_bare}
foreground=${foreground_bare}

# Terminal colors from theme
regular0=${terminal_black_bare}
regular1=${terminal_red_bare}
regular2=${terminal_green_bare}
regular3=${terminal_yellow_bare}
regular4=${terminal_blue_bare}
regular5=${terminal_magenta_bare}
regular6=${terminal_cyan_bare}
regular7=${terminal_white_bare}

bright0=${terminal_bright_black_bare}
bright1=${terminal_bright_red_bare}
bright2=${terminal_bright_green_bare}
bright3=${terminal_bright_yellow_bare}
bright4=${terminal_bright_blue_bare}
bright5=${terminal_bright_magenta_bare}
bright6=${terminal_bright_cyan_bare}
bright7=${terminal_bright_white_bare}

selection-foreground=${foreground_bare}
selection-background=1e3a5f
```

**Step 2: Create hyprland-colors.conf template**

This is a fragment sourced by the main config. Contains only the color variable assignments that hyprland.conf references:

```conf
# Theme colors — generated by theme engine. Do not edit.
$active_border_1 = ${accent_bare}cc
$active_border_2 = ${accent_bright_bare}cc
$inactive_border_1 = 003366cc
$inactive_border_2 = 00203fcc
$shadow_color = 1a1a1aee
$groupbar_active = ${terminal_bright_blue_bare}
$groupbar_inactive = 45475a
```

**Step 3: Modify hyprland.conf**

Add `source` line near the top of the file (after the monitor/env lines, before `general`):

```
source = ~/.config/hypr/hyprland-colors.conf
```

Replace hardcoded colors with variable references:

Line 64: `col.active_border = rgba($active_border_1) rgba($active_border_2) 45deg`
Line 65: `col.inactive_border = rgba($inactive_border_1) rgba($inactive_border_2) 45deg`
Line 81: `color = rgba($shadow_color)`
Line 237: `col.active = rgb($groupbar_active)`
Line 238: `col.inactive = rgb($groupbar_inactive)`

**Step 4: Create hyprlock-colors.conf template**

Hyprlock uses decimal `rgba(r, g, b, a)` format. Most colors here are white/transparent variants that don't change much per theme, but we template the ones that should:

```conf
# Theme colors for hyprlock — generated by theme engine. Do not edit.
$lock_text_color = rgba(${foreground_rgba}, 0.9)
$lock_text_dim = rgba(${foreground_rgba}, 0.6)
$lock_text_medium = rgba(${foreground_rgba}, 0.8)
$lock_text_faint = rgba(${foreground_rgba}, 0.5)
$lock_input_outer = rgba(${foreground_rgba}, 0.15)
$lock_input_inner = rgba(${background_rgba}, 0.5)
$lock_input_text = rgba(${foreground_rgba}, 0.9)
```

Then modify hyprlock.conf to `source` this file and use the variables (same pattern as hyprland).

**Step 5: Test rendering**

```bash
python3 -c "
from engine import load_theme, build_namespace, render_template
theme = load_theme('ocean')
ns = build_namespace(theme)
result = render_template('foot.ini.tmpl', ns)
assert '0a0e14' in result  # background
assert 'dc143c' in result  # red
print('Foot template renders correctly')
"
```

**Step 6: Commit**

```bash
git add .local/share/theme-engine/templates/foot.ini.tmpl \
        .local/share/theme-engine/templates/hyprland-colors.conf.tmpl \
        .local/share/theme-engine/templates/hyprlock-colors.conf.tmpl \
        .config/hypr/hyprland.conf
git commit -m "add foot/hyprland/hyprlock templates, source color fragments"
```

---

### Task 4: Create templates for Waybar and Swaync

**Files:**
- Create: `.local/share/theme-engine/templates/waybar-style.css.tmpl`
- Create: `.local/share/theme-engine/templates/swaync-style.css.tmpl`

**Step 1: Create waybar template**

Copy the current `style.css` content and replace hardcoded colors with template variables. Color mapping:

| Current hex | Template variable | Semantic meaning |
|---|---|---|
| `#e0e0e0` | `${foreground}` | Default text |
| `#c0c0c0` | `${terminal_white}` | Slightly dimmer text |
| `#808080` | `${dim}` | Dim/muted text |
| `#606060` | — | Keep hardcoded (very dim UI) |
| `#404040` | — | Keep hardcoded (off/disabled) |
| `#ffffff` | `${terminal_bright_white}` | Bright white |
| `#89cff0` | `${accent_bright}` | Accent/info |
| `#1e90ff` | `${accent}` | — |
| `#00bfff` | `${secondary}` (where `secondary` = resolved `accent_bright`) | Secondary accent |
| `#dc143c` | `${error}` | Error/critical/urgent |
| `#00ff7f` | `${success}` | Success/active |
| `#ffa500` | `${warning}` | Warning |
| `#003366` | — | Derived dark accent (keep or add `accent_dark` to palette) |
| `rgba(...)` backgrounds | Keep as-is | Glass effects are opacity-based, not theme-colored |

For the `@keyframes` animation colors (glow-blue, glow-green, glow-amber, etc.), these should also use template variables. The "glow" keyframes need a brighter variant — use the color + a lightened version. The engine should provide `name_light` variants or we hardcode the lightened versions in the template.

**Decision:** Add a `_light` suffix variant to the engine namespace (lighten by 30% for glow animations). This keeps templates clean.

For waybar, also template the `rgba(50, 50, 50, 0.7)` and similar semi-transparent backgrounds using `${surface_rgba}` where appropriate. Keep glass-effect `rgba(80, 80, 80, ...)` and `rgba(100, 100, 100, ...)` hardcoded since they're opacity effects, not theme colors.

The claude-ask waybar animation colors (`#b4befe`, `#a6e3a1`, `#fab387`, `#f9e2af`, `#585b70`) should map to:
- `#b4befe` (idle/base) → `${accent_bright}`
- `#a6e3a1` (speaking/done green) → `${success}`
- `#fab387` (tool_use/listening amber) → `${warning}`
- `#f9e2af` (worker-working yellow) → `${terminal_yellow}`
- `#585b70` (worker-pending dim) → `${dim}`

The glow keyframe midpoint colors (lighter versions like `#b4d8f0`, `#d4f0d3`, `#fcd5b8`) → `${accent_bright_light}`, `${success_light}`, `${warning_light}`

**Step 2: Create swaync template**

Swaync already uses `@define-color` variables — the template just replaces the `@define-color` block at the top. The rest of the CSS references `@text-color`, `@accent-color`, etc. and stays unchanged.

However, there are also ~20 hardcoded `rgba(137, 207, 240, ...)` values scattered through the file (box-shadows, hover glows). These are the accent color (137, 207, 240 = `#89cff0`) at various opacities. Replace these with `${accent_bright_rgba}` variants.

Similarly, `rgba(220, 20, 60, ...)` values = error color. Replace with `${error_rgba}`.

The `@define-color` block maps:

```css
@define-color cc-bg rgba(${surface_rgba}, 0.9);
@define-color noti-bg rgba(${surface_rgba}, 0.9);
@define-color noti-bg-darker rgba(${background_rgba}, 0.95);
@define-color noti-bg-hover rgba(${surface_r + 15}, ${surface_g + 15}, ${surface_b + 15}, 0.94);
...
@define-color text-color ${foreground};
@define-color text-color-secondary #c8c8c8;
@define-color text-color-disabled ${dim};
@define-color accent-color ${accent_bright};
@define-color accent-color-hover #a8dcf6;
@define-color border-color rgba(${accent_bright_rgba}, 0.35);
@define-color urgent-color ${error};
@define-color success-color ${success};
@define-color warning-color ${warning};
```

For the computed values like `noti-bg-hover` (surface + 15 per channel) and `accent-color-hover` (accent lightened), add pre-computed variants to the engine namespace rather than doing math in templates.

**Step 3: Test rendering**

```bash
python3 -c "
from engine import load_theme, build_namespace, render_template
theme = load_theme('ocean')
ns = build_namespace(theme)
waybar = render_template('waybar-style.css.tmpl', ns)
assert '#e0e0e0' not in waybar or '\${' not in waybar  # no unresolved vars
swaync = render_template('swaync-style.css.tmpl', ns)
assert '@define-color accent-color' in swaync
print('CSS templates render correctly')
"
```

**Step 4: Commit**

```bash
git add .local/share/theme-engine/templates/waybar-style.css.tmpl \
        .local/share/theme-engine/templates/swaync-style.css.tmpl
git commit -m "add waybar and swaync CSS templates"
```

---

### Task 5: Create templates for fzf, fuzzel, and claude-ask

**Files:**
- Create: `.local/share/theme-engine/templates/fzf-theme.sh.tmpl`
- Create: `.local/share/theme-engine/templates/fuzzel.ini.tmpl`
- Modify: `.local/share/theme-engine/engine.py` — add claude-ask patching logic

**Step 1: Create fzf template**

```bash
# Shared fzf theme for foot+fzf picker TUIs
# Source this, then: fzf "$${FZF_PICKER_OPTS[@]}" --border-label=' Label ' ...

FZF_PICKER_OPTS=(
    --height=100%
    --layout=reverse
    --no-sort
    --exact
    --no-scrollbar
    --info=hidden
    --border=rounded
    --pointer='▸'
    --marker='●'
    --separator='─'
    --color='bg:-1,bg+:#1a2a40,fg:${dim},fg+:${foreground}'
    --color='hl:${accent_bright},hl+:${accent_bright}'
    --color='info:#404040,marker:${success},spinner:${accent_bright}'
    --color='prompt:${accent},pointer:${accent}'
    --color='header:${accent_bright},header:bold'
    --color='border:${accent},label:${accent_bright},query:${foreground}'
    --color='separator:#303050'
)
```

Note: `$$` escapes the `$` for `string.Template` so `${FZF_PICKER_OPTS[@]}` doesn't get interpreted as a template variable.

**Step 2: Create fuzzel template**

```ini
prompt=:
icon-theme=hicolor

[colors]
background=${background_bare}ff
text=${foreground_bare}ff
match=${warning_bare}ff
selection=${surface_bare}dd
selection-text=${foreground_bare}ff
border=${accent_bare}ff

[border]

[dmenu]

[key-bindings]
```

**Step 3: Add claude-ask patching to engine**

Instead of templating the whole `input.py`, the engine patches just the 3 color lines:

```python
def patch_claude_ask(namespace):
    path = os.path.expanduser("~/.local/share/claude-ask/input.py")
    content = open(path).read()
    replacements = {
        '"conv-user"': f'"conv-user": "{namespace["dim"]}"',
        '"conv-asst"': f'"conv-asst": "#666666"',  # keep as-is or use dim variant
        '"separator"': f'"separator": "#444444"',   # keep as-is or derive
    }
    # Use regex to replace the style_defs lines
    ...
```

Actually, these 3 colors are very neutral grays that work with any theme. Consider skipping claude-ask from theming entirely for now and adding it later if needed.

**Step 4: Commit**

```bash
git add .local/share/theme-engine/templates/fzf-theme.sh.tmpl \
        .local/share/theme-engine/templates/fuzzel.ini.tmpl \
        .local/share/theme-engine/engine.py
git commit -m "add fzf and fuzzel templates, claude-ask patching"
```

---

### Task 6: Generate theme.el for Emacs

**Files:**
- Modify: `.local/share/theme-engine/engine.py` — add `generate_emacs_theme()` function
- Modify: `.emacs.d/init.org:126-138` — replace hardcoded theme with `(load theme.el)`

**Step 1: Add Emacs theme generation to engine**

The engine reads `[emacs]` and `[emacs.faces]` from the TOML and writes `~/.emacs.d/theme.el`:

```python
def generate_emacs_theme(theme_data):
    emacs = theme_data.get("emacs", {})
    theme_name = emacs.get("theme", "deeper-blue")
    faces = emacs.get("faces", {})

    lines = [
        f';; Generated by theme engine. Do not edit.',
        f'(load-theme \'{theme_name} t)',
    ]

    if faces:
        face_specs = []
        # Map face keys like "mode-line-active-bg" to face specs
        # Group by face name, collect attributes
        face_map = {}
        for key, value in faces.items():
            # key format: "face-name-attribute" e.g. "mode-line-active-bg"
            # Last segment is the attribute (bg, fg)
            parts = key.rsplit("-", 1)
            face_name = parts[0]
            attr = parts[1] if len(parts) > 1 else None
            face_map.setdefault(face_name, {})[attr] = value

        custom_faces = []
        for face, attrs in face_map.items():
            props = []
            if "bg" in attrs:
                props.append(f':background "{attrs["bg"]}"')
                # Add box for mode-line faces
                if "mode-line" in face:
                    props.append(f':box (:line-width 4 :color "{attrs["bg"]}")')
            if "fg" in attrs:
                props.append(f':foreground "{attrs["fg"]}"')
            props_str = " ".join(props)
            custom_faces.append(f"   '({face} ((t ({props_str}))))")

        if custom_faces:
            lines.append("(custom-set-faces")
            lines.append("   '(mode-line ((t (:height 0.8))))")
            lines.extend(custom_faces)
            lines[-1] += ")"  # close custom-set-faces

    output = "\n".join(lines) + "\n"
    path = os.path.expanduser("~/.emacs.d/theme.el")
    with open(path, "w") as f:
        f.write(output)
```

**Step 2: Modify init.org**

Replace lines 126-138 in `.emacs.d/init.org`:

```org
** themes
#+begin_src emacs-lisp
  ;; Theme loaded from system theme engine (~/.config/themes/)
  ;; Run `theme set <name>` to change. See docs/plans/2026-02-25-color-theming-design.md
  (load (expand-file-name "~/.emacs.d/theme.el") t t)
#+end_src
```

**Step 3: Generate initial theme.el from ocean.toml and verify**

```bash
python3 -c "
from engine import load_theme, generate_emacs_theme
theme = load_theme('ocean')
generate_emacs_theme(theme)
print(open(os.path.expanduser('~/.emacs.d/theme.el')).read())
"
```

Verify it produces the same `load-theme` + `custom-set-faces` as current init.org.

**Step 4: Tangle init.org and reload**

```bash
emacsclient --eval '(org-babel-tangle-file "~/.emacs.d/init.org")'
emacsclient --eval '(load "~/.emacs.d/theme.el" nil t)'
```

**Step 5: Commit**

```bash
git add .local/share/theme-engine/engine.py .emacs.d/init.org
git commit -m "generate theme.el from TOML, load in init.org"
```

---

### Task 7: Build the CLI wrapper and fzf picker

**Files:**
- Create: `.local/bin/theme`
- Create: `.local/bin/theme-picker`

**Step 1: Create the `theme` CLI**

Bash wrapper that calls the Python engine:

```bash
#!/usr/bin/env bash
set -euo pipefail

THEME_DIR="$HOME/.config/themes"
ENGINE="$HOME/.local/share/theme-engine/engine.py"

case "${1:-}" in
    list)
        for f in "$THEME_DIR"/*.toml; do
            name=$(basename "$f" .toml)
            current=""
            if [ -L "$THEME_DIR/current" ] && [ "$(readlink "$THEME_DIR/current")" = "$f" ]; then
                current=" *"
            fi
            echo "${name}${current}"
        done
        ;;
    current)
        if [ -L "$THEME_DIR/current" ]; then
            basename "$(readlink "$THEME_DIR/current")" .toml
        else
            echo "No theme set"
            exit 1
        fi
        ;;
    set)
        [ -z "${2:-}" ] && echo "Usage: theme set <name>" && exit 1
        python3 "$ENGINE" apply "$2"
        ;;
    edit)
        [ -z "${2:-}" ] && echo "Usage: theme edit <name>" && exit 1
        ${EDITOR:-vim} "$THEME_DIR/${2}.toml"
        ;;
    new)
        [ -z "${2:-}" ] && echo "Usage: theme new <name>" && exit 1
        if [ ! -L "$THEME_DIR/current" ]; then
            echo "No current theme to copy from"
            exit 1
        fi
        cp "$(readlink "$THEME_DIR/current")" "$THEME_DIR/${2}.toml"
        # Update the name in the new file
        sed -i "s/^name *= *\".*\"/name = \"${2}\"/" "$THEME_DIR/${2}.toml"
        ${EDITOR:-vim} "$THEME_DIR/${2}.toml"
        ;;
    *)
        echo "Usage: theme {list|current|set|edit|new} [name]"
        exit 1
        ;;
esac
```

Make executable: `chmod +x ~/.local/bin/theme`

**Step 2: Create the fzf picker**

```bash
#!/usr/bin/env bash
set -euo pipefail

source "$HOME/.local/share/fzf-picker/theme.sh"

THEME_DIR="$HOME/.config/themes"
CURRENT=$(theme current 2>/dev/null || echo "")

SELECTION=$(
    for f in "$THEME_DIR"/*.toml; do
        name=$(basename "$f" .toml)
        marker=""
        [ "$name" = "$CURRENT" ] && marker=" [active]"
        echo "${name}${marker}"
    done | fzf "${FZF_PICKER_OPTS[@]}" \
        --border-label=' Theme ' \
        --prompt='theme> '
)

[ -z "$SELECTION" ] && exit 0

# Strip the " [active]" marker if present
NAME="${SELECTION%% \[active\]}"

hyprctl dispatch exec "theme set '$NAME'"
```

Make executable: `chmod +x ~/.local/bin/theme-picker`

**Step 3: Test the CLI**

```bash
theme list         # should show "ocean"
```

**Step 4: Commit**

```bash
git add .local/bin/theme .local/bin/theme-picker
git commit -m "add theme CLI and fzf picker"
```

---

### Task 8: Wire up hyprland keybind for theme picker

**Files:**
- Modify: `.config/hypr/hyprland.conf` — add keybind for theme picker

**Step 1: Add keybind**

Add to the keybinds section of hyprland.conf:

```
bind = $mainMod, F6, exec, foot --app-id=fzf-picker -e theme-picker
```

(Pick an appropriate key — check existing binds to avoid conflicts.)

**Step 2: Commit**

```bash
git add .config/hypr/hyprland.conf
git commit -m "add keybind for theme picker"
```

---

### Task 9: End-to-end test with `theme set ocean`

**Step 1: Run the full apply**

```bash
theme set ocean
```

**Step 2: Verify each output**

- `diff ~/.config/foot/foot.ini` — should match current content
- `cat ~/.config/hypr/hyprland-colors.conf` — new file with color variables
- `hyprctl reload` should succeed
- Waybar should look identical
- Swaync notifications should look identical
- `cat ~/.emacs.d/theme.el` — should have `load-theme 'deeper-blue`
- `theme current` — should print `ocean`

**Step 3: Verify Emacs**

```bash
emacsclient --eval '(car custom-enabled-themes)'
# Should return: deeper-blue
```

---

### Task 10: Create a second theme to validate the system

**Files:**
- Create: `.config/themes/forest.toml`

**Step 1: Create the forest theme**

```bash
theme new forest
```

Then edit with green-focused colors:

```toml
[meta]
name = "Forest"
style = "dark"

[palette]
background    = "#1a1c1e"
surface       = "#2a2e2a"
foreground    = "#d4d4d4"
dim           = "#6a7a6a"
border        = "#3a4a3a"

[hues]
red           = "#e06060"
orange        = "#d4944a"
yellow        = "#c4b060"
green         = "#50c878"
cyan          = "#60b0a0"
blue          = "#6090c0"
magenta       = "#a070b0"

[roles]
accent        = "green"
accent_bright = "cyan"
error         = "red"
warning       = "orange"
success       = "green"

[emacs]
theme = "wheatgrass"

[emacs.faces]
mode-line-active-bg   = "#2a3a2a"
mode-line-active-fg   = "#b0c0b0"
mode-line-inactive-bg = "#1e2e1e"
mode-line-inactive-fg = "#6a7a6a"
```

**Step 2: Apply and verify**

```bash
theme set forest
```

Visually confirm: borders turn green, waybar accents are green, foot terminal colors shift, Emacs switches to wheatgrass theme.

**Step 3: Switch back**

```bash
theme set ocean
```

Confirm everything returns to the blue theme.

**Step 4: Commit**

```bash
git add .config/themes/forest.toml
git commit -m "add forest theme to validate theme switching"
```

---

### Task 11: Stow and final cleanup

**Step 1: Stow new files**

```bash
cd /home/ifit/dotfiles && stow .
```

**Step 2: Verify symlinks**

```bash
ls -la ~/.config/themes/ocean.toml
ls -la ~/.local/bin/theme
ls -la ~/.local/share/theme-engine/engine.py
```

All should be symlinks pointing into the dotfiles repo.

**Step 3: Final commit**

```bash
git add -A
git commit -m "stow theme engine files"
```
