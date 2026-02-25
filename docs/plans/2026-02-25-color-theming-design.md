# System-Wide Color Theming

## Overview

A template-based color theme engine that lets you define themes as TOML files and apply them across your entire desktop: Hyprland, Waybar, Foot, Swaync, Emacs, fzf pickers, fuzzel, and claude-ask. Switch themes with `theme set <name>` or an fzf picker.

## Theme TOML Format

~25 values per theme. Lives in `~/.config/themes/<name>.toml`.

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
accent        = "blue"         # references hues.blue
accent_bright = "cyan"         # references hues.cyan
error         = "red"
warning       = "orange"
success       = "green"

[emacs]
theme = "deeper-blue"

[emacs.faces]
# Optional face overrides (attribute = "palette-ref" or "#hex")
# mode-line-active.bg = "#353550"

[terminal]
# Auto-derived from palette + hues if omitted.
# Override individual colors:
# bright0 = "#404040"
```

### Derivation Rules

When `[terminal]` colors are omitted:

- `black` = palette.background, `white` = palette.foreground
- `red`, `green`, `yellow`, `blue`, `magenta`, `cyan` = matching hue
- `bright_*` = lightened ~20% variant of each normal color
- `bright_black` = palette.dim, `bright_white` = `#ffffff`

Explicit values override derived ones.

### Role Resolution

Role values can be either a hue name (resolved to that hue's hex) or a raw `#hex` value.

## File Layout

```
~/.config/themes/
  current              # symlink -> ocean.toml (active theme)
  ocean.toml
  forest.toml
  ...

~/.local/bin/
  theme                # CLI entry point
  theme-picker         # fzf picker (foot --app-id=fzf-picker -e theme-picker)

~/.local/share/theme-engine/
  engine.py            # core: read TOML, render templates, reload apps
  templates/
    foot.ini.tmpl
    hyprland-colors.conf.tmpl
    hyprlock-colors.conf.tmpl
    waybar-style.css.tmpl
    swaync-style.css.tmpl
    fzf-theme.sh.tmpl
    claude-ask-colors.tmpl
    fuzzel.ini.tmpl
```

## Template System

Templates use Python `string.Template` syntax (`${variable}` placeholders).

The engine exposes a flat namespace of variables derived from the TOML:

| Variable pattern | Example | Use case |
|---|---|---|
| `${name}` | `${accent}` -> `#1e90ff` | CSS, fzf (with `#` prefix) |
| `${name_bare}` | `${accent_bare}` -> `1e90ff` | Foot, Hyprland (no `#`) |
| `${name_rgba}` | `${accent_rgba}` -> `30, 144, 255` | Hyprlock (decimal rgba) |
| `${terminal_COLOR}` | `${terminal_red}` -> `#dc143c` | Terminal colors |
| `${terminal_COLOR_bare}` | `${terminal_bright_blue_bare}` -> `89cff0` | Foot terminal |

## Config Targets

### Hyprland (`hyprland-colors.conf`)

Sourced from main `hyprland.conf` via `source = ~/.config/hypr/hyprland-colors.conf`. Contains only color variables (border colors, shadow colors, group bar colors). Main config stays hand-editable.

### Hyprlock (`hyprlock-colors.conf`)

Same pattern — sourced color fragment. Hyprlock uses decimal `rgba(r, g, b, a)` format.

### Foot (`foot.ini`)

Full template. Foot has no include mechanism. Colors section uses bare hex without `#`.

### Waybar (`style.css`)

Full template. CSS with hardcoded hex values — no native variable system worth using.

### Swaync (`style.css`)

Full template. Currently uses `@define-color` variables which map nicely to template vars.

### Fzf picker (`theme.sh`)

Full template. Shell script with `--color` flags using `#hex` format.

### Fuzzel (`fuzzel.ini`)

Full template. INI colors section.

### Claude-ask (`input.py`)

Sed/patch the 3 color values in the Python dict, or template the relevant section.

### Emacs (`~/.emacs.d/theme.el`)

Not a template — the engine generates this `.el` file directly from the `[emacs]` TOML section. Contains:

```elisp
(load-theme 'deeper-blue t)
(custom-set-faces
 '(mode-line-active ((t (:background "#353550" ...)))))
```

The `init.org` themes block is replaced with:

```elisp
(load (expand-file-name "~/.emacs.d/theme.el") t t)
```

Live reload: `emacsclient --eval '(load "~/.emacs.d/theme.el" nil t)'`

## Reload Strategy

| App | Method |
|---|---|
| Hyprland | `hyprctl reload` |
| Waybar | `killall -SIGUSR2 waybar` |
| Swaync | `swaync-client -rs` |
| Foot | New terminals pick it up; existing ones unchanged |
| Emacs | `emacsclient --eval '(load "~/.emacs.d/theme.el" nil t)'` |
| Fzf | Sourced on next picker launch |
| Fuzzel | Next launch |
| Claude-ask | `systemctl --user restart claude-ask` |

## CLI Interface

```bash
theme list              # list available themes
theme current           # print active theme name
theme set <name>        # apply theme: render templates, write configs, reload
theme edit <name>       # open theme TOML in $EDITOR
theme new <name>        # copy current theme as starting point, open in $EDITOR
```

## Fzf Picker

`theme-picker` script launched via `foot --app-id=fzf-picker -e theme-picker`. Lists available themes, selecting one runs `theme set <name>` via `hyprctl dispatch exec`.

## Migration Path

1. Extract current hardcoded colors into `ocean.toml` (the first theme)
2. Create templates from existing configs, replacing hardcoded colors with `${variables}`
3. Add `source` lines to hyprland/hyprlock configs for color fragments
4. Replace `init.org` theme block with `(load "~/.emacs.d/theme.el" t t)`
5. Verify: running `theme set ocean` produces output identical to current configs
6. Create a second theme (e.g., `forest`) to validate the system works

## Implementation Language

Python. Uses `tomllib` (stdlib 3.11+) for TOML parsing and `string.Template` for rendering. Single `engine.py` file, no external dependencies.
