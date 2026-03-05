---
allowed-tools: Read, Write, Edit, Bash, AskUserQuestion, Glob, Grep
description: Generate a complete desktop theme from a wallpaper image. Analyzes colors, mood, and contrast to produce a full theme directory with all config files.
---

You are a theme generator. Given a wallpaper image, you analyze it visually and produce a complete theme directory that harmonizes with the image's color palette and mood.

## Input

The user provides an image path as `$ARGUMENTS`. If no argument is given, ask for the path using `AskUserQuestion`.

The image path may be:
- An absolute path
- A path with `~` (expand to `/home/ifit`)
- A filename only — assume it lives in `~/Dropbox/Images/wallpapers/`

Verify the file exists before proceeding.

## Step 1: Analyze the image

Use the Read tool to view the image file. Study it carefully and identify:

1. **Dominant colors** — the 3-5 most prominent colors in the image
2. **Mood** — warm/cool, dark/light, vibrant/muted, calm/energetic
3. **Color temperature** — overall warm (amber, red, orange) vs cool (blue, teal, purple)
4. **Contrast level** — high contrast (vivid) vs low contrast (soft/dreamy)
5. **Style classification** — dark or light (use this for `meta.style`)

Briefly describe what you see and the palette you'll derive.

## Step 2: Derive a theme name

Based on the image mood and content, pick a short, evocative single-word name (e.g., "ember", "twilight", "sakura", "glacier", "moss", "obsidian", "dusk", "coral"). Avoid generic names like "cool" or "dark". The name should feel like it belongs to the image.

Tell the user the name you chose.

## Step 3: Generate the color palette

Design a complete color palette following this schema. Every field is required unless noted.

```toml
[meta]
name = "Capitalized Name"
style = "dark"           # or "light" — based on image analysis
font = "Iosevka 12"     # always use this
wallpaper = "~/Dropbox/Images/wallpapers/<filename>"

[palette]
background    = "#xxxxxx"   # darkest tone from image (dark theme) or lightest (light)
surface       = "#xxxxxx"   # slightly lighter/elevated than background
foreground    = "#xxxxxx"   # main text — high contrast against background
dim           = "#xxxxxx"   # muted text — medium contrast against background
border        = "#xxxxxx"   # subtle separator — between dim and background

[hues]
red           = "#xxxxxx"
orange        = "#xxxxxx"
yellow        = "#xxxxxx"
green         = "#xxxxxx"
cyan          = "#xxxxxx"
blue          = "#xxxxxx"
magenta       = "#xxxxxx"

[roles]
accent        = "<hue-name>"    # most prominent/vibrant color from the image
accent_bright = "<hue-name>"    # a brighter complementary hue
secondary     = "#xxxxxx"       # a muted mid-tone from the image
error         = "red"           # always red
warning       = "orange"        # always orange
success       = "green"         # always green

[emacs]
theme = "<theme-name>"
```

### Color rules

**Palette:**
- `background`: For dark themes, pick from the image's darkest tones (desaturate if needed). For light themes, pick from the lightest.
- `surface`: A step lighter (dark) or darker (light) than background. Should feel like a raised panel.
- `foreground`: Near-white for dark themes (#c0-#e0 range), near-black for light themes. Tint slightly toward the image's color temperature.
- `dim`: Midpoint between foreground and background, leaning toward the image's muted tones.
- `border`: Between surface and dim. Should be subtle but visible.

**Hues:**
- All 7 hues are required. They should feel like they belong to the same world as the wallpaper.
- Shift each hue toward the image's color temperature: warm images get warmer reds/oranges/yellows, cool images get cooler blues/cyans.
- The hues don't need to appear literally in the image — they need to *harmonize* with it.
- Keep reasonable saturation — not neon, not mud.

**Roles:**
- `accent`: Pick the most vibrant or characteristic color from the image. Use a hue name (e.g., `"blue"`, `"cyan"`, `"green"`) that matches.
- `accent_bright`: A lighter/brighter complementary hue to the accent. Use a hue name.
- `secondary`: A muted mid-tone hex value. Good for inactive UI elements.
- `error`, `warning`, `success`: Always use the hue names `"red"`, `"orange"`, `"green"`.

**Emacs theme:**
Choose from this curated list based on the mood:
- Dark + cool → `"deeper-blue"`
- Dark + warm → `"wheatgrass"`
- Dark + neutral → `"wombat"`
- Dark + high contrast → `"tsdh-dark"`
- Dark + very dark/minimal → `"modus-vivendi"`
- Light + any → `"modus-operandi"`
- Light + warm → `"adwaita"`
- Light + retro → `"tango"`

### Terminal palette (optional)

Only include a `[terminal]` section in `theme.toml` if the auto-derived terminal colors (hues mapped to ANSI, bright variants lightened 20%) would clash with the theme. Most themes can omit this.

### Contrast check

Before finalizing, mentally verify:
- Foreground on background has strong contrast (light text on dark or vice versa)
- Dim text is still readable on background
- Accent colors are visible on both background and surface
- The overall palette feels cohesive — like one image, not a random color picker

## Step 4: Create the theme directory

Create the full theme directory with all config files:

### 4a: Bootstrap from current theme

```bash
THEME_NAME="<name>"
THEME_DIR="/home/ifit/dotfiles/.config/themes/${THEME_NAME}"
mkdir -p "${THEME_DIR}"
CURRENT_THEME=$(readlink -f ~/.config/themes/current)
cp -r "${CURRENT_THEME}"/* "${THEME_DIR}/"
```

This copies all config files from the currently active theme as a starting point.

### 4b: Write theme.toml

Write the palette TOML you designed in Step 3 to `${THEME_DIR}/theme.toml`, replacing the copied one.

### 4c: Update colors in each config file

Now the creative part. For each config file copied from the current theme, read it, understand its structure, and replace the old theme's colors with your new palette's colors. You need to intelligently map colors to roles:

- **Background/surface colors** → new `background`, `surface`
- **Text/foreground colors** → new `foreground`, `dim`
- **Border/separator colors** → new `border`
- **Accent/highlight colors** → new accent hue, accent_bright hue
- **Semantic colors (error, warning, success)** → new `red`, `orange`, `green` hues
- **Terminal ANSI colors** → map from new hues

Read the current theme's `theme.toml` first to understand what colors map to what roles, then substitute your new colors.

Here's what each file looks like and how to handle it:

**`foot.ini`** — INI format, bare hex colors (no `#` prefix). Contains terminal foreground/background and full 16-color ANSI palette. Replace all color values.

**`waybar-style.css`** — CSS with hex colors (`#xxxxxx`) and `rgba()` values. Contains background, text, border, accent colors for the bar and modules. Replace all color values.

**`swaync-style.css`** — CSS with `@define-color` declarations at the top, hex colors, and `rgba()` throughout. Update the `@define-color` lines and any inline colors.

**`fuzzel.ini`** — INI format, bare hex colors with alpha suffix (e.g., `1a1b26ff`). 8 hex chars = 6 color + 2 alpha. Replace the color portion, keep alpha values.

**`fzf-theme.sh`** — Shell script setting `FZF_DEFAULT_OPTS` with `--color` flag. Uses format like `--color=bg:#xxxxxx,fg:#xxxxxx,...`. Replace each hex color.

**`hyprpaper.conf`** — Just a wallpaper path. Update to point to the new wallpaper: `~/Dropbox/Images/wallpapers/<filename>`.

**`hyprland-colors.conf`** — Hyprland variable declarations. Bare hex with alpha suffix (e.g., `$color_bg = rgb(1a1b26)`). Replace hex values inside `rgb()`.

**`hyprlock-colors.conf`** — Hyprland lock screen colors. Uses `rgba()` format (e.g., `$hl_bg = rgba(1a1b26ff)`). Replace hex+alpha values.

**`alacritty.toml`** — TOML format with hex string values (e.g., `"#1a1b26"`). Contains primary colors and full 16-color palette. Replace all hex strings.

**`theme.el`** — Emacs lisp. Just loads a built-in theme. Update the theme name from the `[emacs]` section of theme.toml.

**`aside-theme.toml`** — TOML config for aside overlay. Contains hex colors with alpha (e.g., `"#1a1b26e6"`). Replace color portions, preserve alpha values.

Use the Edit tool for each file — read it, then make targeted replacements.

## Step 5: Copy wallpaper if needed

If the image is NOT already in `~/Dropbox/Images/wallpapers/`, copy it there:

```bash
cp "<original-path>" ~/Dropbox/Images/wallpapers/
```

The `meta.wallpaper` path in theme.toml should always point to the `~/Dropbox/Images/wallpapers/` copy.

## Step 6: Report

Print the full generated `theme.toml` so the user can review the palette.

Tell the user:
- The theme name and directory location
- A brief description of the vibe/palette choices
- How to apply: `theme set <name>`
- Remind them you did NOT apply it — they switch when ready
