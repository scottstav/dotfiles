---
allowed-tools: Read, Write, Bash, AskUserQuestion, Glob, Grep
description: Generate a complete desktop theme from a wallpaper image. Analyzes colors, mood, and contrast to produce a harmonized theme TOML.
---

You are a theme generator. Given a wallpaper image, you analyze it visually and produce a complete theme TOML file that harmonizes with the image's color palette and mood.

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

## Step 3: Generate the theme TOML

Produce a complete theme file following this exact schema. Every field is required unless noted.

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

Only include a `[terminal]` section if the auto-derived terminal colors (hues mapped to ANSI, bright variants lightened 20%) would clash with the theme. Most themes can omit this.

### Contrast check

Before finalizing, mentally verify:
- Foreground on background has strong contrast (light text on dark or vice versa)
- Dim text is still readable on background
- Accent colors are visible on both background and surface
- The overall palette feels cohesive — like one image, not a random color picker

## Step 4: Write the file

Write the theme TOML to: `/home/ifit/dotfiles/.config/themes/<name>.toml`

## Step 5: Copy wallpaper if needed

If the image is NOT already in `~/Dropbox/Images/wallpapers/`, copy it there:

```bash
cp "<original-path>" ~/Dropbox/Images/wallpapers/
```

The `meta.wallpaper` path in the TOML should always point to the `~/Dropbox/Images/wallpapers/` copy.

## Step 6: Report

Print the full generated TOML so the user can review it.

Tell the user:
- The theme name and file location
- A brief description of the vibe/palette choices
- How to apply: `theme apply <name>`
- Remind them you did NOT apply it — they switch when ready
