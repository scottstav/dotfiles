#!/usr/bin/env python3
"""System-wide color theme engine.

Reads a TOML theme file, resolves color roles, derives terminal colors,
builds a variable namespace with multiple formats, renders templates,
generates an Emacs theme file, and reloads affected services.

Usage:
    python3 engine.py apply <theme-name>
"""

import colorsys
import os
import string
import subprocess
import sys
import time

try:
    import tomllib
except ModuleNotFoundError:
    import tomli as tomllib  # Python < 3.11 fallback

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

THEMES_DIR = os.path.expanduser("~/.config/themes")
TEMPLATES_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "templates")

TEMPLATE_MAP = {
    "foot.ini.tmpl":              "~/.config/foot/foot.ini",
    "hyprland-colors.conf.tmpl":  "~/.config/hypr/hyprland-colors.conf",
    "hyprlock-colors.conf.tmpl":  "~/.config/hypr/hyprlock-colors.conf",
    "waybar-style.css.tmpl":      "~/.config/waybar/style.css",
    "swaync-style.css.tmpl":      "~/.config/swaync/style.css",
    "fzf-theme.sh.tmpl":          "~/.local/share/fzf-picker/theme.sh",
    "fuzzel.ini.tmpl":            "~/.config/fuzzel/fuzzel.ini",
    "hyprpaper.conf.tmpl":        "~/.config/hypr/hyprpaper.conf",
    "claude-overlay-config.tmpl": "~/.config/claude-overlay/config",
}

RELOAD_COMMANDS = [
    ["hyprctl", "reload"],
    ["killall", "-SIGUSR2", "waybar"],
    ["swaync-client", "-rs"],
    ["emacsclient", "--eval", '(load "~/.emacs.d/theme.el" nil t)'],
    ["systemctl", "--user", "restart", "claude-ask"],
    ["systemctl", "--user", "restart", "claude-overlay"],
]

HYPRPAPER_RELOAD = ["killall", "hyprpaper"]
HYPRPAPER_START = ["hyprctl", "dispatch", "exec", "hyprpaper"]

# The six chromatic terminal color names and their hue counterparts.
TERMINAL_HUES = {
    "red":     "red",
    "green":   "green",
    "yellow":  "yellow",
    "blue":    "blue",
    "magenta": "magenta",
    "cyan":    "cyan",
}

# ---------------------------------------------------------------------------
# Color helpers
# ---------------------------------------------------------------------------

def hex_to_rgb(h: str) -> tuple[int, int, int]:
    """Convert '#rrggbb' to (r, g, b) ints."""
    h = h.lstrip("#")
    return int(h[0:2], 16), int(h[2:4], 16), int(h[4:6], 16)


def rgb_to_hex(r: int, g: int, b: int) -> str:
    """Convert (r, g, b) ints to '#rrggbb'."""
    return f"#{r:02x}{g:02x}{b:02x}"


def lighten(hex_color: str, amount: float) -> str:
    """Lighten a hex color by *amount* (0-1) in HLS space."""
    r, g, b = hex_to_rgb(hex_color)
    h, l, s = colorsys.rgb_to_hls(r / 255.0, g / 255.0, b / 255.0)
    l = min(l + amount, 1.0)
    nr, ng, nb = colorsys.hls_to_rgb(h, l, s)
    return rgb_to_hex(int(nr * 255), int(ng * 255), int(nb * 255))


# ---------------------------------------------------------------------------
# 1. Load TOML theme
# ---------------------------------------------------------------------------

def load_theme(name: str) -> dict:
    """Load a theme TOML file from ~/.config/themes/<name>.toml."""
    path = os.path.join(THEMES_DIR, f"{name}.toml")
    with open(path, "rb") as f:
        return tomllib.load(f)


# ---------------------------------------------------------------------------
# 2. Resolve roles — hue name -> hex, or pass-through #hex
# ---------------------------------------------------------------------------

def resolve_roles(theme: dict) -> dict:
    """Return a dict of role_name -> resolved hex color."""
    hues = theme.get("hues", {})
    roles = theme.get("roles", {})
    resolved = {}
    for role, value in roles.items():
        if value.startswith("#"):
            resolved[role] = value
        elif value in hues:
            resolved[role] = hues[value]
        else:
            # Unknown reference — keep raw value as a fallback
            resolved[role] = value
    return resolved


# ---------------------------------------------------------------------------
# 3. Derive terminal colors
# ---------------------------------------------------------------------------

def derive_terminal_colors(theme: dict) -> dict:
    """Build the full 16-color terminal palette, filling gaps with defaults."""
    palette = theme.get("palette", {})
    hues = theme.get("hues", {})
    explicit = dict(theme.get("terminal", {}))
    result = {}

    # --- Base 8 colors ---
    result["black"] = explicit.get("black", palette.get("background", "#000000"))
    result["white"] = explicit.get("white", palette.get("foreground", "#ffffff"))

    for tname, hname in TERMINAL_HUES.items():
        result[tname] = explicit.get(tname, hues.get(hname, "#888888"))

    # --- Bright variants ---
    result["bright_black"] = explicit.get("bright_black", palette.get("dim", "#808080"))
    result["bright_white"] = explicit.get("bright_white", "#ffffff")

    for tname, hname in TERMINAL_HUES.items():
        key = f"bright_{tname}"
        if key in explicit:
            result[key] = explicit[key]
        else:
            # Lighten the base color by 20%
            result[key] = lighten(result[tname], 0.2)

    return result


# ---------------------------------------------------------------------------
# 4. Build variable namespace
# ---------------------------------------------------------------------------

def _add_color_variants(ns: dict, name: str, hex_color: str) -> None:
    """Add all suffix variants for a single color into *ns*."""
    r, g, b = hex_to_rgb(hex_color)
    ns[name]            = hex_color
    ns[f"{name}_bare"]  = hex_color.lstrip("#")
    ns[f"{name}_rgba"]  = f"{r}, {g}, {b}"
    ns[f"{name}_r"]     = str(r)
    ns[f"{name}_g"]     = str(g)
    ns[f"{name}_b"]     = str(b)
    ns[f"{name}_light"] = lighten(hex_color, 0.3)


def build_namespace(theme: dict) -> dict:
    """Build a flat variable namespace with every color in multiple formats."""
    ns: dict[str, str] = {}

    # Palette colors
    for key, value in theme.get("palette", {}).items():
        _add_color_variants(ns, key, value)

    # Hue colors
    for key, value in theme.get("hues", {}).items():
        _add_color_variants(ns, key, value)

    # Resolved roles
    resolved_roles = resolve_roles(theme)
    for key, value in resolved_roles.items():
        _add_color_variants(ns, key, value)

    # Terminal colors
    terminal = derive_terminal_colors(theme)
    for key, value in terminal.items():
        _add_color_variants(ns, f"terminal_{key}", value)

    # Non-color values from [meta]
    meta = theme.get("meta", {})
    if "wallpaper" in meta:
        ns["wallpaper"] = meta["wallpaper"]

    return ns


# ---------------------------------------------------------------------------
# 5. Render templates
# ---------------------------------------------------------------------------

def render_templates(ns: dict) -> None:
    """Render all .tmpl files from the templates dir using safe_substitute."""
    if not os.path.isdir(TEMPLATES_DIR):
        return

    for tmpl_name, target_path in TEMPLATE_MAP.items():
        tmpl_path = os.path.join(TEMPLATES_DIR, tmpl_name)
        if not os.path.isfile(tmpl_path):
            continue

        with open(tmpl_path, "r") as f:
            template = string.Template(f.read())

        rendered = template.safe_substitute(ns)

        target = os.path.expanduser(target_path)
        os.makedirs(os.path.dirname(target), exist_ok=True)
        with open(target, "w") as f:
            f.write(rendered)


# ---------------------------------------------------------------------------
# 6. Generate ~/.emacs.d/theme.el
# ---------------------------------------------------------------------------

def generate_emacs_theme(theme_data: dict) -> None:
    """Write ~/.emacs.d/theme.el from the [emacs] section of the theme."""
    emacs = theme_data.get("emacs", {})
    theme_name = emacs.get("theme", "deeper-blue")
    faces = emacs.get("faces", {})

    # Resolve any face values that reference hue names
    hues = theme_data.get("hues", {})
    resolved_faces = {}
    for key, value in faces.items():
        if value.startswith("#"):
            resolved_faces[key] = value
        elif value in hues:
            resolved_faces[key] = hues[value]
        else:
            resolved_faces[key] = value

    lines = [
        ";; Generated by theme engine. Do not edit.",
        f"(load-theme '{theme_name} t)",
    ]

    if resolved_faces:
        # Group by face name, collect attributes
        face_map: dict[str, dict[str, str]] = {}
        for key, value in resolved_faces.items():
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
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w") as f:
        f.write(output)


# ---------------------------------------------------------------------------
# 7. Update current symlink
# ---------------------------------------------------------------------------

def update_current_symlink(name: str) -> None:
    """Point ~/.config/themes/current -> the applied theme file."""
    theme_file = os.path.join(THEMES_DIR, f"{name}.toml")
    link_path = os.path.join(THEMES_DIR, "current")

    if os.path.islink(link_path) or os.path.exists(link_path):
        os.remove(link_path)
    os.symlink(theme_file, link_path)


# ---------------------------------------------------------------------------
# 8. Reload services
# ---------------------------------------------------------------------------

def reload_services() -> None:
    """Run reload commands for each app. Ignore failures."""
    for cmd in RELOAD_COMMANDS:
        try:
            subprocess.run(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        except FileNotFoundError:
            pass

    # Hyprpaper needs kill + restart (no reload signal)
    try:
        subprocess.run(HYPRPAPER_RELOAD, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        time.sleep(0.5)
        subprocess.run(HYPRPAPER_START, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    except FileNotFoundError:
        pass


# ---------------------------------------------------------------------------
# Orchestrator
# ---------------------------------------------------------------------------

def apply_theme(name: str) -> None:
    """Full pipeline: load, resolve, render, generate, symlink, reload."""
    theme = load_theme(name)
    ns = build_namespace(theme)

    render_templates(ns)
    generate_emacs_theme(theme)
    update_current_symlink(name)
    reload_services()

    print(f"Applied theme: {theme.get('meta', {}).get('name', name)}")


# ---------------------------------------------------------------------------
# CLI entry point
# ---------------------------------------------------------------------------

if __name__ == "__main__":
    if len(sys.argv) < 3 or sys.argv[1] != "apply":
        print("Usage: python3 engine.py apply <theme-name>", file=sys.stderr)
        sys.exit(1)

    theme_name = sys.argv[2]
    apply_theme(theme_name)
