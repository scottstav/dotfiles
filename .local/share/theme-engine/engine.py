#!/usr/bin/env python3
"""System-wide theme engine.

Switches the active theme by updating the current symlink and reloading
affected services.

Usage:
    python3 engine.py apply <theme-name>
"""

import os
import subprocess
import sys
import time

THEMES_DIR = os.path.expanduser("~/.config/themes")

RELOAD_COMMANDS = [
    ["hyprctl", "reload"],
    ["killall", "-SIGUSR2", "waybar"],
    ["swaync-client", "-rs"],
    ["emacsclient", "--eval", '(load "~/.emacs.d/theme.el" nil t)'],
    ["systemctl", "--user", "restart", "aside-daemon"],
    ["sleep", "1"],
    ["systemctl", "--user", "restart", "aside-overlay"],
]


def switch_theme(name: str) -> None:
    """Point current -> name and reload services."""
    theme_dir = os.path.join(THEMES_DIR, name)
    if not os.path.isdir(theme_dir):
        print(f"Theme directory not found: {theme_dir}", file=sys.stderr)
        sys.exit(1)

    link_path = os.path.join(THEMES_DIR, "current")
    if os.path.islink(link_path) or os.path.exists(link_path):
        os.remove(link_path)
    os.symlink(name, link_path)

    reload_services()

    # Read theme name from theme.toml if available
    try:
        import tomllib
    except ModuleNotFoundError:
        import tomli as tomllib
    toml_path = os.path.join(theme_dir, "theme.toml")
    display_name = name
    if os.path.isfile(toml_path):
        with open(toml_path, "rb") as f:
            meta = tomllib.load(f).get("meta", {})
            display_name = meta.get("name", name)
    print(f"Applied theme: {display_name}")


def reload_services() -> None:
    """Run reload commands for each app."""
    for cmd in RELOAD_COMMANDS:
        try:
            r = subprocess.run(cmd, capture_output=True, text=True)
            if r.returncode != 0:
                print(f"  warn: {cmd[0]} failed (exit {r.returncode})", file=sys.stderr)
        except FileNotFoundError:
            pass

    # Hyprpaper needs kill + restart
    subprocess.run(["killall", "hyprpaper"], capture_output=True)
    time.sleep(0.5)
    subprocess.run(["hyprctl", "dispatch", "exec", "hyprpaper"], capture_output=True)


if __name__ == "__main__":
    if len(sys.argv) < 3 or sys.argv[1] != "apply":
        print("Usage: python3 engine.py apply <theme-name>", file=sys.stderr)
        sys.exit(1)

    switch_theme(sys.argv[2])
