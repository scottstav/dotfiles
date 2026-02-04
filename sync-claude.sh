#!/usr/bin/env bash
# Sync ~/.claude agents and commands into dotfiles and re-stow.
# Run this after creating new agents or commands in ~/.claude/.

set -euo pipefail

DOTFILES="$HOME/dotfiles"
CLAUDE_HOME="$HOME/.claude"

for dir in agents commands; do
    src="$CLAUDE_HOME/$dir"
    dest="$DOTFILES/.claude/$dir"
    [ -d "$src" ] || continue
    mkdir -p "$dest"

    for f in "$src"/*; do
        [ -f "$f" ] || continue
        # Skip files that are already symlinks (already managed by stow)
        [ -L "$f" ] && continue
        name="$(basename "$f")"
        echo "Adopting $dir/$name"
        cp "$f" "$dest/$name"
        rm "$f"
    done
done

cd "$DOTFILES" && stow -v .
