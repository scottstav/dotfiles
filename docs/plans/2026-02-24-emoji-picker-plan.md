# Emoji Picker Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace bemoji + fuzzel with a custom foot + fzf emoji picker that has exact matching and alias-enriched search.

**Architecture:** Single bash script (`emoji-picker`) reads an alias-enriched emoji database, presents it via fzf with history at top, copies selection to clipboard and auto-pastes. Launched via foot with existing `fzf-picker` window rules.

**Tech Stack:** Bash, fzf, wl-copy, wtype, foot

---

### Task 1: Write the emoji-picker script

**Files:**
- Create: `.local/bin/emoji-picker`

**Step 1: Create the script**

```bash
#!/bin/bash
# Emoji picker — foot + fzf with aliases and history

DB="$HOME/.local/share/emoji-picker/emojis.txt"
HISTORY="$HOME/.local/state/emoji-history.txt"

# Build list: history (deduped) first, then remaining emojis
build_list() {
    if [[ -f "$HISTORY" ]]; then
        # History entries first (deduped, preserving order)
        awk '!seen[$0]++' "$HISTORY"
        # Then remaining emojis not in history
        grep -Fxvf "$HISTORY" "$DB"
    else
        cat "$DB"
    fi
}

SELECTION=$(build_list | fzf \
    --exact \
    --height=100% \
    --layout=reverse \
    --no-sort \
    --delimiter='\t' \
    --with-nth=1 \
    --prompt='emoji> ')

[[ -z "$SELECTION" ]] && exit 0

# Extract just the emoji character (first field before space)
EMOJI=$(echo "$SELECTION" | cut -d' ' -f1)

# Update history — prepend selection, keep max 500 lines
mkdir -p "$(dirname "$HISTORY")"
{ echo "$SELECTION"; cat "$HISTORY" 2>/dev/null; } | awk '!seen[$0]++' | head -500 > "$HISTORY.tmp"
mv "$HISTORY.tmp" "$HISTORY"

# Copy to clipboard and auto-paste
printf '%s' "$EMOJI" | wl-copy
sleep 0.05
wtype -M ctrl v -m ctrl
```

**Step 2: Make executable**

Run: `chmod +x /home/ifit/dotfiles/.local/bin/emoji-picker`

**Step 3: Commit**

```
git add .local/bin/emoji-picker
git commit -m "feat: add emoji-picker script with fzf exact match and history"
```

---

### Task 2: Generate alias-enriched emoji database

**Files:**
- Create: `.local/share/emoji-picker/emojis.txt`

The source is `/home/ifit/.local/share/bemoji/emojis.txt` (3624 lines, format: `<emoji> <name>`).

The output format uses a tab separator between the display portion and the alias portion:

```
😤 face with steam from nose	smoke triumph badass huff snort angry frustrated
🙈 see-no-evil monkey	hide peek shy oops cant-look
🔥 fire	flame hot lit fire burning
💀 skull	dead death rip oof skeleton
```

fzf searches the full line (both columns) but `--with-nth=1` only displays the first tab-delimited field.

**Step 1: Create the emoji-picker data directory**

Run: `mkdir -p /home/ifit/dotfiles/.local/share/emoji-picker`

**Step 2: Generate aliases**

Process the bemoji database in batches of ~300 emojis through Claude. For each emoji, generate 3-8 common search aliases — slang, synonyms, related concepts, common names people would type when looking for that emoji.

Rules for alias generation:
- Skin tone variants (e.g. "thumbs up: medium skin tone") get the same aliases as their base emoji
- Flag emojis get country names, abbreviations, demonyms
- Object/symbol emojis get common use-case words
- Don't repeat words already in the official name
- Keep aliases lowercase, space-separated

Save the full enriched database to `.local/share/emoji-picker/emojis.txt`.

**Step 3: Verify the database**

Run: `wc -l /home/ifit/dotfiles/.local/share/emoji-picker/emojis.txt`
Expected: 3624 lines (same count as source)

Run: `grep -P '\tsmoke' /home/ifit/dotfiles/.local/share/emoji-picker/emojis.txt`
Expected: should show 😤 face with steam from nose

**Step 4: Commit**

```
git add .local/share/emoji-picker/emojis.txt
git commit -m "feat: add alias-enriched emoji database (3624 emojis)"
```

---

### Task 3: Update hyprland config

**Files:**
- Modify: `.config/hypr/hyprland.conf:4-5` (remove bemoji env vars)
- Modify: `.config/hypr/hyprland.conf:266` (change keybinding)

**Step 1: Remove bemoji env vars**

Delete these two lines from `.config/hypr/hyprland.conf`:
```
env = BEMOJI_PICKER_CMD,fuzzel --dmenu
env = BEMOJI_ECHO_NEWLINE,false
```

**Step 2: Update keybinding**

Change line 266 from:
```
bind = $mainMod SHIFT, space, exec, bemoji
```
To:
```
bind = $mainMod SHIFT, space, exec, foot --app-id=fzf-picker -e emoji-picker
```

**Step 3: Commit**

```
git add .config/hypr/hyprland.conf
git commit -m "feat: switch emoji picker from bemoji to foot+fzf"
```

---

### Task 4: Stow and test

**Step 1: Stow new files**

Run: `cd /home/ifit/dotfiles && stow .`

This symlinks the new `emoji-picker` script and database into place.

**Step 2: Manual test**

- Press Super+Shift+Space — should open fzf-picker window
- Type "smoke" — should show 😤 face with steam from nose (not see-no-evil monkey)
- Select it — should copy to clipboard and paste into focused window
- Open again — 😤 should appear at top (history)
