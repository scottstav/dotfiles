# Emoji Picker: foot + fzf with Aliases

## Overview

Replace bemoji + fuzzel with a custom emoji picker script following the existing foot + fzf pattern. The current setup uses fuzzel's overly aggressive fuzzy matching, which surfaces absurd results (e.g. "smoke" matching "see-no-evil-monkey"). The new picker uses fzf exact matching on an alias-enriched emoji database.

## Architecture

Single script + data file:

1. **`~/.local/bin/emoji-picker`** (Bash) — the picker script
2. **`~/.local/share/emoji-picker/emojis.txt`** — enriched emoji database with aliases
3. **`~/.local/state/emoji-history.txt`** — recently used emojis

### Flow

```
Keybind (Super+Shift+Space)
  → foot --app-id=fzf-picker -e emoji-picker
  → script builds list: history (deduped) + full database
  → fzf --exact --layout=reverse
  → user searches and selects
  → emoji copied to clipboard (wl-copy)
  → auto-paste into previously focused window (wtype)
  → foot closes
```

## Emoji Database Format

Each line: `<emoji> <official name> | <aliases>`

```
😤 face with steam from nose | smoke triumph badass huff snort angry frustrated
🙈 see-no-evil monkey | hide peek shy oops
🔥 fire | flame hot lit
💀 skull | dead death rip oof
```

The pipe separates the display portion from the alias portion. fzf searches the full line but only displays everything before the pipe using `--delimiter='|' --with-nth=1`.

## Alias Enrichment

Generate the enriched database by:
1. Starting from the existing bemoji `emojis.txt` (3624 emojis, format: `<emoji> <name>`)
2. Using Claude to generate common aliases/slang for each emoji in batch
3. Storing the result as the new database file

This is a one-time generation step. The resulting file is committed to the repo.

## Search Behavior

fzf flags:
- `--exact` — substring matching only, no fuzzy character skipping
- `--layout=reverse` — prompt at top
- `--no-sort` — preserve history-first ordering
- `--height=100%` — fill the foot window
- `--delimiter='|' --with-nth=1` — search full line but display only emoji + name
- `--prompt='emoji> '`

## History

- On selection, prepend the chosen emoji line to `~/.local/state/emoji-history.txt`
- When building the list, read history file, deduplicate, then append remaining emojis from the full database
- History file is just the raw lines from emojis.txt (with aliases), most recent first

## Output

1. Extract just the emoji character from the selected line
2. `wl-copy` — put on clipboard
3. `wtype -M ctrl v -m ctrl` — simulate Ctrl+V paste into the previously focused window

## Hyprland Integration

- Keybinding: `bind = $mainMod SHIFT, space, exec, foot --app-id=fzf-picker -e emoji-picker`
- Reuses existing `fzf-picker` window rules (float, centered, 40%x50%)
- Remove the `BEMOJI_PICKER_CMD` and `BEMOJI_ECHO_NEWLINE` env vars

## Dependencies

All already installed: `foot`, `fzf`, `wl-copy`, `wtype`
