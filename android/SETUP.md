# Android Denote Journal Setup

Access denote-journal files on Android with Dropbox sync.

## What You Get

- Create new journal entries in denote format
- Edit with Markor (native Android editor with org-mode highlighting)
- Sync to Dropbox with one tap
- Two-way sync with desktop Emacs
- Search across all journal files

## Architecture

```
[Markor edit] → [Save] → [Tap Sync widget] → [rclone to Dropbox]
                                                    ↓
                                          [Desktop Emacs auto-reverts]
```

## Required Apps (Install from F-Droid)

All apps **must** be from F-Droid (Play Store versions have different signing keys and won't work together):

1. **Termux** - Terminal emulator
2. **Termux:API** - For notifications
3. **Termux:Widget** - Home screen shortcuts
4. **Markor** - Text editor with org-mode support

## Setup Steps

### 1. Termux Package Installation

Open Termux and run:

```bash
pkg update && pkg install rclone ripgrep fzf termux-api
```

### 2. Storage Access

```bash
termux-setup-storage
```

Grant permission when prompted. Verify with:

```bash
ls ~/storage/shared
```

### 3. Configure rclone for Dropbox

```bash
rclone config
```

Follow the prompts:
- `n` for new remote
- Name: `Dropbox` (capital D)
- Type: `dropbox`
- client_id: **leave blank** (press Enter)
- client_secret: **leave blank** (press Enter)
- Edit advanced config: `n`
- Use web browser: `y`
- Authorize in browser when it opens
- Confirm with `y`
- `q` to quit config

Verify connection:

```bash
rclone lsd Dropbox:
```

### 4. Create Journal Directory

```bash
mkdir -p ~/storage/shared/Dropbox/org/denote/journal
```

### 5. Push Scripts from Desktop

Connect phone via USB with ADB debugging enabled.

From your **desktop** (in dotfiles repo):

```bash
# Create temp directory on phone
adb shell "mkdir -p /storage/emulated/0/termux-scripts"

# Push all scripts
adb push android/scripts/* /storage/emulated/0/termux-scripts/
adb push android/shortcuts/* /storage/emulated/0/termux-scripts/

# Push setup script
adb push android/install.sh /storage/emulated/0/termux-scripts/
```

### 6. Install Scripts in Termux

In Termux:

```bash
bash ~/storage/shared/termux-scripts/install.sh
```

### 7. Initial Sync

```bash
~/.local/bin/journal-sync
```

You should get a notification "Journal synced".

### 8. Configure Markor

1. Open Markor
2. Tap hamburger menu (☰) → **Settings** → **General**
3. Tap **Notebook**
4. Navigate to: `Dropbox/org/denote/journal`
5. Select that folder

### 9. Add Home Screen Widgets

1. Long-press on home screen
2. Tap **Widgets**
3. Find **Termux:Widget**
4. Drag to home screen
5. You'll see: Journal, Sync Journal, Search Journal

### 10. Battery Optimization

To prevent Android from killing Termux:

1. Settings → Apps → Termux → Battery → **Unrestricted**

## Usage

| Action | How |
|--------|-----|
| New/open today's journal | Tap **Journal** widget |
| Sync after editing | Tap **Sync Journal** widget |
| Search journals | Tap **Search Journal** widget (or `journal-search "keyword"` in Termux) |

## Workflow

**On phone:**
1. Tap Journal widget → Markor opens with today's entry
2. Write your entry
3. Save (back button or Markor's save)
4. Tap Sync Journal widget

**On desktop:**
1. Edit in Emacs as normal
2. Dropbox syncs automatically
3. On phone, tap Sync Journal to pull changes

## Troubleshooting

### "Journal synced" notification doesn't appear

Check Termux notification permission in Android settings.

### rclone errors

```bash
# Test connection
rclone lsd Dropbox:

# Check config
rclone config show
```

### Files not appearing in Markor

Make sure Markor's notebook directory is set to `Dropbox/org/denote/journal`.

### Scripts not found

Verify they're installed:

```bash
ls -la ~/.local/bin/journal-*
ls -la ~/.shortcuts/
```

## Files Reference

| File | Purpose |
|------|---------|
| `scripts/journal-sync` | Bidirectional rclone sync with notification |
| `scripts/journal-new` | Create new denote-formatted journal, open Markor |
| `scripts/journal-today` | Sync then open/create today's journal |
| `scripts/journal-search` | Search with ripgrep + fzf |
| `shortcuts/Journal` | Widget: runs journal-today |
| `shortcuts/Sync Journal` | Widget: runs journal-sync |
| `shortcuts/Search Journal` | Widget: runs journal-search |
