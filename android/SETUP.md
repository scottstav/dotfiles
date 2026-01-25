# Android Denote Setup

Access denote files on Android with Dropbox sync.

## What You Get

- Create new journal entries in denote format
- Quick capture notes that convert to denote files
- Edit with Markor (native Android editor with org-mode highlighting)
- Sync to Dropbox with one tap
- Two-way sync with desktop Emacs
- Search across all denote files

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

### 4. Create Denote Directory

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
~/.local/bin/denote-sync
```

You should get a notification "Denote synced".

### 8. Configure Markor

1. Open Markor
2. Tap hamburger menu (☰) → **Settings** → **General**
3. Tap **Notebook**
4. Navigate to: `Dropbox/org/denote`
5. Select that folder

### 9. Set Up Quick Capture (Optional)

Configure Markor's QuickNote feature to append to `quicknote.org`:

1. In Markor, tap hamburger menu (☰) → **Settings** → **General**
2. Find **QuickNote** settings
3. Set the file to: `Dropbox/org/denote/quicknote.org`

Now you can use Markor's share target or QuickNote to quickly append text, then tap **Shred Capture** to convert it to a proper denote file.

### 10. Add Home Screen Widgets

1. Long-press on home screen
2. Tap **Widgets**
3. Find **Termux:Widget**
4. Drag to home screen
5. You'll see: Journal, Sync Denote, Shred Capture

### 11. Battery Optimization

To prevent Android from killing Termux:

1. Settings → Apps → Termux → Battery → **Unrestricted**

## Usage

| Action | How |
|--------|-----|
| Create today's journal | Tap **Journal** widget |
| Sync all denote files | Tap **Sync Denote** widget |
| Shred quick captures | Tap **Shred Capture** widget |
| Search denote files | Use Markor's built-in search |

## Workflows

### Daily Journal

1. Tap **Journal** widget → creates today's journal if it doesn't exist
2. Open in Markor and write
3. Tap **Sync Denote** widget

### Quick Capture

1. Throughout the day, append notes to `quicknote.org` via Markor's QuickNote
2. When ready, tap **Shred Capture** widget
3. Creates a timestamped denote file tagged `:inbox:`
4. Clears `quicknote.org` for fresh input
5. Find captures in Emacs by searching for `_inbox` or `:inbox:` tag

### Desktop Sync

1. Edit in Emacs as normal
2. Dropbox syncs automatically
3. On phone, tap **Sync Denote** to pull changes

## Troubleshooting

### "Denote synced" notification doesn't appear

Check Termux notification permission in Android settings.

### rclone errors

```bash
# Test connection
rclone lsd Dropbox:

# Check config
rclone config show
```

### Files not appearing in Markor

Make sure Markor's notebook directory is set to `Dropbox/org/denote`.

### Scripts not found

Verify they're installed:

```bash
ls -la ~/.local/bin/denote-* ~/.local/bin/capture-shred
ls -la ~/.shortcuts/
```

## Files Reference

| File | Purpose |
|------|---------|
| `scripts/denote-sync` | Bidirectional rclone sync for all denote files |
| `scripts/denote-journal` | Create today's journal entry if it doesn't exist |
| `scripts/capture-shred` | Convert quicknote.org to denote file with inbox tag |
| `shortcuts/Journal` | Widget: runs denote-journal |
| `shortcuts/Sync Denote` | Widget: runs denote-sync |
| `shortcuts/Shred Capture` | Widget: runs capture-shred |
