# Android Denote Setup

Sync denote files on Android via Dropbox using Markor and Termux.

## Required Apps (from F-Droid)

Install from F-Droid (not Play Store - signing keys differ):

- **Termux** - Terminal emulator
- **Termux:API** - For notifications
- **Termux:Widget** - Home screen shortcuts
- **Markor** - Text editor with org-mode support

## Setup

### 1. Termux Setup

```bash
pkg update && pkg install rclone ripgrep fzf termux-api
termux-setup-storage  # grant permission when prompted
```

### 2. Configure rclone for Dropbox

```bash
rclone config
```

Create new remote named `Dropbox` (capital D), type `dropbox`. Accept defaults and authorize in browser.

### 3. Push Scripts to Phone

From desktop with phone connected via USB (ADB enabled):

```bash
./android/push-to-phone.sh
```

### 4. Install in Termux

```bash
bash ~/storage/shared/termux-scripts/install.sh
```

### 5. Configure Markor

Set notebook directory to: `Dropbox/org/denote`

### 6. Add Termux:Widget to Home Screen

Long-press home → Widgets → Termux:Widget

## Usage

| Widget | Action |
|--------|--------|
| **Journal** | Create/open today's journal |
| **Sync Denote** | Sync with Dropbox |
| **Shred Capture** | Convert quicknote.org to denote file |

## Troubleshooting

- **No sync notification**: Check Termux notification permissions
- **rclone errors**: Run `rclone lsd Dropbox:` to test connection
- **Scripts missing**: Check `ls ~/.local/bin/` and `ls ~/.shortcuts/`
