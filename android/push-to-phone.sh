#!/bin/bash
# Push android scripts to phone via ADB
# Run from dotfiles/android directory

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DEST="/storage/emulated/0/termux-scripts"

echo "Checking ADB connection..."
adb devices | grep -q "device$" || { echo "No device connected"; exit 1; }

echo "Creating destination directory..."
adb shell "mkdir -p $DEST"

echo "Pushing scripts..."
adb push "$SCRIPT_DIR/scripts/"* "$DEST/"

echo "Pushing shortcuts..."
adb push "$SCRIPT_DIR/shortcuts/"* "$DEST/"

echo "Pushing install script..."
adb push "$SCRIPT_DIR/install.sh" "$DEST/"

echo ""
echo "Done! Now in Termux run:"
echo "  bash ~/storage/shared/termux-scripts/install.sh"
