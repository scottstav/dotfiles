#!/data/data/com.termux/files/usr/bin/bash
# Install denote scripts and shortcuts in Termux

set -e

SCRIPTS_SRC="$HOME/storage/shared/termux-scripts"

echo "Creating directories..."
mkdir -p ~/.local/bin ~/.shortcuts ~/.termux/tasker

echo "Installing scripts..."
cp "$SCRIPTS_SRC"/denote-sync ~/.local/bin/
cp "$SCRIPTS_SRC"/denote-journal ~/.local/bin/
cp "$SCRIPTS_SRC"/capture-shred ~/.local/bin/
chmod +x ~/.local/bin/denote-sync ~/.local/bin/denote-journal ~/.local/bin/capture-shred

echo "Installing Tasker scripts..."
cp "$SCRIPTS_SRC/denote-sync" ~/.termux/tasker/
chmod +x ~/.termux/tasker/denote-sync

echo "Installing shortcuts..."
cp "$SCRIPTS_SRC/Journal" ~/.shortcuts/
cp "$SCRIPTS_SRC/Sync Denote" ~/.shortcuts/
cp "$SCRIPTS_SRC/Shred Capture" ~/.shortcuts/
chmod +x ~/.shortcuts/*

echo "Done!"
echo ""
echo "Next steps:"
echo "  1. Run: ~/.local/bin/denote-sync"
echo "  2. Configure Markor notebook directory to: Dropbox/org/denote"
echo "  3. Add Termux:Widget to home screen"
