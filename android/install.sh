#!/data/data/com.termux/files/usr/bin/bash
# Install journal scripts and shortcuts in Termux

set -e

SCRIPTS_SRC="$HOME/storage/shared/termux-scripts"

echo "Creating directories..."
mkdir -p ~/.local/bin ~/.shortcuts ~/.termux/tasker

echo "Installing scripts..."
cp "$SCRIPTS_SRC"/journal-* ~/.local/bin/
chmod +x ~/.local/bin/journal-*

echo "Installing Tasker scripts..."
cp "$SCRIPTS_SRC/journal-sync" ~/.termux/tasker/
chmod +x ~/.termux/tasker/journal-sync

echo "Installing shortcuts..."
cp "$SCRIPTS_SRC/Journal" ~/.shortcuts/
cp "$SCRIPTS_SRC/Sync Journal" ~/.shortcuts/
cp "$SCRIPTS_SRC/Search Journal" ~/.shortcuts/
chmod +x ~/.shortcuts/*

echo "Done!"
echo ""
echo "Next steps:"
echo "  1. Run: ~/.local/bin/journal-sync"
echo "  2. Configure Markor notebook directory to: Dropbox/org/denote/journal"
echo "  3. Add Termux:Widget to home screen"
