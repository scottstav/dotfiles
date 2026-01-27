#!/bin/bash
# One-time setup for streaming voice typing
# Run this once to create the venv and download the Vosk model.

set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
VENV="$SCRIPT_DIR/.venv"
MODEL_DIR="$HOME/.local/share/vosk-models"
MODEL_NAME="vosk-model-en-us-0.22"
MODEL_URL="https://alphacephei.com/vosk/models/${MODEL_NAME}.zip"

echo "=== Streaming Voice Typing Setup ==="

# 1. Create venv
if [ ! -d "$VENV" ]; then
    echo "Creating Python venv..."
    python3 -m venv "$VENV"
fi
source "$VENV/bin/activate"

echo "Installing Python dependencies..."
pip install -q -r "$SCRIPT_DIR/requirements.txt"

# 2. Download Vosk model
if [ ! -d "$MODEL_DIR/$MODEL_NAME" ]; then
    echo "Downloading Vosk model ($MODEL_NAME, ~1.8 GB)..."
    mkdir -p "$MODEL_DIR"
    cd "$MODEL_DIR"
    wget -q --show-progress "$MODEL_URL"
    echo "Extracting..."
    unzip -q "${MODEL_NAME}.zip"
    rm "${MODEL_NAME}.zip"
    echo "Model installed at $MODEL_DIR/$MODEL_NAME"
else
    echo "Vosk model already exists at $MODEL_DIR/$MODEL_NAME"
fi

echo ""
echo "=== Setup complete ==="
echo "Start the daemon:  voice-typing-stream-daemon"
echo "Toggle with:       voice-typing-stream"
echo "Or use:            Super+Shift+N"
