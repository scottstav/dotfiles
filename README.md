```
                            ,
                       /(  /:./\
                    ,_/:`-/::/::/\_ ,
                    |:|::/::/::/::;'( ,,
                    |:|:/::/::/:;'::,':(.( ,
                _,-'"HHHHH"""HHoo--.::::,-:(,----..._
            ,-"HHHb  "HHHHb  HHHHb   HHHoo-.::::::::::-.
          ,'   "HHHb  "HHHHb "HHHH   HHHHH  Hoo::::::::::.              _,.-::`.
        ,'      "HH`.  "HHHH  HHHHb  "HHHH  "HHHb`-:::::::.        _.-:::::::;'
       / ,-.        :   HHHH  "HHHH   HHHH   HHHH  Hoo::::;    _,-:::::::::;'
     ,'  `-'        :   HHHH   HHHH   "HHH   "HHH  "HHHH-:._,o:::::::::::;'
    /               :   HHHH __HHHP...........HHH   HHHF   HHH:::::::;:;'
   /_               ; _,-::::::.:::::::::::::''HH   HHH    "HH::::::::(
   (_"-.,          /  : :.::.:.::::::::::,d   HHH   "HH     HH::::::::::.
    (,-'          /    :.::.:::.::::::;'HHH   "HH    HH,::"-.H::::::::::::.
     ".         ,'    : :.:::.::::::;'  "HH    HH   _H-:::)   `-::::::::::::.
       `-.___,-'       `-.:::::,--'"     "H    HH,-::::::/        "--.::::::::.
            """---..__________________,-------'::::::::;/               """---'
                        \::.:---.          `::::::::::;'
                         \::::::|            `::;-'""
                          `.::::|
                            `.::| Sheepshead (Archosargus probatocephalus) (-Krogg)
```

## Usage

```bash
# Stow home directory configs
stow .

# Stow system configs (requires sudo)
sudo stow --target=/ root
```

## Voice Typing Setup

Voice typing uses a modified [voice-typing-linux](https://github.com/GitJuhb/voice-typing-linux), included at `.local/share/voice-typing-linux`.

**Modifications from upstream:** Uses faster-whisper instead of openai-whisper for ~4x speedup. Added pre-recording buffer (600ms) so initial words aren't cut off. Defaults to small model for better accuracy.

```bash
# Install system dependencies
sudo pacman -S ydotool portaudio

# Enable ydotool for Wayland keyboard input
systemctl --user enable --now ydotool

# Add user to input group (then log out/in)
sudo usermod -aG input $USER

# Create Python venv (requires Python 3.11 for onnxruntime)
cd ~/.local/share/voice-typing-linux
python3.11 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
pip install requests
```

## Streaming Voice Typing Setup (Vosk)

A second voice typing mode using Vosk for real-time streaming transcription. Words are typed progressively as you speak (like Android), unlike Whisper which waits until you stop talking.

**How it works:** Vosk produces partial results (shown in a notification overlay for visual feedback) and final results (typed via ydotool). Only finals are typed — no backspacing or rewriting, so switching windows mid-dictation is safe.

**Keybindings:**
- `Super+Shift+N` — toggle streaming voice typing (Vosk)
- `Super+Shift+S` — toggle batch voice typing (Whisper, unchanged)

**First-time setup:**
```bash
# Run the setup script (creates venv + downloads ~1.8 GB Vosk model)
bash ~/.local/share/streaming-voice-typing/setup.sh

# Or manually:
cd ~/.local/share/streaming-voice-typing
python3 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt

# Download the large English model
mkdir -p ~/.local/share/vosk-models
cd ~/.local/share/vosk-models
wget https://alphacephei.com/vosk/models/vosk-model-en-us-0.22.zip
unzip vosk-model-en-us-0.22.zip
```

Both daemons auto-start at login (paused). Toggle whichever you want with its keybinding.
