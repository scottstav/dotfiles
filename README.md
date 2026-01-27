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
pip install requests vosk

# Download Vosk model for real-time preview (~1.8 GB)
mkdir -p ~/.local/share/vosk-models
cd ~/.local/share/vosk-models
wget https://alphacephei.com/vosk/models/vosk-model-en-us-0.22.zip
unzip vosk-model-en-us-0.22.zip
```

