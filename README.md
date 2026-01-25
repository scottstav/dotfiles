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

Voice typing uses [voice-typing-linux](https://github.com/GitJuhb/voice-typing-linux) with faster-whisper.

```bash
# Clone the repo
cd ~/repos
git clone https://github.com/GitJuhb/voice-typing-linux
cd voice-typing-linux

# Create venv with Python 3.11 (required for onnxruntime compatibility)
python3.11 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
pip install requests  # missing from requirements.txt

# Enable ydotool for Wayland keyboard input
sudo pacman -S ydotool
systemctl --user enable --now ydotool

# Add user to input group (then log out/in)
sudo usermod -aG input $USER
```

Keybindings: `Super+Shift+S` or `XF86VoiceCommand`
