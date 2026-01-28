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

## Quick Setup

```bash
git clone <repo-url> ~/dotfiles
cd ~/dotfiles
./setup.sh
```

This installs packages, stows configs, sets up KDE Wallet credentials, enables systemd
services, and configures voice typing. It's idempotent — safe to re-run.

## Manual Usage

```bash
# Stow home directory configs
stow .

# Stow system configs (requires sudo)
sudo stow --target=/ root
```

## KDE Wallet Setup (Bitwarden + Emacs Daemon)

The `start-emacs-daemon` script reads Bitwarden credentials from KDE Wallet to unlock
the vault and pass a session token to Emacs. You need to store three entries in KWallet
under a folder called `Bitwarden` in the `kdewallet` wallet.

```bash
# Open KDE Wallet Manager and create a folder called "Bitwarden" in kdewallet,
# then add these three entries (Map type → add entries, or use the CLI):
#   BW_CLIENTID      — your Bitwarden API client_id
#   BW_CLIENTSECRET  — your Bitwarden API client_secret
#   BW_PASSWORD      — your Bitwarden master password

# To get your API credentials, log into vault.bitwarden.com → Settings → Security → Keys → API Key.

# You can verify the entries are readable:
kwallet-query -l -f Bitwarden kdewallet
kwallet-query -r BW_CLIENTID -f Bitwarden kdewallet
```

The `kwallet-pam` package auto-unlocks KWallet on login when your KWallet password matches
your user login password. If setting up a new user, set the KWallet password to match.

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

