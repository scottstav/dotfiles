#!/bin/bash
set -euo pipefail

# Dotfiles Linux desktop setup
# Automates everything needed on a fresh user or fresh system.
# Idempotent — safe to re-run.

DOTFILES="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BOLD='\033[1m'
DIM='\033[2m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
NC='\033[0m'

step()  { echo -e "\n${BOLD}>>> $1${NC}"; }
ok()    { echo -e "  ${GREEN}✓${NC} $1"; }
skip()  { echo -e "  ${DIM}– $1 (skipped)${NC}"; }
warn()  { echo -e "  ${YELLOW}! $1${NC}"; }
fail()  { echo -e "  ${RED}✗ $1${NC}"; }

# ------------------------------------------------------------------
# 1. Stow dotfiles
# ------------------------------------------------------------------
step "Stow dotfiles"

cd "$DOTFILES"

# Both stow calls use the same pattern: try normally first, fall back to
# --adopt if another user (or manual install) already owns the targets.
# After adopting, git checkout restores our repo versions as the source of truth.

if stow --no-folding . 2>/dev/null; then
    ok "Home directory configs stowed"
else
    warn "Conflicts detected in home dir — adopting existing files"
    stow --no-folding --adopt .
    git -C "$DOTFILES" checkout -- .
    ok "Home configs adopted and restored from repo"
fi

if [ -d "$DOTFILES/root" ]; then
    if sudo stow --target=/ root 2>/dev/null; then
        ok "System configs stowed to /etc"
    else
        # Conflicts are usually stale symlinks from another user's dotfiles repo.
        # Remove them so we can stow our own.
        warn "Conflicts detected in /etc — clearing stale targets"
        while IFS= read -r line; do
            target=$(echo "$line" | sed -n 's/.*existing target is not owned by stow: //p')
            if [ -n "$target" ] && [ -L "/$target" ]; then
                sudo rm "/$target"
                ok "Removed stale symlink /$target"
            fi
        done < <(sudo stow --target=/ root 2>&1)
        sudo stow --target=/ root
        ok "System configs stowed to /etc"
    fi
fi

# ------------------------------------------------------------------
# 2. User groups
# ------------------------------------------------------------------
step "User groups"

if ! groups | grep -qw input; then
    sudo usermod -aG input "$USER"
    warn "Added $USER to input group — log out and back in for this to take effect"
else
    ok "Already in input group"
fi

# Shared dropbox group for cross-user Dropbox access
if ! getent group dropbox &>/dev/null; then
    sudo groupadd dropbox
    ok "Created dropbox group"
else
    ok "dropbox group exists"
fi

if ! id -nG "$USER" | grep -qw dropbox; then
    sudo usermod -aG dropbox "$USER"
    warn "Added $USER to dropbox group — log out and back in for this to take effect"
else
    ok "Already in dropbox group"
fi

# ------------------------------------------------------------------
# 3. Shared Dropbox directory (/shared/)
# ------------------------------------------------------------------
step "Shared Dropbox directory"

# /shared/ holds Dropbox data and binary shared between users.
# - .dropbox-dist (binary): symlinked from each user's home
# - Dropbox (sync data): bind-mounted into each user's ~/Dropbox
#   (Dropbox rejects symlinks at account linking time)
# - .dropbox (runtime metadata): local per-user, NOT shared
# ACLs grant the dropbox group read/write across all shared files.

if [ ! -d /shared ]; then
    sudo mkdir -p /shared
    sudo chown "$USER:dropbox" /shared
    sudo chmod 2775 /shared
    ok "Created /shared/ (owner=$USER:dropbox, mode=2775)"
else
    ok "/shared/ already exists"
fi

# .dropbox-dist (binary) — symlink to shared copy
if [ -L "$HOME/.dropbox-dist" ]; then
    ok ".dropbox-dist → /shared/.dropbox-dist (symlink exists)"
elif [ -e "$HOME/.dropbox-dist" ]; then
    warn "$HOME/.dropbox-dist exists as a real file/directory — migrate it to /shared/.dropbox-dist manually, then re-run"
else
    mkdir -p /shared/.dropbox-dist
    ln -s /shared/.dropbox-dist "$HOME/.dropbox-dist"
    ok ".dropbox-dist → /shared/.dropbox-dist (created)"
fi

# .dropbox (runtime metadata) — each user needs their own.
# Contains per-instance state (PID files, sockets, account link)
# that is user-specific and can't be shared.
if [ -L "$HOME/.dropbox" ] && [ "$(readlink "$HOME/.dropbox")" = "/shared/.dropbox" ]; then
    rm "$HOME/.dropbox"
    ok "Removed stale .dropbox → /shared/.dropbox symlink"
fi
if [ ! -d "$HOME/.dropbox" ]; then
    mkdir -p "$HOME/.dropbox"
    ok "Created local ~/.dropbox directory"
else
    ok "~/.dropbox already exists (local)"
fi

# Dropbox sync folder — bind mount from /shared/Dropbox.
# Dropbox rejects symlinks ("filesystem not supported") but accepts
# bind mounts, which look like a real ext4 directory.
FSTAB_ENTRY="/shared/Dropbox $HOME/Dropbox none bind 0 0"
if [ -L "$HOME/Dropbox" ]; then
    rm "$HOME/Dropbox"
    ok "Removed stale Dropbox symlink"
fi
mkdir -p /shared/Dropbox "$HOME/Dropbox"
if mountpoint -q "$HOME/Dropbox" 2>/dev/null; then
    ok "~/Dropbox already bind-mounted"
else
    if ! grep -qF "$HOME/Dropbox" /etc/fstab 2>/dev/null; then
        echo "$FSTAB_ENTRY" | sudo tee -a /etc/fstab >/dev/null
        sudo systemctl daemon-reload
        ok "Added bind mount to /etc/fstab"
    fi
    sudo mount "$HOME/Dropbox"
    ok "~/Dropbox bind-mounted from /shared/Dropbox"
fi

# Default ACLs so both users get group read/write on all new files.
# Requires sudo because another user may own some files.
if command -v setfacl &>/dev/null; then
    for target in Dropbox .dropbox-dist; do
        if [ -d "/shared/$target" ]; then
            sudo setfacl -R -m g:dropbox:rwX "/shared/$target" 2>/dev/null
            sudo setfacl -R -d -m g:dropbox:rwx "/shared/$target" 2>/dev/null
        fi
    done
    ok "ACLs set on /shared/ directories"
else
    warn "setfacl not found — install acl package for cross-user permissions"
fi

# Setgid bit on all subdirectories
sudo find /shared -type d -exec chmod g+s {} + 2>/dev/null
ok "Setgid applied to /shared/ subdirectories"

# ------------------------------------------------------------------
# 4. KDE Wallet — Bitwarden credentials
# ------------------------------------------------------------------
step "KDE Wallet (Bitwarden credentials)"

if command -v kwallet-query &>/dev/null; then
    # Check if kwallet is accessible
    if kwallet-query -l -f Bitwarden kdewallet &>/dev/null; then
        ok "KWallet Bitwarden folder already exists and is accessible"
    else
        echo "  The start-emacs-daemon script needs three entries in KDE Wallet."
        echo "  Wallet: kdewallet | Folder: Bitwarden"
        echo ""
        echo "  To get your API credentials:"
        echo "    vault.bitwarden.com → Settings → Security → Keys → API Key"
        echo ""

        read -rp "  Set up KWallet credentials now? (y/n) " answer
        if [[ "$answer" =~ ^[Yy]$ ]]; then
            # kwalletd6 may not be running yet outside a desktop session
            if ! pgrep -x kwalletd6 &>/dev/null; then
                warn "kwalletd6 is not running — start a desktop session first, then re-run this step"
            else
                read -rp "  BW_CLIENTID: " bw_clientid
                read -rsp "  BW_CLIENTSECRET: " bw_clientsecret; echo
                read -rsp "  BW_PASSWORD: " bw_password; echo

                # Write entries to kwallet using dbus (kwallet-query is read-only)
                # We use qdbus to write; kwalletmanager can also be used manually
                WALLET_HANDLE=$(qdbus org.kde.kwalletd6 /modules/kwalletd6 open kdewallet 0 setup-dotfiles)
                if [ -n "$WALLET_HANDLE" ]; then
                    qdbus org.kde.kwalletd6 /modules/kwalletd6 writePassword "$WALLET_HANDLE" Bitwarden BW_CLIENTID "$bw_clientid" setup-dotfiles
                    qdbus org.kde.kwalletd6 /modules/kwalletd6 writePassword "$WALLET_HANDLE" Bitwarden BW_CLIENTSECRET "$bw_clientsecret" setup-dotfiles
                    qdbus org.kde.kwalletd6 /modules/kwalletd6 writePassword "$WALLET_HANDLE" Bitwarden BW_PASSWORD "$bw_password" setup-dotfiles
                    qdbus org.kde.kwalletd6 /modules/kwalletd6 close "$WALLET_HANDLE" false setup-dotfiles
                    ok "Credentials written to KWallet"
                else
                    fail "Could not open kwallet — try using KDE Wallet Manager (kwalletmanager) manually"
                fi

                # Clear sensitive vars
                unset bw_clientid bw_clientsecret bw_password
            fi
        else
            warn "Skipped — set up manually via KDE Wallet Manager (kwalletmanager)"
            echo "  Create folder 'Bitwarden' in 'kdewallet' with entries:"
            echo "    BW_CLIENTID, BW_CLIENTSECRET, BW_PASSWORD"
        fi
    fi
else
    warn "kwallet-query not found — install kwalletmanager package"
fi

# ------------------------------------------------------------------
# 5. Bitwarden session (for fetching secrets during setup)
# ------------------------------------------------------------------
step "Bitwarden session"

if command -v bw &>/dev/null && command -v kwallet-query &>/dev/null; then
    if kwallet-query -l -f Bitwarden kdewallet &>/dev/null; then
        export BW_CLIENTID=$(kwallet-query -r BW_CLIENTID -f Bitwarden kdewallet)
        export BW_CLIENTSECRET=$(kwallet-query -r BW_CLIENTSECRET -f Bitwarden kdewallet)
        BW_PASSWORD=$(kwallet-query -r BW_PASSWORD -f Bitwarden kdewallet)

        if [ -n "$BW_CLIENTID" ] && [ -n "$BW_CLIENTSECRET" ] && [ -n "$BW_PASSWORD" ]; then
            bw logout &>/dev/null
            bw login --apikey &>/dev/null
            export BW_PASSWORD
            output=$(bw unlock --passwordenv BW_PASSWORD 2>&1)
            unset BW_PASSWORD
            export BW_SESSION=$(echo "$output" | grep -oP 'export\s+\w+="[^"]*"' | cut -d'"' -f2)

            if [ -n "$BW_SESSION" ]; then
                ok "Bitwarden session active"
            else
                warn "Could not unlock Bitwarden vault"
            fi
        else
            warn "Missing KWallet credentials — skipping Bitwarden login"
        fi
    else
        warn "KWallet Bitwarden folder not found — skipping"
    fi
else
    warn "bw or kwallet-query not found — skipping Bitwarden login"
fi

# ------------------------------------------------------------------
# 6. Systemd user services
# ------------------------------------------------------------------
step "Systemd user services"

systemctl --user daemon-reload

TIMERS=(gcal-refresh.timer system-freshness.timer mbsync.timer)
for timer in "${TIMERS[@]}"; do
    if systemctl --user is-enabled "$timer" &>/dev/null; then
        ok "$timer already enabled"
    else
        systemctl --user enable --now "$timer"
        ok "$timer enabled"
    fi
done

# ydotool (system service, not user)
if systemctl is-enabled ydotool.service &>/dev/null 2>&1; then
    ok "ydotool.service already enabled"
else
    if systemctl cat ydotool.service &>/dev/null 2>&1; then
        sudo systemctl enable --now ydotool.service
        ok "ydotool.service enabled"
    else
        # Some setups use a user service instead
        if systemctl --user cat ydotool.service &>/dev/null 2>&1; then
            systemctl --user enable --now ydotool.service
            ok "ydotool.service (user) enabled"
        else
            warn "ydotool.service not found — may need manual setup"
        fi
    fi
fi

# Dropbox — enable the service. The binary lives at /shared/.dropbox-dist
# (symlinked from ~/.dropbox-dist by section 3).
if [ -f "$HOME/.dropbox-dist/dropboxd" ]; then
    systemctl --user enable dropbox.service
    ok "Dropbox service enabled"
else
    systemctl --user enable dropbox.service 2>/dev/null
    warn "Dropbox binary not found — install from dropbox.com, then: systemctl --user start dropbox.service"
fi

# ------------------------------------------------------------------
# 7. Google Calendar (gcal-refresh)
# ------------------------------------------------------------------
step "Google Calendar setup"

GCAL_CONFIG="$HOME/.config/gcal"
GCAL_CLIENT_SECRET="$GCAL_CONFIG/client_secret.json"
GCAL_TOKENS="$GCAL_CONFIG/tokens"
GCAL_BW_ITEM="2dd796a5-0786-42bf-b33a-b3fc0075b3c6"

# Ensure Python deps are installed
for pkg in python-google-auth python-google-api-python-client python-google-auth-oauthlib; do
    if ! pacman -Q "$pkg" &>/dev/null; then
        yay -S --needed --noconfirm "$pkg"
        ok "$pkg installed"
    fi
done
ok "Python dependencies present"

# Fetch client_secret.json from Bitwarden
if [ -s "$GCAL_CLIENT_SECRET" ]; then
    ok "client_secret.json already exists"
elif [ -n "$BW_SESSION" ]; then
    mkdir -p "$GCAL_CONFIG"
    bw get notes "$GCAL_BW_ITEM" > "$GCAL_CLIENT_SECRET"
    chmod 600 "$GCAL_CLIENT_SECRET"
    if [ -s "$GCAL_CLIENT_SECRET" ]; then
        ok "client_secret.json fetched from Bitwarden"
    else
        rm -f "$GCAL_CLIENT_SECRET"
        fail "Failed to fetch client_secret.json from Bitwarden"
    fi
else
    warn "Bitwarden not available — cannot fetch client_secret.json"
    echo "  Manual fix: bw get notes $GCAL_BW_ITEM > $GCAL_CLIENT_SECRET"
fi

# Run OAuth flow if no tokens exist
if [ -s "$GCAL_CLIENT_SECRET" ]; then
    mkdir -p "$GCAL_TOKENS"
    if ls "$GCAL_TOKENS"/*.json &>/dev/null; then
        ok "OAuth tokens already configured"
    else
        echo "  No Google Calendar accounts configured."
        echo "  This will open a browser to authorize your Google account."
        read -rp "  Run OAuth flow now? (y/n) " answer
        if [[ "$answer" =~ ^[Yy]$ ]]; then
            read -rp "  Account name (e.g. personal, work): " acct_name
            gcal-refresh --auth "$acct_name"
            if [ -f "$GCAL_TOKENS/$acct_name.json" ]; then
                ok "Authenticated as '$acct_name'"
            else
                fail "OAuth flow did not produce a token"
            fi
        else
            warn "Skipped — run: gcal-refresh --auth <name>"
        fi
    fi
fi

# ------------------------------------------------------------------
# 8. Claude Ask / Claude Voice
# ------------------------------------------------------------------
step "Claude Ask / Claude Voice"

CA_DIR="$HOME/.local/share/claude-ask"
CV_DIR="$HOME/.local/share/claude-voice"

# kokoro/faster-whisper need Python <3.13 — ensure python3.11 is installed
if ! command -v python3.11 &>/dev/null; then
    yay -S --needed --noconfirm python311
    ok "python3.11 installed"
fi

# claude-ask venv + deps
if [ ! -d "$CA_DIR/.venv" ]; then
    python3.11 -m venv "$CA_DIR/.venv"
    ok "claude-ask venv created"
else
    ok "claude-ask venv already exists"
fi
"$CA_DIR/.venv/bin/pip" install -q -r "$CA_DIR/requirements.txt"
ok "claude-ask dependencies installed"

# claude-voice venv + deps
if [ ! -d "$CV_DIR/.venv" ]; then
    python3.11 -m venv "$CV_DIR/.venv"
    ok "claude-voice venv created"
else
    ok "claude-voice venv already exists"
fi
"$CV_DIR/.venv/bin/pip" install -q -r "$CV_DIR/requirements.txt"
ok "claude-voice dependencies installed"

# Archive directory for conversation transcripts
mkdir -p "$HOME/Dropbox/LLM/Chats"
ok "Conversation archive directory ready"

# Enable services
for svc in claude-ask.service claude-voice.service; do
    if systemctl --user is-enabled "$svc" &>/dev/null; then
        ok "$svc already enabled"
    else
        systemctl --user enable "$svc"
        ok "$svc enabled"
    fi
done

# ------------------------------------------------------------------
# 9. Voice typing
# ------------------------------------------------------------------
step "Voice typing setup"

VT_DIR="$HOME/.local/share/voice-typing-linux"

if [ -d "$VT_DIR" ]; then
    if [ ! -d "$VT_DIR/.venv" ]; then
        if command -v python3.11 &>/dev/null; then
            python3.11 -m venv "$VT_DIR/.venv"
            ok "Voice typing venv created"
        else
            warn "python3.11 not found — needed for onnxruntime compatibility"
        fi
    else
        ok "Voice typing venv already exists"
    fi

    if [ -d "$VT_DIR/.venv" ]; then
        "$VT_DIR/.venv/bin/pip" install -q -r "$VT_DIR/requirements.txt"
        "$VT_DIR/.venv/bin/pip" install -q requests vosk
        ok "Voice typing dependencies installed"
    fi

    # Vosk model
    VOSK_DIR="$HOME/.local/share/vosk-models"
    if [ ! -d "$VOSK_DIR/vosk-model-en-us-0.22" ]; then
        read -rp "  Download Vosk model (~1.8 GB)? (y/n) " answer
        if [[ "$answer" =~ ^[Yy]$ ]]; then
            mkdir -p "$VOSK_DIR"
            wget -q --show-progress -O "$VOSK_DIR/vosk-model-en-us-0.22.zip" \
                "https://alphacephei.com/vosk/models/vosk-model-en-us-0.22.zip"
            unzip -q "$VOSK_DIR/vosk-model-en-us-0.22.zip" -d "$VOSK_DIR"
            rm "$VOSK_DIR/vosk-model-en-us-0.22.zip"
            ok "Vosk model installed"
        else
            skip "Vosk model download"
        fi
    else
        ok "Vosk model already present"
    fi
    # Enable service
    if systemctl --user is-enabled voice-typing.service &>/dev/null; then
        ok "voice-typing.service already enabled"
    else
        systemctl --user enable voice-typing.service
        ok "voice-typing.service enabled"
    fi
else
    warn "voice-typing-linux not found at $VT_DIR"
fi

# ------------------------------------------------------------------
# 10. NVM / Node
# ------------------------------------------------------------------
step "NVM / Node.js"

export NVM_DIR="$HOME/.nvm"
if [ -d "$NVM_DIR" ]; then
    ok "NVM already installed"
else
    if [ -s /usr/share/nvm/init-nvm.sh ]; then
        source /usr/share/nvm/init-nvm.sh
        nvm install --lts
        ok "Node.js LTS installed via NVM"
    else
        warn "NVM init script not found — install nvm package"
    fi
fi


# ------------------------------------------------------------------
# 11. Email (mu4e + mbsync + msmtp)
# ------------------------------------------------------------------
step "Email setup (mu4e)"

read -rp "  Set up email (mu4e + Gmail)? (y/n) " answer
if [[ "$answer" =~ ^[Yy]$ ]]; then
    # Install mu (provides mu CLI + mu4e elisp)
    if ! command -v mu &>/dev/null; then
        if command -v yay &>/dev/null; then
            yay -S --needed --noconfirm mu
            ok "mu installed"
        else
            warn "yay not found — install mu manually: yay -S mu"
        fi
    else
        ok "mu already installed"
    fi

    # Store Gmail app password for mbsync and msmtp
    MBSYNC_PW_FILE="$HOME/.config/mbsync/password"
    MSMTP_PW_FILE="$HOME/.config/msmtp/password"

    if [ -s "$MBSYNC_PW_FILE" ]; then
        ok "mbsync password file already exists"
    else
        rm -f "$MBSYNC_PW_FILE"
        mkdir -p "$(dirname "$MBSYNC_PW_FILE")"
        bw get password a5ed5643-21a5-4664-8b03-b3e100931aac > "$MBSYNC_PW_FILE"
        chmod 600 "$MBSYNC_PW_FILE"
        ok "mbsync password saved"
    fi

    if [ -s "$MSMTP_PW_FILE" ]; then
        ok "msmtp password file already exists"
    else
        rm -f "$MSMTP_PW_FILE"
        mkdir -p "$(dirname "$MSMTP_PW_FILE")"
        bw get password a5ed5643-21a5-4664-8b03-b3e100931aac > "$MSMTP_PW_FILE"
        chmod 600 "$MSMTP_PW_FILE"
        ok "msmtp password saved"
    fi

    # Create maildir structure
    for folder in Inbox Sent Drafts Trash Archive Spam; do
        mkdir -p "$HOME/.mail/gmail/$folder"/{cur,new,tmp}
    done
    ok "Maildir structure created"

    # First sync
    if command -v mbsync &>/dev/null; then
        echo "  Running first mail sync (this may take a moment)..."
        mbsync -Va && ok "Mail synced" || warn "mbsync failed — check ~/.config/mbsync/password and .mbsyncrc"
    else
        warn "mbsync not found — install isync package"
    fi

    # Initialize mu index
    if command -v mu &>/dev/null; then
        mu init --maildir="$HOME/.mail" --my-address=scottstavinoha@gmail.com 2>/dev/null
        mu index
        ok "mu index initialized"
    fi

    # Enable mbsync timer
    if systemctl --user is-enabled mbsync.timer &>/dev/null; then
        ok "mbsync.timer already enabled"
    else
        systemctl --user enable --now mbsync.timer
        ok "mbsync.timer enabled"
    fi
else
    skip "Email setup"
fi

# ------------------------------------------------------------------
# Decrypt ssh key
# ------------------------------------------------------------------
step "Decrypt ssh key"
if gpg --card-status &>/dev/null; then
    gpg -d $HOME/.ssh/id_ed25519.gpg > ~/.ssh/id_ed25519
    chmod 600 ~/.ssh/id_ed25519
else
    echo "Warning: No hardware key detected. Skipping SSH key decryption."
fi

# ------------------------------------------------------------------
# Done
# ------------------------------------------------------------------
step "Setup complete"
echo ""
echo "  Remaining manual steps:"
echo "    1. Log out and back in (for group changes: input, dropbox)"
echo "    2. If KWallet credentials were skipped, set them up via kwalletmanager"
echo "       (folder: Bitwarden, entries: BW_CLIENTID, BW_CLIENTSECRET, BW_PASSWORD)"
echo "    3. If Dropbox binary not yet installed: download from dropbox.com"
echo "       It will install to /shared/.dropbox-dist/ via the symlink."
echo "       Then authenticate: ~/.dropbox-dist/dropboxd"
echo "    4. Start Hyprland from tty1 (happens automatically via .profile)"
echo "    5. If email was skipped: re-run setup or manually run the email steps"
echo "       (mu init, mbsync -Va, systemctl --user enable --now mbsync.timer)"
echo ""
