#!/usr/bin/env python3
"""Claude-ask daemon: listens on a Unix socket, manages conversations."""

import asyncio
import json
import logging
import os
import subprocess
import sys
import uuid
from datetime import datetime, timezone
from pathlib import Path

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    datefmt="%H:%M:%S",
)
log = logging.getLogger("claude-ask")

CONVERSATIONS_DIR = Path.home() / ".local" / "state" / "claude-ask" / "conversations"


def get_socket_path():
    runtime_dir = os.environ.get("XDG_RUNTIME_DIR", f"/run/user/{os.getuid()}")
    return os.path.join(runtime_dir, "claude-ask.sock")


# ---------------------------------------------------------------------------
# Credential loading
# ---------------------------------------------------------------------------

def load_api_key():
    """Decrypt ~/.authinfo.gpg and extract the Anthropic API key."""
    authinfo = Path.home() / ".authinfo.gpg"
    if not authinfo.exists():
        print(
            "ERROR: ~/.authinfo.gpg not found.\n"
            "Create it with a line like:\n"
            "  machine api.anthropic.com login apikey password sk-ant-api03-...\n"
            "Then encrypt:  gpg -e -r <your-key-id> ~/.authinfo",
            file=sys.stderr,
        )
        sys.exit(1)

    try:
        result = subprocess.run(
            ["gpg", "--batch", "--quiet", "--decrypt", str(authinfo)],
            capture_output=True,
            text=True,
            check=True,
        )
    except subprocess.CalledProcessError as e:
        print(f"ERROR: Failed to decrypt ~/.authinfo.gpg:\n{e.stderr}", file=sys.stderr)
        sys.exit(1)

    for line in result.stdout.splitlines():
        parts = line.split()
        if len(parts) < 6:
            continue
        # machine <host> login <user> password <pass>
        try:
            machine_idx = parts.index("machine")
            if parts[machine_idx + 1] != "api.anthropic.com":
                continue
            password_idx = parts.index("password")
            return parts[password_idx + 1]
        except (ValueError, IndexError):
            continue

    print(
        "ERROR: No entry for api.anthropic.com found in ~/.authinfo.gpg.\n"
        "Add a line like:\n"
        "  machine api.anthropic.com login apikey password sk-ant-api03-...",
        file=sys.stderr,
    )
    sys.exit(1)


# ---------------------------------------------------------------------------
# Conversation store
# ---------------------------------------------------------------------------

class ConversationStore:
    """Load and save conversation JSON files."""

    def __init__(self, directory=CONVERSATIONS_DIR):
        self.directory = directory
        self.directory.mkdir(parents=True, exist_ok=True)

    def _path_for(self, conv_id):
        return self.directory / f"{conv_id}.json"

    def get_or_create(self, conv_id=None):
        """Return an existing conversation dict, or create a new one."""
        if conv_id:
            path = self._path_for(conv_id)
            if path.exists():
                with open(path) as f:
                    return json.load(f)

        # Create new conversation
        new_id = conv_id or str(uuid.uuid4())
        return {
            "id": new_id,
            "created": datetime.now(timezone.utc).isoformat(),
            "messages": [],
        }

    def save(self, conv):
        """Write conversation dict to disk."""
        path = self._path_for(conv["id"])
        with open(path, "w") as f:
            json.dump(conv, f, indent=2)
        log.info("Saved conversation %s (%d messages)", conv["id"][:8], len(conv["messages"]))


# ---------------------------------------------------------------------------
# Daemon
# ---------------------------------------------------------------------------

class Daemon:
    def __init__(self, api_key):
        self.api_key = api_key
        self.store = ConversationStore()

    async def handle_client(self, reader, writer):
        """Read a single JSON message from a client connection."""
        try:
            data = await reader.read(65536)
            if not data:
                return

            msg = json.loads(data.decode("utf-8"))
            text = msg.get("text", "").strip()
            conv_id = msg.get("conversation_id")

            if not text:
                log.warning("Received empty message, ignoring")
                return

            log.info("Received query: %s", text[:80])
            await self.handle_query(text, conv_id)

        except json.JSONDecodeError as e:
            log.error("Invalid JSON from client: %s", e)
        except Exception:
            log.exception("Error handling client")
        finally:
            writer.close()
            await writer.wait_closed()

    async def handle_query(self, text, conv_id=None):
        """Process a query. Placeholder -- will call Claude API in Task 4."""
        conv = self.store.get_or_create(conv_id)
        conv["messages"].append({"role": "user", "content": text})
        log.info(
            "Conversation %s now has %d messages (API call not yet implemented)",
            conv["id"][:8],
            len(conv["messages"]),
        )
        self.store.save(conv)

    async def run(self):
        """Start the asyncio Unix socket server."""
        sock_path = get_socket_path()

        # Clean up stale socket
        try:
            os.unlink(sock_path)
        except FileNotFoundError:
            pass

        server = await asyncio.start_unix_server(self.handle_client, path=sock_path)
        os.chmod(sock_path, 0o600)
        log.info("Listening on %s", sock_path)

        async with server:
            await server.serve_forever()


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

def main():
    api_key = load_api_key()
    log.info("API key loaded: %s...", api_key[:12])

    daemon = Daemon(api_key)

    try:
        asyncio.run(daemon.run())
    except KeyboardInterrupt:
        log.info("Shutting down")


if __name__ == "__main__":
    main()
