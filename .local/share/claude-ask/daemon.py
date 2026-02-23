#!/usr/bin/env python3
"""Claude-ask daemon: listens on a Unix socket, manages conversations."""

import asyncio
import importlib.util
import json
import logging
import os
import subprocess
import sys
import threading
import time
import uuid
from datetime import datetime, timezone
from pathlib import Path

import anthropic

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    datefmt="%H:%M:%S",
)
log = logging.getLogger("claude-ask")

CONVERSATIONS_DIR = Path.home() / ".local" / "state" / "claude-ask" / "conversations"
TOOLS_DIR = Path.home() / ".local" / "share" / "claude-ask" / "tools"

MODEL = "claude-sonnet-4-6"
MAX_TOKENS = 4096
NOTIFY_DEBOUNCE_SECS = 0.2
NOTIFY_BODY_MAX_CHARS = 300


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
        self.client = anthropic.Anthropic(api_key=api_key)
        self.tools = self._load_tools()

    # -- Tool loading -------------------------------------------------------

    def _load_tools(self):
        """Scan ~/.local/share/claude-ask/tools/ for Python tool plugins.

        Each tool file must define module-level `name`, `description`, and
        `input_schema` variables and a `run(input)` function.  Returns a list
        of dicts suitable for the Anthropic tools parameter.
        """
        tools = []
        if not TOOLS_DIR.is_dir():
            TOOLS_DIR.mkdir(parents=True, exist_ok=True)
            log.info("Created tools directory: %s", TOOLS_DIR)
            return tools

        for path in sorted(TOOLS_DIR.glob("*.py")):
            try:
                spec = importlib.util.spec_from_file_location(path.stem, path)
                mod = importlib.util.module_from_spec(spec)
                spec.loader.exec_module(mod)

                tool_def = {
                    "name": mod.name,
                    "description": mod.description,
                    "input_schema": mod.input_schema,
                }
                tools.append(tool_def)
                log.info("Loaded tool: %s (%s)", mod.name, path.name)
            except Exception:
                log.exception("Failed to load tool from %s", path)

        return tools

    def _run_tool(self, name, input_data):
        """Execute a loaded tool by name and return its result string."""
        for path in TOOLS_DIR.glob("*.py"):
            try:
                spec = importlib.util.spec_from_file_location(path.stem, path)
                mod = importlib.util.module_from_spec(spec)
                spec.loader.exec_module(mod)
                if mod.name == name:
                    result = mod.run(input_data)
                    return str(result)
            except Exception:
                log.exception("Error running tool %s", name)
                return f"Error: tool {name} raised an exception"
        return f"Error: tool {name} not found"

    # -- Notifications ------------------------------------------------------

    def _notify(self, tag, text):
        """Fire-and-forget in-place notification update."""
        truncated = text[:NOTIFY_BODY_MAX_CHARS]
        if len(text) > NOTIFY_BODY_MAX_CHARS:
            truncated += "..."
        subprocess.Popen(
            [
                "notify-send", "-t", "0",
                "-h", f"string:x-canonical-private-synchronous:{tag}",
                "-a", "Claude",
                "Claude", truncated,
            ],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )

    def _final_notify(self, tag, text, conv_id):
        """Show final notification with Reply and Open actions.

        Runs in a background thread because --wait blocks until the user
        clicks an action or dismisses the notification.
        """
        def _run():
            truncated = text[:NOTIFY_BODY_MAX_CHARS]
            if len(text) > NOTIFY_BODY_MAX_CHARS:
                truncated += "..."
            try:
                result = subprocess.run(
                    [
                        "notify-send", "-t", "0",
                        "-h", f"string:x-canonical-private-synchronous:{tag}",
                        "-a", "Claude",
                        "-A", "reply=Reply",
                        "-A", "open=Open in Emacs",
                        "--wait",
                        "Claude", truncated,
                    ],
                    capture_output=True,
                    text=True,
                )
                action = result.stdout.strip()
                if action == "reply":
                    subprocess.Popen(["claude-ask", "--reply", conv_id])
                elif action == "open":
                    # Escape for elisp string: backslashes, then double quotes, then newlines
                    escaped = text.replace("\\", "\\\\")
                    escaped = escaped.replace('"', '\\"')
                    escaped = escaped.replace("\n", "\\n")
                    elisp = (
                        f'(let ((buf (generate-new-buffer "*Claude Response*")))'
                        f'  (switch-to-buffer buf)'
                        f'  (insert "{escaped}")'
                        f'  (goto-char (point-min))'
                        f'  (markdown-mode))'
                    )
                    subprocess.Popen(
                        ["emacsclient", "-c", "--eval", elisp],
                        stdout=subprocess.DEVNULL,
                        stderr=subprocess.DEVNULL,
                    )
            except Exception:
                log.exception("Error in final notification handler")

        thread = threading.Thread(target=_run, daemon=True)
        thread.start()

    # -- Streaming API call -------------------------------------------------

    def _stream_response(self, messages, tag):
        """Call Claude API with streaming, update notifications, return response.

        This is a synchronous blocking method meant to be called via
        run_in_executor from async code.

        Returns the full Message object from the API.
        """
        api_kwargs = {
            "model": MODEL,
            "max_tokens": MAX_TOKENS,
            "messages": messages,
        }
        if self.tools:
            api_kwargs["tools"] = self.tools

        accumulated_text = ""
        last_notify_time = 0.0

        self._notify(tag, "Thinking...")

        with self.client.messages.stream(**api_kwargs) as stream:
            for event in stream:
                if event.type == "content_block_delta":
                    if event.delta.type == "text_delta":
                        accumulated_text += event.delta.text
                        now = time.monotonic()
                        if now - last_notify_time >= NOTIFY_DEBOUNCE_SECS:
                            self._notify(tag, accumulated_text)
                            last_notify_time = now

            # Final update with complete text
            response = stream.get_final_message()

        return response

    # -- Query handling -----------------------------------------------------

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
        """Process a query: stream Claude API response with live notifications."""
        conv = self.store.get_or_create(conv_id)
        conv["messages"].append({"role": "user", "content": text})
        tag = f"claude-{conv['id'][:8]}"

        loop = asyncio.get_running_loop()

        try:
            await self._complete_conversation(conv, tag, loop)
        except anthropic.APIError as e:
            log.error("API error: %s", e)
            self._notify(tag, f"API error: {e}")
            # Save conversation so the user message is preserved for retry
            self.store.save(conv)
            return
        except Exception:
            log.exception("Unexpected error during API call")
            self._notify(tag, "Unexpected error (check logs)")
            self.store.save(conv)
            return

    async def _complete_conversation(self, conv, tag, loop):
        """Stream responses, handle tool calls in a loop until done."""
        while True:
            response = await loop.run_in_executor(
                None, self._stream_response, conv["messages"], tag
            )

            # Build the assistant message content blocks for the conversation
            # The API returns content blocks that may include text and tool_use
            assistant_content = []
            tool_use_blocks = []

            for block in response.content:
                if block.type == "text":
                    assistant_content.append({
                        "type": "text",
                        "text": block.text,
                    })
                elif block.type == "tool_use":
                    assistant_content.append({
                        "type": "tool_use",
                        "id": block.id,
                        "name": block.name,
                        "input": block.input,
                    })
                    tool_use_blocks.append(block)

            conv["messages"].append({"role": "assistant", "content": assistant_content})

            if not tool_use_blocks:
                # No tool calls -- we're done
                break

            # Execute each tool and feed results back
            tool_results = []
            for tool_block in tool_use_blocks:
                log.info("Executing tool: %s", tool_block.name)
                self._notify(tag, f"Running tool: {tool_block.name}...")
                result_text = self._run_tool(tool_block.name, tool_block.input)
                log.info("Tool %s result: %s", tool_block.name, result_text[:100])
                tool_results.append({
                    "type": "tool_result",
                    "tool_use_id": tool_block.id,
                    "content": result_text,
                })

            conv["messages"].append({"role": "user", "content": tool_results})
            # Loop continues -- next iteration will call the API again

        # Extract final text for the notification
        final_text = ""
        for block in response.content:
            if block.type == "text":
                final_text += block.text

        self.store.save(conv)
        log.info("Conversation %s complete (%d messages)", conv["id"][:8], len(conv["messages"]))

        # Show final notification with actions
        self._final_notify(tag, final_text, conv["id"])

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
