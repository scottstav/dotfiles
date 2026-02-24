#!/usr/bin/env python3
"""Claude-ask daemon: listens on a Unix socket, manages conversations."""

import asyncio
import importlib.util
import json
import logging
import os
import socket
import subprocess
import sys
import threading
import time
import uuid
from datetime import datetime, timezone
from pathlib import Path

import anthropic
import tomllib

from sentence_buffer import SentenceBuffer
from tts import TTSPipeline
from waybar_state import WaybarState

from format import archive_path, format_conversation

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    datefmt="%H:%M:%S",
)
log = logging.getLogger("claude-ask")

CONVERSATIONS_DIR = Path.home() / ".local" / "state" / "claude-ask" / "conversations"
LAST_STATE_FILE = Path.home() / ".local" / "state" / "claude-ask" / "last.json"
USAGE_LOG = Path.home() / ".local" / "state" / "claude-ask" / "usage.jsonl"
CONFIG_FILE = Path.home() / ".config" / "claude-ask" / "config.toml"
TOOLS_DIR = Path.home() / ".local" / "share" / "claude-ask" / "tools"

MODEL = "claude-sonnet-4-6"
MAX_TOKENS = 4096
NOTIFY_DEBOUNCE_SECS = 0.2
NOTIFY_BODY_MAX_CHARS = 300

TOKEN_PRICES = {  # USD per million tokens
    "claude-sonnet-4-6":  {"input": 3.0, "output": 15.0},
    "claude-opus-4-6":    {"input": 5.0, "output": 25.0},
    "claude-haiku-4-5":   {"input": 1.0, "output": 5.0},
}

SYSTEM_PROMPT = """\
You are a quick-answer assistant running as a desktop overlay on a Linux workstation \
(Arch Linux, Hyprland/Wayland, Emacs). Responses are displayed in desktop notifications \
with limited space.

Be extremely concise. Default to 1-3 sentences. Use bullet points over paragraphs. \
Skip preamble, hedging, and sign-offs. Only give longer responses when the user \
explicitly asks for detail or the question genuinely requires it.

You have tools. Use them proactively:
- shell: run commands to answer questions about the system, files, processes, etc.
- web_search + fetch_url: search the web, then read articles for current info/news.
- clipboard: copy useful output to the user's clipboard without being asked.
- screenshot: capture the screen when the user asks about something visible.\
"""


def get_socket_path():
    runtime_dir = os.environ.get("XDG_RUNTIME_DIR", f"/run/user/{os.getuid()}")
    return os.path.join(runtime_dir, "claude-ask.sock")


def load_voice_config():
    """Load voice configuration from config.toml."""
    defaults = {
        "enabled": False,
        "model": "af_heart",
        "speed": 1.0,
        "lang": "a",
        "filter": {"skip_code_blocks": True, "skip_urls": True},
    }
    try:
        with open(CONFIG_FILE, "rb") as f:
            config = tomllib.load(f)
        voice = config.get("voice", {})
        return {
            "enabled": voice.get("enabled", defaults["enabled"]),
            "model": voice.get("model", defaults["model"]),
            "speed": voice.get("speed", defaults["speed"]),
            "lang": voice.get("lang", defaults["lang"]),
            "filter": voice.get("filter", defaults["filter"]),
        }
    except (FileNotFoundError, tomllib.TOMLDecodeError):
        log.warning("Could not load voice config, using defaults")
        return defaults


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
        self.waybar = WaybarState()
        self._voice_config = load_voice_config()
        self.waybar.speak_enabled = self._voice_config["enabled"]
        self.tts = TTSPipeline(
            model=self._voice_config["model"],
            speed=self._voice_config["speed"],
            lang=self._voice_config["lang"],
        )
        self._session_tokens = 0

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
        """Execute a loaded tool by name.

        Returns either a string or a dict with type/data/media_type for images.
        """
        for path in TOOLS_DIR.glob("*.py"):
            try:
                spec = importlib.util.spec_from_file_location(path.stem, path)
                mod = importlib.util.module_from_spec(spec)
                spec.loader.exec_module(mod)
                if mod.name == name:
                    result = mod.run(input_data)
                    # Tools can return a dict for image results
                    if isinstance(result, dict) and result.get("type") == "image":
                        return result
                    return str(result)
            except Exception:
                log.exception("Error running tool %s", name)
                return f"Error: tool {name} raised an exception"
        return f"Error: tool {name} not found"

    # -- State tracking -----------------------------------------------------

    def _save_last_state(self, conv_id):
        """Write last.json so the TUI knows the most recent conversation."""
        try:
            LAST_STATE_FILE.write_text(json.dumps({
                "conversation_id": conv_id,
                "timestamp": time.time(),
            }))
        except OSError:
            pass

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

    def _trigger_voice_listen(self, conv_id):
        """Send a listen command to the claude-voice daemon."""
        runtime_dir = os.environ.get("XDG_RUNTIME_DIR", f"/run/user/{os.getuid()}")
        sock_path = os.path.join(runtime_dir, "claude-voice.sock")
        payload = json.dumps({"action": "listen", "conversation_id": conv_id})
        try:
            sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            sock.connect(sock_path)
            sock.sendall(payload.encode("utf-8"))
            sock.close()
            log.info("Triggered claude-voice listen for conv %s", conv_id[:8])
        except (ConnectionRefusedError, FileNotFoundError):
            log.warning("claude-voice not running, cannot trigger mic")

    def _archive_conversation(self, conv, arch):
        """Write formatted transcript to Dropbox. Runs in executor thread."""
        try:
            arch.write_text(format_conversation(conv))
            log.info("Archived conversation to %s", arch)
        except OSError:
            log.exception("Failed to write archive %s", arch)

    # -- Usage tracking -----------------------------------------------------

    def _log_usage(self, response):
        """Append token usage for one API call to the JSONL log."""
        usage = response.usage
        model = response.model
        prices = TOKEN_PRICES.get(model, TOKEN_PRICES["claude-sonnet-4-6"])
        cost = (usage.input_tokens * prices["input"] + usage.output_tokens * prices["output"]) / 1_000_000
        entry = {
            "ts": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
            "model": model,
            "input_tokens": usage.input_tokens,
            "output_tokens": usage.output_tokens,
            "cost_usd": round(cost, 6),
        }
        try:
            USAGE_LOG.parent.mkdir(parents=True, exist_ok=True)
            with open(USAGE_LOG, "a") as f:
                f.write(json.dumps(entry) + "\n")
        except OSError:
            log.exception("Failed to write usage log")

    def _final_notify(self, tag, text, conv_id, archive_file=None):
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
                        "-A", "reply=\U000f0369",
                        "-A", "mic=\U000f036c",
                        "-A", "open=\U000f0219",
                        "--wait",
                        "Claude", truncated,
                    ],
                    capture_output=True,
                    text=True,
                )
                action = result.stdout.strip()
                if action == "reply":
                    subprocess.Popen(["claude-ask", "--reply", conv_id])
                elif action == "mic":
                    self._trigger_voice_listen(conv_id)
                elif action == "open":
                    if archive_file and archive_file.exists():
                        subprocess.Popen(
                            ["emacsclient", "-c", str(archive_file)],
                            stdout=subprocess.DEVNULL,
                            stderr=subprocess.DEVNULL,
                        )
            except Exception:
                log.exception("Error in final notification handler")

        thread = threading.Thread(target=_run, daemon=True)
        thread.start()

    # -- Streaming API call -------------------------------------------------

    def _stream_response(self, messages, tag):
        """Call Claude API with streaming, update notifications, return response."""
        api_kwargs = {
            "model": MODEL,
            "max_tokens": MAX_TOKENS,
            "system": SYSTEM_PROMPT,
            "messages": messages,
        }
        if self.tools:
            api_kwargs["tools"] = self.tools

        accumulated_text = ""
        last_notify_time = 0.0
        sentence_buf = SentenceBuffer()

        speak_on = self.waybar.speak_enabled

        with self.client.messages.stream(**api_kwargs) as stream:
            for event in stream:
                if event.type == "content_block_delta":
                    if event.delta.type == "text_delta":
                        accumulated_text += event.delta.text

                        if speak_on:
                            sentences = sentence_buf.add(event.delta.text)
                            for sentence in sentences:
                                self.tts.speak(sentence)

                        if not speak_on:
                            now = time.monotonic()
                            if now - last_notify_time >= NOTIFY_DEBOUNCE_SECS:
                                self._notify(tag, accumulated_text)
                                last_notify_time = now

            response = stream.get_final_message()

        # Flush remaining text in sentence buffer
        if speak_on:
            for sentence in sentence_buf.flush():
                self.tts.speak(sentence)

        return response

    # -- Query handling -----------------------------------------------------

    async def handle_client(self, reader, writer):
        """Read a single JSON message from a client connection."""
        try:
            # Read all data (up to 10MB for image payloads)
            chunks = []
            while True:
                chunk = await reader.read(65536)
                if not chunk:
                    break
                chunks.append(chunk)
            data = b"".join(chunks)
            if not data:
                return

            msg = json.loads(data.decode("utf-8"))

            # Handle control actions
            action = msg.get("action")
            if action == "stop_tts":
                log.info("Stop TTS requested via socket")
                self.tts.stop()
                self.waybar.set_status("idle")
                return

            text = msg.get("text", "").strip()
            conv_id = msg.get("conversation_id")
            image = msg.get("image")  # base64 PNG or None
            file = msg.get("file")    # file path or None

            if not text:
                log.warning("Received empty message, ignoring")
                return

            log.info("Received query: %s%s%s", text[:80],
                     " (+image)" if image else "",
                     f" (+file: {file})" if file else "")
            await self.handle_query(text, conv_id, image=image, file=file)

        except json.JSONDecodeError as e:
            log.error("Invalid JSON from client: %s", e)
        except Exception:
            log.exception("Error handling client")
        finally:
            writer.close()
            await writer.wait_closed()

    async def handle_query(self, text, conv_id=None, image=None, file=None):
        """Process a query: stream Claude API response with live notifications."""
        conv = self.store.get_or_create(conv_id)

        # Inject attached file context into the text
        if file:
            text = f"[Attached file: {file}]\nWhen you produce an output file, copy it to the clipboard using the clipboard tool's file parameter.\n\n{text}"

        # Build user message content (text or text+image)
        if image:
            user_content = [
                {
                    "type": "image",
                    "source": {
                        "type": "base64",
                        "media_type": "image/png",
                        "data": image,
                    },
                },
                {"type": "text", "text": text},
            ]
        else:
            user_content = text
        conv["messages"].append({"role": "user", "content": user_content})
        tag = f"claude-{conv['id'][:8]}"

        loop = asyncio.get_running_loop()

        try:
            await self._complete_conversation(conv, tag, loop)
        except anthropic.APIError as e:
            log.error("API error: %s", e)
            self._notify(tag, f"API error: {e}")
            self.tts.stop()
            self.waybar.set_status("idle")
            self.store.save(conv)
            return
        except Exception:
            log.exception("Unexpected error during API call")
            self._notify(tag, "Unexpected error (check logs)")
            self.tts.stop()
            self.waybar.set_status("idle")
            self.store.save(conv)
            return

    async def _complete_conversation(self, conv, tag, loop):
        """Stream responses, handle tool calls in a loop until done."""
        # Start TTS once for the whole conversation (survives tool call loops)
        self.waybar.reload_speak_enabled()
        if self.waybar.speak_enabled:
            self.tts.start()
            self.waybar.set_status("speaking")
        else:
            self.waybar.set_status("thinking")

        while True:
            response = await loop.run_in_executor(
                None, self._stream_response, conv["messages"], tag
            )
            self._log_usage(response)

            # Update Waybar usage display
            prices = TOKEN_PRICES.get(response.model, TOKEN_PRICES["claude-sonnet-4-6"])
            query_cost = (response.usage.input_tokens * prices["input"] +
                          response.usage.output_tokens * prices["output"]) / 1_000_000
            self._session_tokens += response.usage.input_tokens + response.usage.output_tokens
            self.waybar.update_usage(query_cost, self._session_tokens)

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
                self.waybar.set_status("tool_use", tool_name=tool_block.name)
                result = self._run_tool(tool_block.name, tool_block.input)

                # Image results get wrapped as image content blocks
                if isinstance(result, dict) and result.get("type") == "image":
                    log.info("Tool %s returned image (%d bytes)", tool_block.name, len(result["data"]))
                    content = [{
                        "type": "image",
                        "source": {
                            "type": "base64",
                            "media_type": result["media_type"],
                            "data": result["data"],
                        },
                    }]
                else:
                    log.info("Tool %s result: %s", tool_block.name, str(result)[:100])
                    content = str(result)

                tool_results.append({
                    "type": "tool_result",
                    "tool_use_id": tool_block.id,
                    "content": content,
                })

            conv["messages"].append({"role": "user", "content": tool_results})
            if self.waybar.speak_enabled:
                self.waybar.set_status("speaking")
            else:
                self.waybar.set_status("thinking")
            # Loop continues -- next iteration will call the API again

        # Extract final text for the notification
        final_text = ""
        for block in response.content:
            if block.type == "text":
                final_text += block.text

        self.store.save(conv)
        self._save_last_state(conv["id"])

        # Wait for TTS to finish speaking if active
        if self.waybar.status == "speaking":
            self.tts.finish()
            self.tts.wait_done(timeout=120)
            self.tts.stop()

        self.waybar.set_status("idle")

        log.info("Conversation %s complete (%d messages)", conv["id"][:8], len(conv["messages"]))

        # Archive formatted conversation to ~/Dropbox/LLM/Chats/ in background
        arch = archive_path(conv)
        loop = asyncio.get_running_loop()
        loop.run_in_executor(None, self._archive_conversation, conv, arch)

        # Show final notification immediately without waiting for archiving
        self._final_notify(tag, final_text, conv["id"], arch)

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
    finally:
        daemon.tts.stop()
        daemon.waybar.set_status("idle")


if __name__ == "__main__":
    main()
