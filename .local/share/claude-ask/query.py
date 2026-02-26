"""Query pipeline: send text to Claude, stream response, run tools, speak.

All functions are self-contained helpers with no daemon/socket dependencies.
Called by claude-voice daemon threads and the TUI.
"""

import importlib.util
import json
import logging
import os
import socket
import subprocess
import threading
import time
import uuid
from datetime import datetime, timezone
from pathlib import Path

import anthropic
import tomllib

from format import archive_path, format_conversation
from sentence_buffer import SentenceBuffer
from tts import TTSPipeline
from waybar_state import WaybarState

log = logging.getLogger("claude-ask")

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

CONVERSATIONS_DIR = Path.home() / ".local" / "state" / "claude-ask" / "conversations"
LAST_STATE_FILE = Path.home() / ".local" / "state" / "claude-ask" / "last.json"
USAGE_LOG = Path.home() / ".local" / "state" / "claude-ask" / "usage.jsonl"
CONFIG_FILE = Path.home() / ".config" / "claude-ask" / "config.toml"
TOOLS_DIR = Path.home() / ".local" / "share" / "claude-ask" / "tools"

DEFAULT_MODEL = "claude-sonnet-4-6"
MAX_TOKENS = 4096
AUTO_REPLY_THRESHOLD_SECS = 60

AVAILABLE_MODELS = [
    {"id": "claude-haiku-4-5", "short": "Haiku 4.5"},
    {"id": "claude-sonnet-4-6", "short": "Sonnet 4.6"},
    {"id": "claude-opus-4-6", "short": "Opus 4.6"},
]

TOKEN_PRICES = {
    "claude-sonnet-4-6":  {"input": 3.0, "output": 15.0},
    "claude-opus-4-6":    {"input": 5.0, "output": 25.0},
    "claude-haiku-4-5":   {"input": 1.0, "output": 5.0},
}

# Sentinel for explicit "start a new conversation"
NEW_CONVERSATION = object()

_SYSTEM_PROMPT_TEMPLATE = """\
You are a quick-answer assistant running as a desktop overlay on a Linux workstation \
(Arch Linux, Hyprland/Wayland, Emacs). Responses are displayed in desktop notifications \
with limited space.

Today is {today_long} ({today_iso}, {weekday}).

Be extremely concise. Default to 1-3 sentences. Use bullet points over paragraphs. \
Skip preamble, hedging, and sign-offs. Only give longer responses when the user \
explicitly asks for detail or the question genuinely requires it.

You have tools. Use them proactively:
- shell: run commands to answer questions about the system, files, processes, etc.
- search_notes: search the user's denote notes by keyword. Good for finding topics \
  across many notes, but only returns snippets — use shell to read full files.
- web_search + fetch_url: search the web, then read articles for current info/news.
- clipboard: copy useful output to the user's clipboard without being asked.
- screenshot: capture the screen when the user asks about something visible.

Denote notes: all notes live under ~/Dropbox/org/denote/ (with subdirs like journal/, \
work/journal/, work/docs/). Every filename starts with a timestamp: YYYYMMDDTHHMMSS. \
When the user asks about a date or time range ("today", "last week", "february 2024"), \
use shell to find files by their date prefix. Examples:
  ls ~/Dropbox/org/denote/journal/20260226*        # specific day
  ls ~/Dropbox/org/denote/journal/202602*           # whole month
  ls ~/Dropbox/org/denote/**/202402*                # feb 2024, all subdirs
  ls -t ~/Dropbox/org/denote/journal/ | head -5     # most recent entries
Then cat the matching files to read their contents. Use search_notes only for \
keyword/topic searches, not date-based lookups.

You can delegate complex coding tasks to a Claude Code worker session. Workers are \
autonomous agents running in their own terminal with full codebase context, file \
editing, git, and multi-file reasoning. Use spawn_worker when the task requires:
- Changes across multiple files
- Understanding significant codebase context to make even a single change \
  (tracing call chains, reading tests, understanding architecture)
- Implementing a feature, refactoring, or debugging that requires exploration

Do NOT use spawn_worker for simple tasks you can handle with the shell tool: \
quick file edits, adding a journal entry, changing a config value, running a command. \
If in doubt, just answer — the user can always ask for a worker explicitly.

When using spawn_worker, infer the project directory from context. Common locations: \
~/code/, ~/dotfiles/, ~/Dropbox/. Use the shell tool to search if unsure (e.g. \
fd -t d <name> ~). Write a thorough, detailed task description for the worker — \
it operates autonomously without follow-up.\
"""


def _build_system_prompt() -> str:
    now = datetime.now()
    return _SYSTEM_PROMPT_TEMPLATE.format(
        today_long=now.strftime("%d %B %Y"),
        today_iso=now.strftime("%Y-%m-%d"),
        weekday=now.strftime("%A"),
        today_prefix=now.strftime("%Y%m%d"),
    )

# ---------------------------------------------------------------------------
# Module-level singletons (lazy)
# ---------------------------------------------------------------------------

_client: anthropic.Anthropic | None = None
_waybar: WaybarState | None = None
_tts: TTSPipeline | None = None
_tools: list[dict] | None = None
_session_tokens = 0


def _get_client() -> anthropic.Anthropic:
    global _client
    if _client is None:
        _client = anthropic.Anthropic(api_key=_load_api_key())
    return _client


def _get_waybar() -> WaybarState:
    global _waybar
    if _waybar is None:
        _waybar = WaybarState()
        voice_cfg = _load_voice_config()
        _waybar.speak_enabled = voice_cfg["enabled"]
    return _waybar


def _get_tts() -> TTSPipeline:
    global _tts
    if _tts is None:
        voice_cfg = _load_voice_config()
        _tts = TTSPipeline(
            model=voice_cfg["model"],
            speed=voice_cfg["speed"],
            lang=voice_cfg["lang"],
        )
    return _tts


def _get_tools() -> list[dict]:
    global _tools
    if _tools is None:
        _tools = _load_tools()
    return _tools


# ---------------------------------------------------------------------------
# Credential loading
# ---------------------------------------------------------------------------

def _load_api_key() -> str:
    """Decrypt ~/.authinfo.gpg and extract the Anthropic API key."""
    authinfo = Path.home() / ".authinfo.gpg"
    if not authinfo.exists():
        raise RuntimeError("~/.authinfo.gpg not found")

    result = subprocess.run(
        ["gpg", "--batch", "--quiet", "--decrypt", str(authinfo)],
        capture_output=True, text=True, check=True,
    )

    for line in result.stdout.splitlines():
        parts = line.split()
        if len(parts) < 6:
            continue
        try:
            machine_idx = parts.index("machine")
            if parts[machine_idx + 1] != "api.anthropic.com":
                continue
            password_idx = parts.index("password")
            return parts[password_idx + 1]
        except (ValueError, IndexError):
            continue

    raise RuntimeError("No api.anthropic.com entry in ~/.authinfo.gpg")


def _load_model() -> str:
    """Read the configured model from config.toml, falling back to DEFAULT_MODEL."""
    try:
        with open(CONFIG_FILE, "rb") as f:
            config = tomllib.load(f)
        return config.get("model", {}).get("name", DEFAULT_MODEL)
    except (FileNotFoundError, tomllib.TOMLDecodeError):
        return DEFAULT_MODEL


def _load_voice_config() -> dict:
    defaults = {
        "enabled": False, "model": "af_heart",
        "speed": 1.0, "lang": "a",
        "filter": {"skip_code_blocks": True, "skip_urls": True},
    }
    try:
        with open(CONFIG_FILE, "rb") as f:
            config = tomllib.load(f)
        voice = config.get("voice", {})
        return {k: voice.get(k, defaults[k]) for k in defaults}
    except (FileNotFoundError, tomllib.TOMLDecodeError):
        return defaults


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
        if conv_id:
            path = self._path_for(conv_id)
            if path.exists():
                with open(path) as f:
                    return json.load(f)
        new_id = conv_id or str(uuid.uuid4())
        return {
            "id": new_id,
            "created": datetime.now(timezone.utc).isoformat(),
            "messages": [],
        }

    def save(self, conv):
        path = self._path_for(conv["id"])
        with open(path, "w") as f:
            json.dump(conv, f, indent=2)
        log.info("Saved conversation %s (%d messages)", conv["id"][:8], len(conv["messages"]))


_store = ConversationStore()


# ---------------------------------------------------------------------------
# Tool loading and execution
# ---------------------------------------------------------------------------

def _load_tools() -> list[dict]:
    tools = []
    if not TOOLS_DIR.is_dir():
        TOOLS_DIR.mkdir(parents=True, exist_ok=True)
        return tools
    for path in sorted(TOOLS_DIR.glob("*.py")):
        try:
            spec = importlib.util.spec_from_file_location(path.stem, path)
            mod = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(mod)
            tools.append({
                "name": mod.name,
                "description": mod.description,
                "input_schema": mod.input_schema,
            })
            log.info("Loaded tool: %s (%s)", mod.name, path.name)
        except Exception:
            log.exception("Failed to load tool from %s", path)
    return tools


def run_tool(name: str, input_data: dict):
    """Execute a tool plugin by name. Returns str or image dict."""
    for path in TOOLS_DIR.glob("*.py"):
        try:
            spec = importlib.util.spec_from_file_location(path.stem, path)
            mod = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(mod)
            if mod.name == name:
                result = mod.run(input_data)
                if isinstance(result, dict) and result.get("type") == "image":
                    return result
                return str(result)
        except Exception:
            log.exception("Error running tool %s", name)
            return f"Error: tool {name} raised an exception"
    return f"Error: tool {name} not found"


# ---------------------------------------------------------------------------
# Notifications
# ---------------------------------------------------------------------------

def notify(tag: str, text: str):
    """Fire-and-forget in-place notification update."""
    subprocess.Popen(
        ["notify-send", "-t", "0",
         "-h", f"string:x-canonical-private-synchronous:{tag}",
         "-a", "Claude", "Claude", text],
        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
    )


def _connect_overlay():
    """Connect to the claude-overlay Unix socket. Returns socket or None."""
    runtime_dir = os.environ.get("XDG_RUNTIME_DIR", f"/run/user/{os.getuid()}")
    sock_path = os.path.join(runtime_dir, "claude-overlay.sock")
    try:
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        sock.connect(sock_path)
        return sock
    except (ConnectionRefusedError, FileNotFoundError, OSError):
        return None


def _overlay_send(sock, msg):
    """Send a JSON-line command to the overlay. Swallows errors."""
    if sock is None:
        return
    try:
        sock.sendall((json.dumps(msg) + "\n").encode("utf-8"))
    except (BrokenPipeError, OSError):
        pass


def _overlay_close(sock):
    """Close overlay socket, ignoring errors."""
    if sock is None:
        return
    try:
        sock.close()
    except OSError:
        pass


def _voice_control(action: str, **extra):
    """Send a control command to the claude-voice daemon."""
    runtime_dir = os.environ.get("XDG_RUNTIME_DIR", f"/run/user/{os.getuid()}")
    sock_path = os.path.join(runtime_dir, "claude-voice.sock")
    payload = json.dumps({"action": action, **extra})
    try:
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        sock.connect(sock_path)
        sock.sendall(payload.encode("utf-8"))
        sock.close()
    except (ConnectionRefusedError, FileNotFoundError):
        pass


def notify_final(tag: str, text: str, conv_id: str,
                 archive_file: Path | None = None,
                 tools_used: list[str] | None = None):
    """Show final notification with Reply, Mic, and Open actions.

    Runs in a background thread because --wait blocks.
    """
    def _run():
        body = text
        if tools_used:
            body += "\n\n<small>" + " \u2192 ".join(tools_used) + "</small>"
        try:
            result = subprocess.run(
                ["notify-send", "-t", "0",
                 "-h", f"string:x-canonical-private-synchronous:{tag}",
                 "-a", "Claude",
                 "-A", "reply=\U000f0369",
                 "-A", "mic=\U000f036c",
                 "-A", "open=\U000f0219",
                 "--wait", "Claude", body],
                capture_output=True, text=True,
            )
            action = result.stdout.strip()
            if action == "reply":
                subprocess.Popen(["claude-ask", "--reply", conv_id])
            elif action == "mic":
                _voice_control("listen", conversation_id=conv_id)
            elif action == "open":
                if archive_file and archive_file.exists():
                    subprocess.Popen(
                        ["emacsclient", "-c", str(archive_file)],
                        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                    )
        except Exception:
            log.exception("Error in final notification handler")

    threading.Thread(target=_run, daemon=True).start()


# ---------------------------------------------------------------------------
# Usage tracking
# ---------------------------------------------------------------------------

def _log_usage(response):
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


def _save_last_state(conv_id: str):
    try:
        LAST_STATE_FILE.parent.mkdir(parents=True, exist_ok=True)
        LAST_STATE_FILE.write_text(json.dumps({
            "conversation_id": conv_id,
            "timestamp": time.time(),
        }))
    except OSError:
        pass


# ---------------------------------------------------------------------------
# Conversation resolution (tri-state)
# ---------------------------------------------------------------------------

def auto_resolve_conversation():
    """Read last.json, return conv_id if <60s ago, else None."""
    try:
        data = json.loads(LAST_STATE_FILE.read_text())
        conv_id = data.get("conversation_id")
        ts = data.get("timestamp", 0)
        if conv_id and (time.time() - ts) < AUTO_REPLY_THRESHOLD_SECS:
            return conv_id
    except (FileNotFoundError, json.JSONDecodeError, OSError):
        pass
    return None


# ---------------------------------------------------------------------------
# Streaming API call
# ---------------------------------------------------------------------------

def stream_response(messages: list, tag: str, cancel_event: threading.Event | None = None,
                    prior_text: str = "", overlay_sock=None) -> tuple:
    """Call Claude API with streaming, update overlay and TTS.

    Returns (response, accumulated_text).
    """
    client = _get_client()
    tools = _get_tools()
    waybar = _get_waybar()
    tts = _get_tts()

    api_kwargs = {
        "model": _load_model(),
        "max_tokens": MAX_TOKENS,
        "system": _build_system_prompt(),
        "messages": messages,
    }
    if tools:
        api_kwargs["tools"] = tools

    accumulated_text = prior_text
    sentence_buf = SentenceBuffer()
    speak_on = waybar.speak_enabled

    with client.messages.stream(**api_kwargs) as stream:
        for event in stream:
            if cancel_event and cancel_event.is_set():
                _overlay_send(overlay_sock, {"cmd": "clear"})
                stream.close()
                break
            if event.type == "content_block_delta":
                if event.delta.type == "text_delta":
                    accumulated_text += event.delta.text

                    if speak_on:
                        for sentence in sentence_buf.add(event.delta.text):
                            tts.speak(sentence)

                    _overlay_send(overlay_sock, {
                        "cmd": "text",
                        "data": event.delta.text,
                    })

        response = stream.get_final_message()

    if speak_on:
        for sentence in sentence_buf.flush():
            tts.speak(sentence)

    return response, accumulated_text


# ---------------------------------------------------------------------------
# Main query pipeline
# ---------------------------------------------------------------------------

def send_query(text: str, conversation_id=None, cancel_event: threading.Event | None = None,
               image: str | None = None, file: str | None = None):
    """Send text to Claude. The single entry point for all query paths.

    conversation_id:
      - None              -> auto-detect (<60s check)
      - NEW_CONVERSATION  -> explicit fresh conversation
      - "uuid-string"     -> continue that conversation
    """
    global _session_tokens

    # Resolve conversation (tri-state)
    if conversation_id is NEW_CONVERSATION:
        conv = _store.get_or_create()
    elif conversation_id is not None:
        conv = _store.get_or_create(conversation_id)
    else:
        resolved = auto_resolve_conversation()
        conv = _store.get_or_create(resolved) if resolved else _store.get_or_create()

    # Inject file context
    if file:
        text = (f"[Attached file: {file}]\n"
                "When you produce an output file, copy it to the clipboard "
                "using the clipboard tool's file parameter.\n\n" + text)

    # Build user message content
    if image:
        user_content = [
            {"type": "image", "source": {
                "type": "base64", "media_type": "image/png", "data": image}},
            {"type": "text", "text": text},
        ]
    else:
        user_content = text
    conv["messages"].append({"role": "user", "content": user_content})
    tag = f"claude-{conv['id'][:8]}"

    waybar = _get_waybar()
    tts = _get_tts()

    # Start TTS if enabled
    waybar.reload_speak_enabled()
    if waybar.speak_enabled:
        _voice_control("mute")
        tts.start()
        waybar.set_status("speaking")
    else:
        waybar.set_status("thinking")

    tools_used = []
    full_text = ""
    overlay_sock = _connect_overlay()
    _overlay_send(overlay_sock, {"cmd": "open"})

    try:
        while True:
            response, full_text = stream_response(
                conv["messages"], tag, cancel_event=cancel_event,
                prior_text=full_text, overlay_sock=overlay_sock,
            )
            _log_usage(response)

            # Update Waybar usage
            prices = TOKEN_PRICES.get(response.model, TOKEN_PRICES["claude-sonnet-4-6"])
            query_cost = (response.usage.input_tokens * prices["input"] +
                          response.usage.output_tokens * prices["output"]) / 1_000_000
            _session_tokens += response.usage.input_tokens + response.usage.output_tokens
            waybar.update_usage(query_cost, _session_tokens)

            # Build assistant message
            assistant_content = []
            tool_use_blocks = []
            for block in response.content:
                if block.type == "text":
                    assistant_content.append({"type": "text", "text": block.text})
                elif block.type == "tool_use":
                    assistant_content.append({
                        "type": "tool_use", "id": block.id,
                        "name": block.name, "input": block.input,
                    })
                    tool_use_blocks.append(block)
            conv["messages"].append({"role": "assistant", "content": assistant_content})

            if not tool_use_blocks or (cancel_event and cancel_event.is_set()):
                break

            # Execute tools
            tool_results = []
            for tool_block in tool_use_blocks:
                if cancel_event and cancel_event.is_set():
                    break
                tools_used.append(tool_block.name)
                log.info("Executing tool: %s", tool_block.name)
                waybar.set_status("tool_use", tool_name=tool_block.name)
                result = run_tool(tool_block.name, tool_block.input)

                if isinstance(result, dict) and result.get("type") == "image":
                    content = [{"type": "image", "source": {
                        "type": "base64", "media_type": result["media_type"],
                        "data": result["data"]}}]
                else:
                    content = str(result)

                tool_results.append({
                    "type": "tool_result",
                    "tool_use_id": tool_block.id,
                    "content": content,
                })

            conv["messages"].append({"role": "user", "content": tool_results})
            tool_names = " \u2192 ".join(b.name for b in tool_use_blocks)
            if full_text:
                full_text += "\n\n"
            full_text += f"{tool_names}\n\n"
            _overlay_send(overlay_sock, {"cmd": "replace", "data": full_text})
            if waybar.speak_enabled and tts._running:
                waybar.set_status("speaking")
            else:
                waybar.set_status("thinking")

        _store.save(conv)
        _save_last_state(conv["id"])

        # Wait for TTS (check _running, not waybar status — status can be
        # changed externally by stop_tts without the TTS threads being alive)
        if tts._running:
            tts.finish()
            tts.wait_done(120)
            tts.stop()
            _voice_control("unmute")

        waybar.set_status("idle")
        log.info("Conversation %s complete (%d messages)", conv["id"][:8], len(conv["messages"]))

        # Archive
        arch = archive_path(conv)
        threading.Thread(
            target=lambda: arch.write_text(format_conversation(conv)),
            daemon=True,
        ).start()

        # Fade out overlay
        _overlay_send(overlay_sock, {"cmd": "done"})
        _overlay_close(overlay_sock)

        # Final notification
        notify_final(tag, full_text, conv["id"], arch, tools_used=tools_used or None)

    except anthropic.APIError as e:
        log.error("API error: %s", e)
        _overlay_send(overlay_sock, {"cmd": "clear"})
        _overlay_close(overlay_sock)
        notify(tag, f"API error: {e}")
        tts.stop()
        _voice_control("unmute")
        waybar.set_status("idle")
        _store.save(conv)
    except Exception:
        log.exception("Unexpected error during query")
        _overlay_send(overlay_sock, {"cmd": "clear"})
        _overlay_close(overlay_sock)
        notify(tag, "Unexpected error (check logs)")
        tts.stop()
        _voice_control("unmute")
        waybar.set_status("idle")
        _store.save(conv)
