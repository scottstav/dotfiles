"""Search and read emails from the local Maildir store via mu."""

import importlib.util
import json
import re
import subprocess
from email import policy
from email.parser import BytesParser
from pathlib import Path

# Load _name_variants via importlib — can't use sys.path because this file
# is named email.py and adding tools dir to sys.path shadows stdlib email.
_nv_spec = importlib.util.spec_from_file_location(
    "_name_variants", str(Path(__file__).parent / "_name_variants.py")
)
_nv_mod = importlib.util.module_from_spec(_nv_spec)
_nv_spec.loader.exec_module(_nv_mod)
get_variants = _nv_mod.get_variants

TOOL_SPEC = {
    "name": "email",
    "description": (
        "Search and read emails from the user's local mailbox. Two modes:\n\n"
        "**Search mode** — provide any combination of sender, recipient, subject, keywords, "
        "date_range, unread_only, or folder to find matching emails. **Start broad** — use just "
        "sender or 1-2 keywords. Don't over-specify with many fields at once. Sender/recipient "
        "name searches automatically consider alternate spellings. Returns a list with date, "
        "sender, subject, flags, and file path for each match (up to 10).\n\n"
        "**Read mode** — provide read_path (a file path from search results) to read the full "
        "email content including headers and body.\n\n"
        "Typical flow: search first to find the right email, then read it by path.\n\n"
        "Date range examples: \"today\", \"1d\" (last day), \"3d\", \"1w\" (last week), \"2w\", \"1m\" (last month), "
        "\"2025-01-01..2025-02-01\"."
    ),
    "parameters": {
        "type": "object",
        "properties": {
            "sender": {
                "type": "string",
                "description": "Who the email is from — partial name or address (e.g. 'john', 'acme')",
            },
            "recipient": {
                "type": "string",
                "description": "Who the email is to — partial name or address",
            },
            "subject": {
                "type": "string",
                "description": "Words expected in the subject line",
            },
            "keywords": {
                "type": "string",
                "description": "Words expected anywhere in the email",
            },
            "date_range": {
                "type": "string",
                "description": "Time range — e.g. 'today', '1w', '3d', '1m', '2025-01-01..2025-02-01'",
            },
            "unread_only": {
                "type": "boolean",
                "description": "Only show unread emails",
            },
            "folder": {
                "type": "string",
                "enum": ["inbox", "sent", "drafts", "trash", "archive"],
                "description": "Limit search to a specific folder",
            },
            "read_path": {
                "type": "string",
                "description": "Absolute path to an email file (from search results) to read in full",
            },
        },
    },
}

FOLDER_MAP = {
    "inbox": "/gmail/Inbox",
    "sent": "/gmail/Sent",
    "drafts": "/gmail/Drafts",
    "trash": "/gmail/Trash",
    "archive": "/gmail/Archive",
}

MAX_BODY_CHARS = 4000
MAX_RESULTS = 10


def _fuzzy_field(field, value):
    """Build fuzzy mu query clauses for a field from a natural language value.

    For person-name fields (from/to), each token is expanded with spelling
    variants (e.g. Stephanie → Stefanie) so STT-transcribed names match
    emails stored under alternative spellings.
    """
    tokens = value.split()
    is_person_field = field in ("from", "to")
    clauses = []
    for token in tokens:
        clean = re.sub(r"[^\w@.\-]", "", token.lower())
        if not clean:
            continue
        if is_person_field and "@" not in clean:
            # Expand name token into spelling variants
            variants = get_variants(clean)
            if len(variants) > 1:
                or_parts = [f"{field}:{v}*" for v in variants]
                clauses.append(f"({' OR '.join(or_parts)})")
            else:
                clauses.append(f"{field}:{clean}*")
        else:
            clauses.append(f"{field}:{clean}*")
    return clauses


def _build_query(params):
    """Build a mu find query string from semantic parameters."""
    parts = []

    if params.get("sender"):
        parts.extend(_fuzzy_field("from", params["sender"]))
    if params.get("recipient"):
        parts.extend(_fuzzy_field("to", params["recipient"]))
    if params.get("subject"):
        parts.extend(_fuzzy_field("subject", params["subject"]))
    if params.get("keywords"):
        # Body/general search — no field prefix, OR'd (any match is useful)
        kw_parts = []
        for token in params["keywords"].split():
            clean = re.sub(r"[^\w@.\-]", "", token.lower())
            if clean:
                kw_parts.append(f"{clean}*")
        if kw_parts:
            if len(kw_parts) == 1:
                parts.append(kw_parts[0])
            else:
                parts.append(f"({' OR '.join(kw_parts)})")
    if params.get("date_range"):
        dr = params["date_range"].strip()
        if ".." in dr:
            parts.append(f"date:{dr}")
        elif dr == "today":
            parts.append("date:today")
        else:
            parts.append(f"date:{dr}..")
    if params.get("unread_only"):
        parts.append("flag:unread")
    if params.get("folder"):
        maildir = FOLDER_MAP.get(params["folder"])
        if maildir:
            parts.append(f"maildir:{maildir}")

    return " AND ".join(parts)


def _search(query):
    """Run mu find and return formatted results."""
    cmd = [
        "mu", "find", query,
        "--format=json",
        "--maxnum", str(MAX_RESULTS),
        "--skip-dups",
        "--sortfield=date",
        "--reverse",
    ]
    result = subprocess.run(cmd, capture_output=True, text=True, timeout=15)

    if result.returncode != 0:
        stderr = result.stderr.strip()
        if "no matches" in stderr.lower() or not stderr:
            return "No emails found matching your search."
        return f"Search error: {stderr}"

    stdout = result.stdout.strip()
    if not stdout:
        return "No emails found matching your search."

    try:
        messages = json.loads(stdout)
    except json.JSONDecodeError:
        return "No emails found matching your search."

    if not messages:
        return "No emails found matching your search."

    lines = [f"Found {len(messages)} email(s):\n"]
    for i, msg in enumerate(messages, 1):
        from_list = msg.get(":from", [])
        from_str = from_list[0].get(":name", from_list[0].get(":email", "?")) if from_list else "?"
        from_email = from_list[0].get(":email", "") if from_list else ""
        if from_str != from_email and from_email:
            from_str = f"{from_str} <{from_email}>"

        subject = msg.get(":subject", "(no subject)")
        flags = ", ".join(msg.get(":flags", []))
        path = msg.get(":path", "")
        maildir = msg.get(":maildir", "")

        # Format date from unix timestamp
        date_unix = msg.get(":date-unix")
        if date_unix:
            from datetime import datetime
            try:
                date_str = datetime.fromtimestamp(date_unix).strftime("%Y-%m-%d %H:%M")
            except (OSError, ValueError):
                date_str = "?"
        else:
            date_str = "?"

        lines.append(f"{i}. [{date_str}] {from_str}")
        lines.append(f"   Subject: {subject}")
        lines.append(f"   Folder: {maildir}  Flags: {flags}")
        lines.append(f"   Path: {path}")
        lines.append("")

    return "\n".join(lines)


def _strip_html(html):
    """Crude HTML to text conversion."""
    text = re.sub(r"<style[^>]*>.*?</style>", "", html, flags=re.DOTALL | re.IGNORECASE)
    text = re.sub(r"<script[^>]*>.*?</script>", "", text, flags=re.DOTALL | re.IGNORECASE)
    text = re.sub(r"<br\s*/?>", "\n", text, flags=re.IGNORECASE)
    text = re.sub(r"</(p|div|tr|li|h[1-6])>", "\n", text, flags=re.IGNORECASE)
    text = re.sub(r"<[^>]+>", "", text)
    text = re.sub(r"&nbsp;", " ", text)
    text = re.sub(r"&amp;", "&", text)
    text = re.sub(r"&lt;", "<", text)
    text = re.sub(r"&gt;", ">", text)
    text = re.sub(r"&#\d+;", "", text)
    text = re.sub(r"\n{3,}", "\n\n", text)
    return text.strip()


def _read_email(path):
    """Read and format a single email file."""
    p = Path(path)
    if not p.is_file():
        return f"Email file not found: {path}"

    with open(p, "rb") as f:
        msg = BytesParser(policy=policy.default).parse(f)

    # Headers
    headers = []
    for hdr in ("From", "To", "Cc", "Date", "Subject"):
        val = msg.get(hdr)
        if val:
            headers.append(f"{hdr}: {val}")

    # Body extraction — prefer text/plain, fall back to text/html
    body = None
    html_body = None

    if msg.is_multipart():
        for part in msg.walk():
            ct = part.get_content_type()
            if ct == "text/plain" and body is None:
                payload = part.get_content()
                if payload:
                    body = payload
            elif ct == "text/html" and html_body is None:
                payload = part.get_content()
                if payload:
                    html_body = payload
    else:
        ct = msg.get_content_type()
        if ct == "text/plain":
            body = msg.get_content()
        elif ct == "text/html":
            html_body = msg.get_content()

    if body is None and html_body is not None:
        body = _strip_html(html_body)

    if body is None:
        body = "(no readable body)"

    # Truncate
    if len(body) > MAX_BODY_CHARS:
        body = body[:MAX_BODY_CHARS] + "\n\n[...truncated]"

    return "\n".join(headers) + "\n\n" + body


def run(sender: str = None, recipient: str = None, subject: str = None,
        keywords: str = None, date_range: str = None, unread_only: bool = None,
        folder: str = None, read_path: str = None) -> str:
    if read_path:
        return _read_email(read_path)

    # Build search query from semantic params
    search_params = {}
    if sender:
        search_params["sender"] = sender
    if recipient:
        search_params["recipient"] = recipient
    if subject:
        search_params["subject"] = subject
    if keywords:
        search_params["keywords"] = keywords
    if date_range:
        search_params["date_range"] = date_range
    if unread_only:
        search_params["unread_only"] = unread_only
    if folder:
        search_params["folder"] = folder

    if not search_params:
        return (
            "Please provide at least one search parameter: "
            "sender, recipient, subject, keywords, date_range, unread_only, or folder. "
            "Or provide read_path to read a specific email."
        )

    query = _build_query(search_params)
    if not query:
        return "Could not build a search query from the provided parameters."

    result = _search(query)

    # Auto-fallback: if strict AND found nothing and we have multiple params,
    # retry with progressively fewer constraints (drop keywords, then subject)
    if result.startswith("No emails found") and len(search_params) > 1:
        for drop_key in ("keywords", "subject", "date_range", "folder"):
            if drop_key in search_params:
                fallback = {k: v for k, v in search_params.items() if k != drop_key}
                fq = _build_query(fallback)
                if fq:
                    fr = _search(fq)
                    if not fr.startswith("No emails found"):
                        return fr

    return result
