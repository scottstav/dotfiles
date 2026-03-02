"""Add or update fields on contacts stored as vCard files."""

import re
import shutil
from datetime import datetime
from pathlib import Path

TOOL_SPEC = {
    "name": "edit_contact",
    "description": (
        "Add or update a field on a contact, or create a new contact.\n\n"
        "IMPORTANT SAFETY RULES:\n"
        "- This tool ONLY adds or updates fields. It CANNOT delete contacts or remove information.\n"
        "- Always search for the contact first with the 'contacts' tool to verify the correct match.\n"
        "- Only modify ONE contact per tool call.\n\n"
        "Supported fields: birthday, phone, email, note, organization, title, nickname, url, address.\n"
        "Birthday accepts: 'YYYY-MM-DD', 'MM-DD', 'Month Day', 'Month Day, Year' "
        "(e.g. 'December 16' or 'December 16, 1990').\n"
        "Changes sync to Google Contacts automatically within 15 minutes."
    ),
    "parameters": {
        "type": "object",
        "properties": {
            "contact_name": {
                "type": "string",
                "description": "Name of the contact to edit (must match an existing contact), or the full name for a new contact if create_new is true",
            },
            "field": {
                "type": "string",
                "enum": [
                    "birthday",
                    "phone",
                    "email",
                    "note",
                    "organization",
                    "title",
                    "nickname",
                    "url",
                    "address",
                ],
                "description": "The field to add or update",
            },
            "value": {
                "type": "string",
                "description": "The value to set",
            },
            "label": {
                "type": "string",
                "description": "Optional type label for phone/email/address (e.g. 'mobile', 'work', 'home')",
            },
            "create_new": {
                "type": "boolean",
                "description": "Set true to create a new contact if no match found. Default false.",
            },
        },
        "required": ["contact_name", "field", "value"],
    },
}

CONTACTS_DIR = Path.home() / ".local" / "share" / "contacts"
BACKUPS_DIR = CONTACTS_DIR / ".backups"

FIELD_MAP = {
    "birthday": "BDAY",
    "phone": "TEL",
    "email": "EMAIL",
    "note": "NOTE",
    "organization": "ORG",
    "title": "TITLE",
    "nickname": "NICKNAME",
    "url": "URL",
    "address": "ADR",
}

# Single-value: replace existing entry
SINGLE_VALUE_FIELDS = {"BDAY", "ORG", "TITLE", "NICKNAME"}
# Multi-value: append new entry (skip if duplicate)
# TEL, EMAIL, ADR, URL, NOTE


MONTH_NAMES = {
    "january": 1, "february": 2, "march": 3, "april": 4,
    "may": 5, "june": 6, "july": 7, "august": 8,
    "september": 9, "october": 10, "november": 11, "december": 12,
}


def _parse_birthday(value):
    """Parse a birthday string into vCard BDAY format (YYYY-MM-DD or --MM-DD)."""
    value = value.strip()
    if re.match(r"^\d{4}-\d{2}-\d{2}$", value):
        return value
    if re.match(r"^\d{2}-\d{2}$", value):
        return f"--{value}"
    # "December 16", "december 16th", "December 16, 1990"
    m = re.match(
        r"(\w+)\s+(\d{1,2})(?:st|nd|rd|th)?(?:\s*,?\s*(\d{4}))?$", value, re.I
    )
    if m:
        month_str = m.group(1).lower()
        day = int(m.group(2))
        year = m.group(3)
        if month_str in MONTH_NAMES:
            month = MONTH_NAMES[month_str]
            if year:
                return f"{int(year):04d}-{month:02d}-{day:02d}"
            return f"--{month:02d}-{day:02d}"
    # MM/DD or MM/DD/YYYY
    m = re.match(r"(\d{1,2})/(\d{1,2})(?:/(\d{4}))?$", value)
    if m:
        month, day = int(m.group(1)), int(m.group(2))
        year = m.group(3)
        if year:
            return f"{int(year):04d}-{month:02d}-{day:02d}"
        return f"--{month:02d}-{day:02d}"
    return None


def _find_contact(name_query):
    """Find a contact by name. Returns (path, display_name, alternatives)."""
    if not CONTACTS_DIR.is_dir():
        return None, "", []

    query_lower = name_query.lower()
    query_terms = query_lower.split()
    candidates = []

    for vcf_path in CONTACTS_DIR.glob("*.vcf"):
        try:
            text = vcf_path.read_text(errors="replace")
        except OSError:
            continue

        fn = ""
        for line in text.splitlines():
            if line.startswith("FN:"):
                fn = line[3:].strip()
                break
        if not fn:
            continue

        fn_lower = fn.lower()

        # Exact match — return immediately
        if fn_lower == query_lower:
            return vcf_path, fn, []

        # All query terms present as substrings
        if all(term in fn_lower for term in query_terms):
            candidates.append((vcf_path, fn))

    if not candidates:
        return None, "", []
    if len(candidates) == 1:
        return candidates[0][0], candidates[0][1], []

    # Multiple matches: pick shortest name (most specific), list others
    candidates.sort(key=lambda c: len(c[1]))
    best = candidates[0]
    alternatives = [c[1] for c in candidates[1:5]]
    return best[0], best[1], alternatives


def _backup_file(path):
    """Create a timestamped backup before editing."""
    BACKUPS_DIR.mkdir(parents=True, exist_ok=True)
    ts = datetime.now().strftime("%Y%m%d_%H%M%S")
    dst = BACKUPS_DIR / f"{path.stem}_{ts}.vcf"
    shutil.copy2(path, dst)


def _update_vcard(text, vcard_field, value, label=None):
    """Add or update a field in vCard text. Returns (new_text, changed)."""
    lines = text.splitlines()
    type_suffix = f";TYPE={label.upper()}" if label else ""
    new_line = f"{vcard_field}{type_suffix}:{value}"

    if vcard_field in SINGLE_VALUE_FIELDS:
        # Replace existing or insert
        replaced = False
        for i, line in enumerate(lines):
            base = line.split(";")[0].split(":")[0].upper()
            if base == vcard_field:
                lines[i] = new_line
                replaced = True
                break
        if not replaced:
            for i, line in enumerate(lines):
                if line.strip().upper() == "END:VCARD":
                    lines.insert(i, new_line)
                    break
        return "\n".join(lines), True

    # Multi-value: skip if exact duplicate exists
    for line in lines:
        if ":" in line:
            base = line.split(":")[0].split(";")[0].upper()
            val = ":".join(line.split(":")[1:]).strip()
            if base == vcard_field and val == value:
                return text, False
    # Append before END:VCARD
    for i, line in enumerate(lines):
        if line.strip().upper() == "END:VCARD":
            lines.insert(i, new_line)
            break
    return "\n".join(lines), True


def _create_vcard(display_name, vcard_field, value, label=None):
    """Create a minimal new vCard."""
    parts = display_name.strip().split()
    given = parts[0] if parts else display_name
    family = " ".join(parts[1:]) if len(parts) > 1 else ""
    type_suffix = f";TYPE={label.upper()}" if label else ""

    lines = [
        "BEGIN:VCARD",
        "VERSION:3.0",
        f"FN:{display_name}",
        f"N:{family};{given};;;",
        f"{vcard_field}{type_suffix}:{value}",
        "END:VCARD",
    ]
    return "\n".join(lines)


def run(contact_name: str, field: str, value: str, label: str = None,
        create_new: bool = False) -> str:
    vcard_field = FIELD_MAP[field]

    # Parse/normalize value
    if field == "birthday":
        parsed = _parse_birthday(value)
        if not parsed:
            return (
                f"Could not parse birthday: '{value}'. "
                f"Use YYYY-MM-DD, MM-DD, 'Month Day', or 'Month Day, Year'."
            )
        value = parsed
    elif field == "note":
        value = value.replace("\n", "\\n")
    elif field == "address":
        value = f";;{value.replace(chr(10), ' ')};;;;"

    # Find existing contact
    vcf_path, display_name, alternatives = _find_contact(contact_name)

    if vcf_path is None:
        if not create_new:
            return (
                f"No contact found matching '{contact_name}'. "
                f"Search with the 'contacts' tool first, or set create_new=true to create a new contact."
            )
        # Create new contact
        CONTACTS_DIR.mkdir(parents=True, exist_ok=True)
        safe = "".join(c if c.isalnum() or c in " -_" else "_" for c in contact_name)
        vcf_path = CONTACTS_DIR / f"{safe}_local.vcf"
        vcard_text = _create_vcard(contact_name, vcard_field, value, label)
        vcf_path.write_text(vcard_text)
        return (
            f"Created new contact '{contact_name}' with {field}: {value}\n"
            f"Will sync to Google Contacts within 15 minutes."
        )

    # Edit existing contact
    text = vcf_path.read_text(errors="replace")
    _backup_file(vcf_path)

    modified, changed = _update_vcard(text, vcard_field, value, label)

    if not changed:
        return f"'{display_name}' already has {field} set to '{value}'. No changes made."

    vcf_path.write_text(modified)

    result = f"Updated '{display_name}': set {field} to '{value}'"
    if alternatives:
        result += f"\n(Also matched: {', '.join(alternatives)})"
    result += "\nBackup saved. Will sync to Google Contacts within 15 minutes."
    return result
