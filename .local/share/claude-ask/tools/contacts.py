"""Search contacts stored as vCard files."""

import os
import re
from pathlib import Path

name = "contacts"

description = "Search the user's contacts by name, phone number, email, or other info. \
Returns matching contact details including phone, email, address, organization, etc."

input_schema = {
    "type": "object",
    "properties": {
        "query": {
            "type": "string",
            "description": "Search term — a name, phone number, email, or keyword to match against contacts",
        },
    },
    "required": ["query"],
}

CONTACTS_DIR = Path.home() / ".local" / "share" / "contacts"

FIELD_LABELS = {
    "FN": "Name",
    "TEL": "Phone",
    "EMAIL": "Email",
    "ADR": "Address",
    "ORG": "Organization",
    "TITLE": "Title",
    "BDAY": "Birthday",
    "NOTE": "Note",
    "URL": "URL",
    "NICKNAME": "Nickname",
}


def _parse_vcard(text):
    """Parse a vCard into a dict of label -> list of values."""
    info = {}
    for line in text.splitlines():
        if line.startswith(("BEGIN:", "END:", "VERSION:", "UID:", "X-", "PRODID:")):
            continue
        # Split on first colon to get field and value
        if ":" not in line:
            continue
        field_part, _, value = line.partition(":")
        # Extract base field name (before any ;TYPE= params)
        base_field = field_part.split(";")[0].upper()
        label = FIELD_LABELS.get(base_field)
        if not label:
            continue
        # Clean up address fields (semicolons to commas, collapse whitespace)
        if base_field == "ADR":
            value = value.replace("\\n", ", ")
            value = re.sub(r";+", ", ", value)
            value = re.sub(r"(, )+", ", ", value).strip(", ")
        if not value.strip():
            continue
        # Extract type hint
        type_hint = ""
        for part in field_part.split(";")[1:]:
            if part.upper().startswith("TYPE="):
                type_hint = part[5:].capitalize()
        display_label = f"{label} ({type_hint})" if type_hint else label
        info.setdefault(display_label, []).append(value.strip())
    return info


def _score_contact(query_terms, text):
    """Score a contact by how well it matches the query terms."""
    text_lower = text.lower()
    words = set(re.findall(r"[a-z]{2,}", text_lower))
    score = 0
    for term in query_terms:
        if term in text_lower:
            score += 10  # exact substring match
        else:
            # prefix match: require at least 3 chars of overlap
            best = 0
            for word in words:
                overlap = min(len(term), len(word))
                if overlap >= 3 and (word.startswith(term[:overlap]) or term.startswith(word[:overlap])):
                    best = max(best, overlap)
            if best:
                score += best  # longer overlap = better match
    return score


def run(input):
    query = input["query"].lower()
    query_terms = query.split()
    if not CONTACTS_DIR.is_dir():
        return f"Contacts directory not found: {CONTACTS_DIR}"

    scored = []
    for vcf_file in CONTACTS_DIR.glob("*.vcf"):
        try:
            text = vcf_file.read_text(errors="replace")
        except OSError:
            continue
        score = _score_contact(query_terms, text)
        if score > 0:
            info = _parse_vcard(text)
            if info:
                scored.append((score, info))

    if not scored:
        return f"No contacts found matching '{input['query']}'"

    scored.sort(key=lambda x: x[0], reverse=True)
    matches = [info for _, info in scored[:10]]

    results = []
    for contact in matches:
        lines = []
        for label, values in contact.items():
            for v in values:
                lines.append(f"  {label}: {v}")
        results.append("\n".join(lines))

    total = len(scored)
    header = f"Found {total} contact(s)"
    if total > 10:
        header += " (showing top 10)"
    return header + ":\n\n" + "\n\n".join(results)
