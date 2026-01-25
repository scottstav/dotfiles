#!/bin/bash
# Convert Google Keep JSON export to denote org files
#
# Usage: keep-to-denote.sh /path/to/Keep
# Requires: jq (sudo pacman -S jq)

set -euo pipefail

KEEP_DIR="${1:?Usage: keep-to-denote.sh /path/to/Keep}"
DENOTE_DIR="$HOME/Dropbox/org/denote"

if [[ ! -d "$KEEP_DIR" ]]; then
    echo "Error: Directory not found: $KEEP_DIR" >&2
    exit 1
fi

if ! command -v jq &>/dev/null; then
    echo "Error: jq is required. Install with: sudo pacman -S jq" >&2
    exit 1
fi

mkdir -p "$DENOTE_DIR"

# Track content hashes for deduplication
declare -A seen_hashes

count=0
skipped=0
dupes=0

for json in "$KEEP_DIR"/*.json; do
    [[ -f "$json" ]] || continue

    # Skip trashed notes
    if jq -e '.isTrashed == true' "$json" >/dev/null 2>&1; then
        echo "Skipping trashed: $(basename "$json")"
        ((++skipped))
        continue
    fi

    # Get timestamp (userEdited or created, in microseconds)
    USEC=$(jq -r '.userEditedTimestampUsec // .createdTimestampUsec' "$json")
    SEC=$((USEC / 1000000))

    # Generate denote timestamp and date bracket
    TIMESTAMP=$(date -d "@$SEC" +%Y%m%dT%H%M%S)
    DATE_BRACKET=$(date -d "@$SEC" +"[%Y-%m-%d %a %H:%M]")

    # Get content
    TITLE=$(jq -r '.title // empty' "$json")
    CONTENT=$(jq -r '.textContent // empty' "$json")

    # Skip empty notes
    if [[ -z "$CONTENT" && -z "$TITLE" ]]; then
        echo "Skipping empty: $(basename "$json")"
        ((++skipped))
        continue
    fi

    # Deduplicate by content hash (handles Keep's "note (1).json" duplicates)
    CONTENT_HASH=$(echo -n "${TITLE}${CONTENT}" | md5sum | cut -d' ' -f1)
    if [[ -n "${seen_hashes[$CONTENT_HASH]:-}" ]]; then
        echo "Skipping duplicate: $(basename "$json") (same as ${seen_hashes[$CONTENT_HASH]})"
        ((++dupes))
        continue
    fi
    seen_hashes[$CONTENT_HASH]=$(basename "$json")

    # Generate filename
    FILENAME="${TIMESTAMP}--quick-note__inbox.org"
    FILEPATH="${DENOTE_DIR}/${FILENAME}"

    # Handle duplicate timestamps (add suffix)
    if [[ -f "$FILEPATH" ]]; then
        i=1
        while [[ -f "${DENOTE_DIR}/${TIMESTAMP}-${i}--quick-note__inbox.org" ]]; do
            ((++i))
        done
        FILENAME="${TIMESTAMP}-${i}--quick-note__inbox.org"
        FILEPATH="${DENOTE_DIR}/${FILENAME}"
    fi

    # Create denote file
    {
        echo "#+title:      quick note"
        echo "#+date:       $DATE_BRACKET"
        echo "#+filetags:   :inbox:"
        echo "#+identifier: $TIMESTAMP"
        echo ""
        [[ -n "$TITLE" ]] && echo "* $TITLE" && echo ""
        echo "$CONTENT"
    } > "$FILEPATH"

    echo "Created: $FILENAME"
    ((++count))
done

echo ""
echo "Migration complete!"
echo "  Created:    $count notes"
echo "  Duplicates: $dupes notes"
echo "  Skipped:    $skipped notes (trashed/empty)"
echo "  Output:     $DENOTE_DIR"
