"""Sentence buffer: accumulates streaming text, yields complete sentences.

Strips markdown formatting, skips code blocks and bare URLs,
and detects sentence boundaries for TTS consumption.
"""

import re

# Sentence boundary: period, question mark, or exclamation followed by
# whitespace (or end-of-string after flush).  We split on these but keep
# the punctuation attached to the sentence.
_SENTENCE_RE = re.compile(r'(?<=[.!?])\s+|(?<=\n)')

# Markdown patterns to strip
_BOLD_ITALIC = re.compile(r'\*{1,3}(.*?)\*{1,3}')
_INLINE_CODE = re.compile(r'`([^`]+)`')
_MD_IMAGE = re.compile(r'!\[.*?\]\(.*?\)')
_MD_LINK = re.compile(r'\[([^\]]+)\]\([^\)]+\)')
_BARE_URL = re.compile(r'https?://\S+')
_HEADING = re.compile(r'^#{1,6}\s+', re.MULTILINE)

# Fenced code block detection
_CODE_FENCE = re.compile(r'^```', re.MULTILINE)



class SentenceBuffer:
    """Accumulate streaming text chunks and yield complete sentences."""

    def __init__(self):
        self._buffer = ""
        self._in_code_block = False

    def add(self, text: str) -> list[str]:
        """Add a text chunk. Returns list of complete sentences (may be empty)."""
        self._buffer += text
        return self._extract_sentences()

    def flush(self) -> list[str]:
        """Return any remaining buffered text as a sentence."""
        if not self._buffer.strip():
            self._buffer = ""
            return []
        text = self._buffer.strip()
        self._buffer = ""
        text = self._clean(text)
        return [text] if text.strip() else []

    def clear(self):
        """Discard all buffered text."""
        self._buffer = ""
        self._in_code_block = False

    def _extract_sentences(self) -> list[str]:
        """Split buffer on sentence boundaries, keep remainder buffered."""
        sentences = []

        # Process code fences line by line
        lines = self._buffer.split('\n')
        clean_lines = []
        i = 0
        while i < len(lines):
            line = lines[i]
            if _CODE_FENCE.match(line.strip()):
                if self._in_code_block:
                    self._in_code_block = False
                else:
                    self._in_code_block = True
                i += 1
                continue
            if self._in_code_block:
                i += 1
                continue
            clean_lines.append(line)
            i += 1

        # Rejoin non-code text
        text = '\n'.join(clean_lines)

        # Split on sentence boundaries
        parts = _SENTENCE_RE.split(text)
        if not parts:
            self._buffer = ""
            return []

        # Last part might be incomplete — keep it buffered
        self._buffer = parts[-1] if parts else ""
        complete = parts[:-1]

        for part in complete:
            cleaned = self._clean(part.strip())
            if cleaned.strip():
                sentences.append(cleaned)

        return sentences

    def _clean(self, text: str) -> str:
        """Strip markdown formatting, URLs, image refs from text."""
        text = _MD_IMAGE.sub('', text)
        text = _MD_LINK.sub(r'\1', text)
        text = _BARE_URL.sub('', text)
        text = _BOLD_ITALIC.sub(r'\1', text)
        text = _INLINE_CODE.sub(r'\1', text)
        text = _HEADING.sub('', text)
        text = re.sub(r'  +', ' ', text)
        return text.strip()
