"""Tests for the SpeechEndDetector module."""

import pytest
from speech_detector import SpeechEndDetector


class TestSpeechEndDetector:
    """Tests for end-of-speech detection logic."""

    def test_silence_timeout_triggers_end(self):
        """Fixed timeout triggers after elapsed time exceeds base threshold."""
        detector = SpeechEndDetector(silence_timeout=2.5, smart_silence=False)
        detector.on_speech_start()
        detector.update_transcript("hello world")

        # Below threshold: not done
        assert detector.is_done(2.0) is False
        # At threshold: not done (must exceed)
        assert detector.is_done(2.5) is True
        # Above threshold: done
        assert detector.is_done(3.0) is True

    def test_smart_silence_shortens_on_sentence_end(self):
        """Sentence-ending punctuation (. ? !) reduces timeout to 1.5s."""
        detector = SpeechEndDetector(silence_timeout=2.5, smart_silence=True)
        detector.on_speech_start()

        # Period
        detector.update_transcript("I am done.")
        assert detector.is_done(1.5) is True
        assert detector.is_done(1.0) is False

        # Question mark
        detector.update_transcript("Are you there?")
        assert detector.is_done(1.5) is True
        assert detector.is_done(1.0) is False

        # Exclamation mark
        detector.update_transcript("Wow!")
        assert detector.is_done(1.5) is True
        assert detector.is_done(1.0) is False

    def test_smart_silence_extends_mid_sentence(self):
        """Trailing prepositions/conjunctions/articles extend timeout to 3.5s."""
        detector = SpeechEndDetector(silence_timeout=2.5, smart_silence=True)
        detector.on_speech_start()

        # Preposition
        detector.update_transcript("tell me about the things in")
        assert detector.is_done(3.0) is False
        assert detector.is_done(3.5) is True

        # Conjunction
        detector.update_transcript("I want this and")
        assert detector.is_done(3.0) is False
        assert detector.is_done(3.5) is True

        # Article
        detector.update_transcript("show me the")
        assert detector.is_done(3.0) is False
        assert detector.is_done(3.5) is True

        # Verb like "search", "find", "tell", "show"
        detector.update_transcript("please search")
        assert detector.is_done(3.0) is False
        assert detector.is_done(3.5) is True

        detector.update_transcript("can you find")
        assert detector.is_done(3.0) is False
        assert detector.is_done(3.5) is True

    def test_force_send_phrase_triggers_immediately(self):
        """Force-send phrase at end of transcript is detected."""
        detector = SpeechEndDetector(
            force_send_phrases=["send it", "go ahead"]
        )
        detector.on_speech_start()

        detector.update_transcript("Tell me the weather send it")
        result = detector.check_force_send()
        assert result == "send it"

        # Case insensitive
        detector.update_transcript("Tell me the weather SEND IT")
        result = detector.check_force_send()
        assert result == "send it"

        # Different phrase
        detector.update_transcript("Do the thing go ahead")
        result = detector.check_force_send()
        assert result == "go ahead"

    def test_force_send_not_triggered_by_partial(self):
        """Partial match of a force-send phrase does NOT trigger."""
        detector = SpeechEndDetector(
            force_send_phrases=["send it"]
        )
        detector.on_speech_start()

        # "send" alone should not match "send it"
        detector.update_transcript("send me a message")
        result = detector.check_force_send()
        assert result is None

        # Phrase in the middle, not at end
        detector.update_transcript("send it now please")
        result = detector.check_force_send()
        assert result is None

        # No transcript at all
        detector.update_transcript("")
        result = detector.check_force_send()
        assert result is None

    def test_smart_silence_off_uses_fixed_timeout(self):
        """With smart_silence=False, always uses base timeout regardless of transcript."""
        detector = SpeechEndDetector(
            silence_timeout=2.5, smart_silence=False
        )
        detector.on_speech_start()

        # Even with sentence-ending punctuation, should use base timeout
        detector.update_transcript("I am done.")
        assert detector.is_done(1.5) is False  # Would be True with smart silence
        assert detector.is_done(2.5) is True

        # Even with mid-sentence word, should use base timeout
        detector.update_transcript("tell me about the")
        assert detector.is_done(2.5) is True  # Would be False with smart silence (3.5s)
        assert detector.is_done(2.0) is False

    def test_strip_force_send_phrase_from_transcript(self):
        """strip_force_phrase removes the phrase from the end of text."""
        # Basic removal
        result = SpeechEndDetector.strip_force_phrase(
            "Tell me the weather send it", "send it"
        )
        assert result == "Tell me the weather"

        # Trailing whitespace is cleaned up
        result = SpeechEndDetector.strip_force_phrase(
            "Hello world  send it  ", "send it"
        )
        assert result == "Hello world"

        # Case-insensitive removal
        result = SpeechEndDetector.strip_force_phrase(
            "Do something SEND IT", "send it"
        )
        assert result == "Do something"

        # Phrase not at end -- text unchanged
        result = SpeechEndDetector.strip_force_phrase(
            "send it now", "send it"
        )
        assert result == "send it now"
