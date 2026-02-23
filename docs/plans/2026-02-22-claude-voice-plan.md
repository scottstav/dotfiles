# Claude Voice Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add always-on wake word detection and voice input to claude-ask, so the user can say "OK Computer", speak a query, and get Claude's response as a notification — with a Mic button for voice replies.

**Architecture:** Separate `claude-voice` daemon handles wake word (OpenWakeWord), audio capture, VAD, and STT (faster-whisper). Sends transcribed text to the existing `claude-ask` daemon over its Unix socket. `claude-ask` adds a Mic notification action that signals `claude-voice` to start listening for a reply.

**Tech Stack:** Python 3, OpenWakeWord, faster-whisper, PyAudio, WebRTC VAD, asyncio, Unix sockets, notify-send

---

### Task 1: Project Setup

**Files:**
- Create: `~/.local/share/claude-voice/requirements.txt`
- Create: `~/.config/claude-voice/config.yaml`

**Step 1: Create project directory and venv**

```bash
mkdir -p ~/.local/share/claude-voice/models
cd ~/.local/share/claude-voice
python3 -m venv .venv
```

**Step 2: Write requirements.txt**

Create `~/.local/share/claude-voice/requirements.txt`:
```
openwakeword
faster-whisper
pyaudio
webrtcvad
numpy
pyyaml
```

**Step 3: Install dependencies**

```bash
cd ~/.local/share/claude-voice
.venv/bin/pip install -r requirements.txt
```

**Step 4: Download wake word model**

Download the `ok_computer` model from the Home Assistant Wakewords Collection:
```bash
cd ~/.local/share/claude-voice/models
# Find the ok_computer or computer model from:
# https://github.com/fwartner/home-assistant-wakewords-collection
# Download the .tflite file
```

If `ok_computer` isn't available as tflite, use one of OpenWakeWord's built-in models temporarily (e.g. `hey_jarvis`) and note it for later replacement. The code should work with any model file.

**Step 5: Write config file**

Create `~/.config/claude-voice/config.yaml`:
```yaml
wake_word:
  model_path: ~/.local/share/claude-voice/models/ok_computer.tflite
  threshold: 0.5
  pre_roll_seconds: 0.5

speech:
  smart_silence: true
  silence_timeout: 2.5
  force_send_phrases: ["send it", "that's it"]
```

**Step 6: Commit**

```bash
cd ~/dotfiles
git add .local/share/claude-voice/requirements.txt .config/claude-voice/config.yaml
git commit -m "feat(claude-voice): add project skeleton and config"
```

---

### Task 2: Config Loading Module

**Files:**
- Create: `~/.local/share/claude-voice/config.py`
- Create: `~/.local/share/claude-voice/tests/test_config.py`

**Step 1: Write the failing test**

Create `~/.local/share/claude-voice/tests/__init__.py` (empty) and `tests/test_config.py`:

```python
"""Tests for config loading."""
import tempfile
import os
from pathlib import Path

import pytest


def test_load_default_config():
    """Loading config with no file returns sensible defaults."""
    from config import load_config
    cfg = load_config("/nonexistent/path.yaml")
    assert cfg["wake_word"]["threshold"] == 0.5
    assert cfg["speech"]["smart_silence"] is True
    assert cfg["speech"]["silence_timeout"] == 2.5
    assert "send it" in cfg["speech"]["force_send_phrases"]


def test_load_config_from_file():
    """Config values from YAML override defaults."""
    from config import load_config
    with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
        f.write("wake_word:\n  threshold: 0.8\nspeech:\n  silence_timeout: 4.0\n")
        f.flush()
        cfg = load_config(f.name)
    os.unlink(f.name)
    assert cfg["wake_word"]["threshold"] == 0.8
    assert cfg["speech"]["silence_timeout"] == 4.0
    # Defaults still present for unspecified keys
    assert cfg["speech"]["smart_silence"] is True


def test_load_whisper_config():
    """Reads Whisper model settings from voice-typing config."""
    from config import load_whisper_config
    with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
        f.write("model: large\ndevice: cuda\n")
        f.flush()
        wcfg = load_whisper_config(f.name)
    os.unlink(f.name)
    assert wcfg["model"] == "large"
    assert wcfg["device"] == "cuda"


def test_load_whisper_config_defaults():
    """Missing voice-typing config returns safe defaults."""
    from config import load_whisper_config
    wcfg = load_whisper_config("/nonexistent/path.yaml")
    assert wcfg["model"] == "base"
    assert wcfg["device"] == "cpu"
```

**Step 2: Run test to verify it fails**

```bash
cd ~/.local/share/claude-voice
.venv/bin/pip install pytest
.venv/bin/python -m pytest tests/test_config.py -v
```

Expected: FAIL — `ModuleNotFoundError: No module named 'config'`

**Step 3: Write the implementation**

Create `~/.local/share/claude-voice/config.py`:

```python
"""Configuration loading for claude-voice."""
from pathlib import Path

import yaml

DEFAULTS = {
    "wake_word": {
        "model_path": "~/.local/share/claude-voice/models/ok_computer.tflite",
        "threshold": 0.5,
        "pre_roll_seconds": 0.5,
    },
    "speech": {
        "smart_silence": True,
        "silence_timeout": 2.5,
        "force_send_phrases": ["send it", "that's it"],
    },
}

WHISPER_DEFAULTS = {
    "model": "base",
    "device": "cpu",
}

VOICE_TYPING_CONFIG = Path.home() / ".config" / "voice-typing" / "config.yaml"
CLAUDE_VOICE_CONFIG = Path.home() / ".config" / "claude-voice" / "config.yaml"


def _deep_merge(base, override):
    """Merge override into base dict, recursively for nested dicts."""
    result = dict(base)
    for key, val in override.items():
        if key in result and isinstance(result[key], dict) and isinstance(val, dict):
            result[key] = _deep_merge(result[key], val)
        else:
            result[key] = val
    return result


def load_config(path=None):
    """Load claude-voice config from YAML, falling back to defaults."""
    path = path or str(CLAUDE_VOICE_CONFIG)
    config = dict(DEFAULTS)
    # Deep copy nested dicts
    config = {k: dict(v) if isinstance(v, dict) else v for k, v in config.items()}

    try:
        with open(path) as f:
            user_cfg = yaml.safe_load(f) or {}
        config = _deep_merge(DEFAULTS, user_cfg)
    except FileNotFoundError:
        pass

    return config


def load_whisper_config(path=None):
    """Load Whisper model settings from voice-typing config."""
    path = path or str(VOICE_TYPING_CONFIG)
    result = dict(WHISPER_DEFAULTS)

    try:
        with open(path) as f:
            vt_cfg = yaml.safe_load(f) or {}
        if "model" in vt_cfg:
            result["model"] = vt_cfg["model"]
        if "device" in vt_cfg:
            result["device"] = vt_cfg["device"]
    except FileNotFoundError:
        pass

    return result
```

**Step 4: Run tests to verify they pass**

```bash
cd ~/.local/share/claude-voice
.venv/bin/python -m pytest tests/test_config.py -v
```

Expected: 4 tests PASS

**Step 5: Commit**

```bash
cd ~/dotfiles
git add .local/share/claude-voice/config.py .local/share/claude-voice/tests/
git commit -m "feat(claude-voice): add config loading with voice-typing integration"
```

---

### Task 3: End-of-Speech Detection Module

This is the core logic for determining when the user is done talking. Pure logic, highly testable.

**Files:**
- Create: `~/.local/share/claude-voice/speech_detector.py`
- Create: `~/.local/share/claude-voice/tests/test_speech_detector.py`

**Step 1: Write the failing tests**

Create `~/.local/share/claude-voice/tests/test_speech_detector.py`:

```python
"""Tests for end-of-speech detection logic."""
import pytest


def test_silence_timeout_triggers_end():
    """After silence_timeout seconds of silence, speech is done."""
    from speech_detector import SpeechEndDetector
    d = SpeechEndDetector(silence_timeout=2.5, smart_silence=False)
    d.on_speech_start()
    d.on_voice_activity(is_speech=True)
    d.on_voice_activity(is_speech=False)
    assert not d.is_done(elapsed_silence=2.0)
    assert d.is_done(elapsed_silence=3.0)


def test_smart_silence_shortens_on_sentence_end():
    """If transcript ends with punctuation, reduce timeout to 1.5s."""
    from speech_detector import SpeechEndDetector
    d = SpeechEndDetector(silence_timeout=2.5, smart_silence=True)
    d.on_speech_start()
    d.on_voice_activity(is_speech=True)
    d.update_transcript("What time is it?")
    d.on_voice_activity(is_speech=False)
    # Should trigger at 1.5s, not 2.5s
    assert not d.is_done(elapsed_silence=1.0)
    assert d.is_done(elapsed_silence=1.6)


def test_smart_silence_extends_mid_sentence():
    """If transcript looks mid-sentence, extend timeout to 3.5s."""
    from speech_detector import SpeechEndDetector
    d = SpeechEndDetector(silence_timeout=2.5, smart_silence=True)
    d.on_speech_start()
    d.on_voice_activity(is_speech=True)
    d.update_transcript("I want to search for")
    d.on_voice_activity(is_speech=False)
    # Mid-sentence: should NOT trigger at 2.5s
    assert not d.is_done(elapsed_silence=3.0)
    # Should trigger at 3.5s
    assert d.is_done(elapsed_silence=3.6)


def test_force_send_phrase_triggers_immediately():
    """Transcript containing a force-send phrase triggers end immediately."""
    from speech_detector import SpeechEndDetector
    d = SpeechEndDetector(
        silence_timeout=2.5, smart_silence=True,
        force_send_phrases=["send it", "that's it"],
    )
    d.on_speech_start()
    d.on_voice_activity(is_speech=True)
    d.update_transcript("tell me the weather send it")
    assert d.check_force_send() == "send it"


def test_force_send_not_triggered_by_partial():
    """'send' alone does not trigger force-send."""
    from speech_detector import SpeechEndDetector
    d = SpeechEndDetector(
        silence_timeout=2.5, smart_silence=True,
        force_send_phrases=["send it"],
    )
    d.on_speech_start()
    d.update_transcript("send me a message")
    assert d.check_force_send() is None


def test_smart_silence_off_uses_fixed_timeout():
    """With smart_silence=False, always uses the fixed timeout regardless of transcript."""
    from speech_detector import SpeechEndDetector
    d = SpeechEndDetector(silence_timeout=3.0, smart_silence=False)
    d.on_speech_start()
    d.on_voice_activity(is_speech=True)
    d.update_transcript("What time is it?")  # Sentence-ending punctuation
    d.on_voice_activity(is_speech=False)
    # Should NOT trigger early — smart silence is off
    assert not d.is_done(elapsed_silence=1.6)
    assert d.is_done(elapsed_silence=3.1)


def test_strip_force_send_phrase_from_transcript():
    """Force-send phrases should be removable from the final transcript."""
    from speech_detector import SpeechEndDetector
    d = SpeechEndDetector(force_send_phrases=["send it", "that's it"])
    d.update_transcript("tell me the weather send it")
    phrase = d.check_force_send()
    cleaned = d.strip_force_phrase("tell me the weather send it", phrase)
    assert cleaned == "tell me the weather"
```

**Step 2: Run tests to verify they fail**

```bash
cd ~/.local/share/claude-voice
.venv/bin/python -m pytest tests/test_speech_detector.py -v
```

Expected: FAIL — `ModuleNotFoundError: No module named 'speech_detector'`

**Step 3: Write the implementation**

Create `~/.local/share/claude-voice/speech_detector.py`:

```python
"""End-of-speech detection with smart silence and force-send phrases."""
import re


# Patterns that suggest a complete sentence
SENTENCE_END_RE = re.compile(r'[.?!]\s*$')

# Patterns that suggest mid-sentence (trailing prepositions, conjunctions, etc.)
MID_SENTENCE_RE = re.compile(
    r'\b(and|but|or|for|the|a|an|to|in|on|at|of|with|is|are|was|were|'
    r'that|which|who|where|when|how|what|about|from|into|because|so|'
    r'if|then|while|as|by|this|these|those|my|your|their|our|its|'
    r'can|could|would|should|will|shall|may|might|must|do|does|did|'
    r'have|has|had|been|being|get|got|make|search|find|tell|show|'
    r'look|check|open|close|start|stop|send|give|take|let|help)\s*$',
    re.IGNORECASE,
)

# Timeouts
SENTENCE_END_TIMEOUT = 1.5
MID_SENTENCE_TIMEOUT = 3.5


class SpeechEndDetector:
    """Determines when the user is done speaking.

    Operates in two modes:
    - smart_silence=True: adjusts silence timeout based on transcript content
    - smart_silence=False: uses fixed silence_timeout
    """

    def __init__(self, silence_timeout=2.5, smart_silence=True, force_send_phrases=None):
        self.base_timeout = silence_timeout
        self.smart_silence = smart_silence
        self.force_send_phrases = [p.lower() for p in (force_send_phrases or [])]
        self._transcript = ""
        self._speech_active = False

    def on_speech_start(self):
        """Called when speech capture begins."""
        self._transcript = ""
        self._speech_active = True

    def on_voice_activity(self, is_speech):
        """Called with each VAD frame result."""
        self._speech_active = is_speech

    def update_transcript(self, text):
        """Update the current interim transcript."""
        self._transcript = text

    def _effective_timeout(self):
        """Return the silence timeout based on current transcript state."""
        if not self.smart_silence:
            return self.base_timeout

        text = self._transcript.strip()
        if not text:
            return self.base_timeout

        if SENTENCE_END_RE.search(text):
            return SENTENCE_END_TIMEOUT

        if MID_SENTENCE_RE.search(text):
            return MID_SENTENCE_TIMEOUT

        return self.base_timeout

    def is_done(self, elapsed_silence):
        """Check if silence duration exceeds the effective timeout."""
        return elapsed_silence >= self._effective_timeout()

    def check_force_send(self):
        """Check if the transcript ends with a force-send phrase.

        Returns the matched phrase, or None.
        """
        text = self._transcript.strip().lower()
        for phrase in self.force_send_phrases:
            if text.endswith(phrase):
                return phrase
        return None

    @staticmethod
    def strip_force_phrase(text, phrase):
        """Remove a force-send phrase from the end of transcript text."""
        if phrase and text.lower().rstrip().endswith(phrase.lower()):
            return text[:len(text.rstrip()) - len(phrase)].rstrip()
        return text
```

**Step 4: Run tests to verify they pass**

```bash
cd ~/.local/share/claude-voice
.venv/bin/python -m pytest tests/test_speech_detector.py -v
```

Expected: 7 tests PASS

**Step 5: Commit**

```bash
cd ~/dotfiles
git add .local/share/claude-voice/speech_detector.py .local/share/claude-voice/tests/test_speech_detector.py
git commit -m "feat(claude-voice): add end-of-speech detection with smart silence"
```

---

### Task 4: Wake Word Listener

**Files:**
- Create: `~/.local/share/claude-voice/wake_word.py`

This wraps OpenWakeWord's API into a simple class the daemon can use. Hard to unit test (audio hardware), so we test manually.

**Step 1: Write wake_word.py**

Create `~/.local/share/claude-voice/wake_word.py`:

```python
"""Wake word detection using OpenWakeWord."""
import logging
from pathlib import Path

import numpy as np

log = logging.getLogger("claude-voice")

# Audio params matching OpenWakeWord requirements
RATE = 16000
CHANNELS = 1
CHUNK = 1280  # 80ms at 16kHz


class WakeWordListener:
    """Continuously monitors microphone for a wake word.

    Usage:
        listener = WakeWordListener(model_path, threshold=0.5)
        listener.start()
        # In audio callback:
        if listener.detect(audio_chunk):
            print("Wake word detected!")
        listener.stop()
    """

    def __init__(self, model_path, threshold=0.5):
        from openwakeword.model import Model

        model_path = str(Path(model_path).expanduser())
        log.info("Loading wake word model: %s (threshold=%.2f)", model_path, threshold)

        self.model = Model(
            wakeword_models=[model_path],
            inference_framework="tflite",
        )
        self.threshold = threshold
        self._model_names = list(self.model.models.keys())
        log.info("Wake word models loaded: %s", self._model_names)

    def detect(self, audio_chunk):
        """Feed an int16 audio chunk and return True if wake word detected.

        Args:
            audio_chunk: numpy int16 array, 1280 samples (80ms at 16kHz)

        Returns:
            True if any model score exceeds threshold
        """
        predictions = self.model.predict(audio_chunk)
        for name in self._model_names:
            score = predictions.get(name, 0)
            if score > self.threshold:
                log.info("Wake word detected: %s (score=%.3f)", name, score)
                self.model.reset()
                return True
        return False

    def reset(self):
        """Reset the model's internal state (call after detection)."""
        self.model.reset()
```

**Step 2: Manual smoke test**

```bash
cd ~/.local/share/claude-voice
.venv/bin/python -c "
from wake_word import WakeWordListener
print('Wake word module imports OK')
# If model file exists, test loading:
import os
model = os.path.expanduser('~/.local/share/claude-voice/models/ok_computer.tflite')
if os.path.exists(model):
    w = WakeWordListener(model)
    print('Model loaded successfully')
else:
    print('Model file not found yet — will test after download')
"
```

**Step 3: Commit**

```bash
cd ~/dotfiles
git add .local/share/claude-voice/wake_word.py
git commit -m "feat(claude-voice): add wake word detection wrapper"
```

---

### Task 5: Audio Pipeline (Capture + Pre-roll + VAD)

**Files:**
- Create: `~/.local/share/claude-voice/audio.py`

This module handles the PyAudio stream, pre-roll buffer, and VAD. It feeds audio to both the wake word detector and the speech capture pipeline.

**Step 1: Write audio.py**

Create `~/.local/share/claude-voice/audio.py`:

```python
"""Audio capture pipeline with pre-roll buffer and VAD."""
import collections
import logging
import struct

import numpy as np
import pyaudio
import webrtcvad

log = logging.getLogger("claude-voice")

RATE = 16000
CHANNELS = 1
FORMAT = pyaudio.paInt16
# 30ms frames for WebRTC VAD (must be 10, 20, or 30ms)
VAD_FRAME_MS = 30
VAD_FRAME_SAMPLES = int(RATE * VAD_FRAME_MS / 1000)  # 480 samples
# 80ms chunks for OpenWakeWord
OWW_CHUNK = 1280


class AudioPipeline:
    """Manages microphone input, pre-roll buffer, and VAD.

    The pipeline runs in two modes:
    1. Wake word mode: feeds 80ms chunks to the wake word detector
    2. Capture mode: records audio with VAD tracking, maintaining a pre-roll buffer

    Call `start()` to open the mic stream. Use `read_oww_chunk()` to get chunks
    for wake word detection. Call `begin_capture()` to switch to capture mode,
    then `read_vad_frame()` to get frames with VAD results.
    """

    def __init__(self, pre_roll_seconds=0.5):
        self._pa = pyaudio.PyAudio()
        self._stream = None
        self._vad = webrtcvad.Vad(2)  # aggressiveness 0-3, 2 is a good default

        # Pre-roll: circular buffer of recent audio for when wake word triggers
        pre_roll_frames = int(pre_roll_seconds * RATE / VAD_FRAME_SAMPLES)
        self._pre_roll = collections.deque(maxlen=max(pre_roll_frames, 1))

        self._capture_buffer = []  # accumulated audio during capture
        self._capturing = False

    def start(self):
        """Open the microphone stream."""
        self._stream = self._pa.open(
            format=FORMAT,
            channels=CHANNELS,
            rate=RATE,
            input=True,
            frames_per_buffer=OWW_CHUNK,
        )
        log.info("Microphone stream opened (rate=%d, chunk=%d)", RATE, OWW_CHUNK)

    def stop(self):
        """Close the microphone stream."""
        if self._stream:
            self._stream.stop_stream()
            self._stream.close()
            self._stream = None
        self._pa.terminate()

    def read_oww_chunk(self):
        """Read an 80ms chunk for wake word detection. Returns numpy int16 array.

        Also feeds frames into the pre-roll buffer for later use.
        """
        raw = self._stream.read(OWW_CHUNK, exception_on_overflow=False)
        audio = np.frombuffer(raw, dtype=np.int16)

        # Store 30ms frames into pre-roll buffer
        for i in range(0, len(audio), VAD_FRAME_SAMPLES):
            frame = audio[i:i + VAD_FRAME_SAMPLES]
            if len(frame) == VAD_FRAME_SAMPLES:
                self._pre_roll.append(frame.tobytes())

        return audio

    def begin_capture(self):
        """Switch to capture mode. Pre-roll buffer becomes start of capture."""
        self._capturing = True
        self._capture_buffer = list(self._pre_roll)
        self._pre_roll.clear()
        log.info("Capture started (pre-roll: %d frames)", len(self._capture_buffer))

    def read_vad_frame(self):
        """Read a 30ms frame and return (raw_bytes, is_speech).

        Must call begin_capture() first.
        """
        raw = self._stream.read(VAD_FRAME_SAMPLES, exception_on_overflow=False)
        is_speech = self._vad.is_speech(raw, RATE)
        self._capture_buffer.append(raw)
        return raw, is_speech

    def get_captured_audio(self):
        """Return all captured audio as a single numpy int16 array."""
        if not self._capture_buffer:
            return np.array([], dtype=np.int16)
        all_bytes = b"".join(self._capture_buffer)
        return np.frombuffer(all_bytes, dtype=np.int16)

    def end_capture(self):
        """End capture mode and return the captured audio."""
        self._capturing = False
        audio = self.get_captured_audio()
        self._capture_buffer = []
        log.info("Capture ended (%d samples, %.1fs)", len(audio), len(audio) / RATE)
        return audio
```

**Step 2: Manual smoke test**

```bash
cd ~/.local/share/claude-voice
.venv/bin/python -c "
from audio import AudioPipeline
p = AudioPipeline(pre_roll_seconds=0.5)
p.start()
print('Reading 5 chunks from mic...')
for i in range(5):
    chunk = p.read_oww_chunk()
    print(f'  Chunk {i}: {len(chunk)} samples, max={chunk.max()}, min={chunk.min()}')
p.stop()
print('Audio pipeline OK')
"
```

**Step 3: Commit**

```bash
cd ~/dotfiles
git add .local/share/claude-voice/audio.py
git commit -m "feat(claude-voice): add audio capture pipeline with pre-roll and VAD"
```

---

### Task 6: Whisper STT Module

**Files:**
- Create: `~/.local/share/claude-voice/stt.py`

Wraps faster-whisper to transcribe audio arrays. Reads model config from voice-typing's config.yaml.

**Step 1: Write stt.py**

Create `~/.local/share/claude-voice/stt.py`:

```python
"""Speech-to-text using faster-whisper, configured from voice-typing settings."""
import logging

import numpy as np

log = logging.getLogger("claude-voice")

# Lazy-loaded model singleton
_model = None
_model_config = None


def _get_model(whisper_config):
    """Load or reuse the faster-whisper model."""
    global _model, _model_config

    config_key = (whisper_config["model"], whisper_config["device"])
    if _model is not None and _model_config == config_key:
        return _model

    from faster_whisper import WhisperModel

    model_size = whisper_config["model"]
    device = whisper_config["device"]
    log.info("Loading Whisper model: %s (device=%s)", model_size, device)

    _model = WhisperModel(
        model_size,
        device=device,
        compute_type="int8" if device == "cpu" else "float16",
    )
    _model_config = config_key
    log.info("Whisper model loaded")
    return _model


def transcribe(audio, whisper_config):
    """Transcribe a numpy int16 audio array to text.

    Args:
        audio: numpy int16 array at 16kHz
        whisper_config: dict with "model" and "device" keys

    Returns:
        Transcribed text string
    """
    if len(audio) == 0:
        return ""

    model = _get_model(whisper_config)

    # faster-whisper expects float32 normalized to [-1, 1]
    audio_float = audio.astype(np.float32) / 32768.0

    segments, info = model.transcribe(
        audio_float,
        beam_size=5,
        language="en",
        vad_filter=True,
    )

    text = " ".join(segment.text.strip() for segment in segments)
    log.info("Transcribed: %s", text[:100])
    return text.strip()
```

**Step 2: Manual smoke test**

```bash
cd ~/.local/share/claude-voice
.venv/bin/python -c "
from stt import transcribe
import numpy as np
# Test with silence — should return empty string
silence = np.zeros(16000, dtype=np.int16)
result = transcribe(silence, {'model': 'base', 'device': 'cpu'})
print(f'Silence transcription: \"{result}\"')
print('STT module OK')
"
```

**Step 3: Commit**

```bash
cd ~/dotfiles
git add .local/share/claude-voice/stt.py
git commit -m "feat(claude-voice): add Whisper STT module with shared config"
```

---

### Task 7: Notification Helpers

**Files:**
- Create: `~/.local/share/claude-voice/notify.py`

Simple notification wrappers for the listening/transcription state.

**Step 1: Write notify.py**

Create `~/.local/share/claude-voice/notify.py`:

```python
"""Notification helpers for claude-voice."""
import subprocess
import logging

log = logging.getLogger("claude-voice")

TAG = "claude-voice"


def notify(text, title="Claude Voice"):
    """Show/update a notification (replaces previous with same tag)."""
    subprocess.Popen(
        [
            "notify-send", "-t", "0",
            "-h", f"string:x-canonical-private-synchronous:{TAG}",
            "-a", "Claude Voice",
            title, text,
        ],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )


def notify_listening():
    """Show 'Listening...' notification."""
    notify("Listening...")


def notify_transcription(text):
    """Update notification with live transcription text."""
    notify(text or "Listening...")


def notify_sending():
    """Show 'Sending to Claude...' notification."""
    notify("Sending to Claude...")


def notify_dismiss():
    """Dismiss the claude-voice notification."""
    # SwayNC: closing by tag isn't directly supported via notify-send,
    # but replacing with a very short timeout effectively dismisses it
    subprocess.Popen(
        [
            "notify-send", "-t", "1",
            "-h", f"string:x-canonical-private-synchronous:{TAG}",
            "-a", "Claude Voice",
            "Claude Voice", "",
        ],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
```

**Step 2: Commit**

```bash
cd ~/dotfiles
git add .local/share/claude-voice/notify.py
git commit -m "feat(claude-voice): add notification helpers"
```

---

### Task 8: Main Daemon

Wire everything together into the main event loop.

**Files:**
- Create: `~/.local/share/claude-voice/daemon.py`

**Step 1: Write daemon.py**

Create `~/.local/share/claude-voice/daemon.py`:

```python
#!/usr/bin/env python3
"""Claude Voice daemon: wake word detection + speech capture + STT.

Listens for a wake word, captures speech, transcribes it, and sends
the text to claude-ask via its Unix socket. Also accepts commands on
its own control socket (e.g. "listen" from claude-ask's Mic button).
"""
import asyncio
import json
import logging
import os
import socket
import threading
import time
from pathlib import Path

import numpy as np

from audio import AudioPipeline, RATE, VAD_FRAME_SAMPLES
from config import load_config, load_whisper_config
from notify import notify_listening, notify_transcription, notify_sending, notify_dismiss
from speech_detector import SpeechEndDetector
from stt import transcribe
from wake_word import WakeWordListener

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    datefmt="%H:%M:%S",
)
log = logging.getLogger("claude-voice")


def get_control_socket_path():
    runtime_dir = os.environ.get("XDG_RUNTIME_DIR", f"/run/user/{os.getuid()}")
    return os.path.join(runtime_dir, "claude-voice.sock")


def get_claude_ask_socket_path():
    runtime_dir = os.environ.get("XDG_RUNTIME_DIR", f"/run/user/{os.getuid()}")
    return os.path.join(runtime_dir, "claude-ask.sock")


def send_to_claude_ask(text, conversation_id=None):
    """Send transcribed text to claude-ask daemon."""
    sock_path = get_claude_ask_socket_path()
    payload = {"text": text}
    if conversation_id:
        payload["conversation_id"] = conversation_id

    try:
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        sock.connect(sock_path)
        sock.sendall(json.dumps(payload).encode("utf-8"))
        sock.close()
        log.info("Sent to claude-ask: %s", text[:80])
    except (ConnectionRefusedError, FileNotFoundError) as e:
        log.error("Could not connect to claude-ask daemon: %s", e)


class Daemon:
    def __init__(self):
        self.config = load_config()
        self.whisper_config = load_whisper_config()

        ww_cfg = self.config["wake_word"]
        self.wake_word = WakeWordListener(
            model_path=ww_cfg["model_path"],
            threshold=ww_cfg["threshold"],
        )

        self.audio = AudioPipeline(
            pre_roll_seconds=ww_cfg.get("pre_roll_seconds", 0.5),
        )

        sp_cfg = self.config["speech"]
        self.detector = SpeechEndDetector(
            silence_timeout=sp_cfg["silence_timeout"],
            smart_silence=sp_cfg["smart_silence"],
            force_send_phrases=sp_cfg.get("force_send_phrases", []),
        )

        # State
        self._listen_request = None  # Set by control socket: {"conversation_id": ...}
        self._lock = threading.Lock()

    def request_listen(self, conversation_id=None):
        """Called by control socket handler to trigger listening mode."""
        with self._lock:
            self._listen_request = {"conversation_id": conversation_id}

    def _check_listen_request(self):
        """Check and consume a pending listen request."""
        with self._lock:
            req = self._listen_request
            self._listen_request = None
            return req

    def run_wake_word_loop(self):
        """Main loop: detect wake word, then capture and transcribe speech."""
        self.audio.start()
        log.info("Wake word loop started")

        try:
            while True:
                # Check for external listen request (Mic button)
                listen_req = self._check_listen_request()
                if listen_req is not None:
                    conv_id = listen_req.get("conversation_id")
                    log.info("External listen request (conv=%s)", conv_id)
                    self._capture_and_send(conversation_id=conv_id)
                    continue

                # Read audio chunk and check for wake word
                chunk = self.audio.read_oww_chunk()
                if self.wake_word.detect(chunk):
                    self._capture_and_send()

        except KeyboardInterrupt:
            log.info("Shutting down")
        finally:
            self.audio.stop()

    def _capture_and_send(self, conversation_id=None):
        """Capture speech, transcribe, send to claude-ask."""
        log.info("Starting speech capture")
        notify_listening()

        self.audio.begin_capture()
        self.detector.on_speech_start()

        silence_start = None
        last_interim_time = 0
        interim_audio = []

        try:
            while True:
                raw, is_speech = self.audio.read_vad_frame()
                self.detector.on_voice_activity(is_speech)

                if is_speech:
                    silence_start = None
                elif silence_start is None:
                    silence_start = time.monotonic()

                # Accumulate audio for interim transcription
                interim_audio.append(raw)

                # Interim transcription every ~2 seconds
                now = time.monotonic()
                if now - last_interim_time >= 2.0 and len(interim_audio) > 0:
                    interim_np = np.frombuffer(b"".join(interim_audio), dtype=np.int16)
                    # Use the full captured audio for better context
                    full_audio = self.audio.get_captured_audio()
                    interim_text = transcribe(full_audio, self.whisper_config)
                    self.detector.update_transcript(interim_text)
                    notify_transcription(interim_text)
                    last_interim_time = now

                    # Check for force-send phrase
                    force_phrase = self.detector.check_force_send()
                    if force_phrase:
                        log.info("Force-send phrase detected: %s", force_phrase)
                        final_audio = self.audio.end_capture()
                        final_text = transcribe(final_audio, self.whisper_config)
                        final_text = self.detector.strip_force_phrase(final_text, force_phrase)
                        if final_text.strip():
                            notify_sending()
                            send_to_claude_ask(final_text.strip(), conversation_id)
                        else:
                            notify_dismiss()
                        return

                # Check silence timeout
                if silence_start is not None:
                    elapsed = time.monotonic() - silence_start
                    if self.detector.is_done(elapsed):
                        log.info("End of speech (silence=%.1fs)", elapsed)
                        break

        except Exception:
            log.exception("Error during speech capture")
            self.audio.end_capture()
            notify_dismiss()
            return

        # Final transcription
        final_audio = self.audio.end_capture()
        if len(final_audio) < RATE * 0.3:
            # Less than 0.3s of audio — probably a false wake word trigger
            log.info("Too short, ignoring")
            notify_dismiss()
            return

        notify_sending()
        final_text = transcribe(final_audio, self.whisper_config)

        if final_text.strip():
            send_to_claude_ask(final_text.strip(), conversation_id)
        else:
            log.info("Empty transcription, ignoring")
            notify_dismiss()


# ---------------------------------------------------------------------------
# Control socket server (for Mic button from claude-ask)
# ---------------------------------------------------------------------------

async def handle_control_client(daemon, reader, writer):
    """Handle a command on the control socket."""
    try:
        data = await reader.read(4096)
        if not data:
            return
        msg = json.loads(data.decode("utf-8"))
        action = msg.get("action")

        if action == "listen":
            conv_id = msg.get("conversation_id")
            log.info("Control: listen (conv=%s)", conv_id)
            daemon.request_listen(conversation_id=conv_id)
        else:
            log.warning("Control: unknown action '%s'", action)
    except Exception:
        log.exception("Error handling control client")
    finally:
        writer.close()
        await writer.wait_closed()


async def run_control_socket(daemon):
    """Run the asyncio control socket server."""
    sock_path = get_control_socket_path()
    try:
        os.unlink(sock_path)
    except FileNotFoundError:
        pass

    server = await asyncio.start_unix_server(
        lambda r, w: handle_control_client(daemon, r, w),
        path=sock_path,
    )
    os.chmod(sock_path, 0o600)
    log.info("Control socket listening: %s", sock_path)

    async with server:
        await server.serve_forever()


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

def main():
    daemon = Daemon()

    # Run control socket in a background thread with its own event loop
    def control_thread():
        asyncio.run(run_control_socket(daemon))

    t = threading.Thread(target=control_thread, daemon=True)
    t.start()

    # Run the wake word loop in the main thread (blocking)
    daemon.run_wake_word_loop()


if __name__ == "__main__":
    main()
```

**Step 2: Smoke test (requires microphone)**

```bash
cd ~/.local/share/claude-voice
.venv/bin/python daemon.py
# Say "OK Computer" (or whatever wake word model you have)
# Speak a sentence, wait for silence detection
# Check that it sends to claude-ask (claude-ask daemon must be running)
# Ctrl+C to stop
```

**Step 3: Commit**

```bash
cd ~/dotfiles
git add .local/share/claude-voice/daemon.py
git commit -m "feat(claude-voice): add main daemon with wake word + capture + STT loop"
```

---

### Task 9: Add Mic Button to claude-ask

**Files:**
- Modify: `~/.local/share/claude-ask/daemon.py:241-289` (the `_final_notify` method)

This is the only change to the existing claude-ask codebase.

**Step 1: Read current _final_notify method**

The method at lines 241-289 of `~/.local/share/claude-ask/daemon.py` currently shows "Reply" and "Open in Emacs" actions. We add a third "Mic" action.

**Step 2: Modify _final_notify to add Mic action**

In `~/.local/share/claude-ask/daemon.py`, update the `_final_notify` method:

Change the notify-send command (around line 252-264) to add the Mic action:

```python
                result = subprocess.run(
                    [
                        "notify-send", "-t", "0",
                        "-h", f"string:x-canonical-private-synchronous:{tag}",
                        "-a", "Claude",
                        "-A", "reply=Reply",
                        "-A", "mic=Mic",
                        "-A", "open=Open in Emacs",
                        "--wait",
                        "Claude", truncated,
                    ],
                    capture_output=True,
                    text=True,
                )
```

And add the handler for the "mic" action after the existing "reply" handler (around line 266):

```python
                action = result.stdout.strip()
                if action == "reply":
                    subprocess.Popen(["claude-ask", "--reply", conv_id])
                elif action == "mic":
                    # Signal claude-voice to start listening for a reply
                    self._trigger_voice_listen(conv_id)
                elif action == "open":
```

Add a new method to the Daemon class:

```python
    def _trigger_voice_listen(self, conv_id):
        """Send a listen command to claude-voice daemon."""
        runtime_dir = os.environ.get("XDG_RUNTIME_DIR", f"/run/user/{os.getuid()}")
        sock_path = os.path.join(runtime_dir, "claude-voice.sock")
        payload = json.dumps({"action": "listen", "conversation_id": conv_id})
        try:
            import socket as _socket
            sock = _socket.socket(_socket.AF_UNIX, _socket.SOCK_STREAM)
            sock.connect(sock_path)
            sock.sendall(payload.encode("utf-8"))
            sock.close()
            log.info("Triggered claude-voice listen for conv %s", conv_id[:8])
        except (ConnectionRefusedError, FileNotFoundError):
            log.warning("claude-voice not running, cannot trigger mic")
```

**Step 3: Test manually**

```bash
# Restart claude-ask daemon
systemctl --user restart claude-ask

# Send a test query
echo '{"text": "hello"}' | socat - UNIX-CONNECT:$XDG_RUNTIME_DIR/claude-ask.sock

# Wait for response notification — verify it shows Reply, Mic, and Open in Emacs buttons
```

**Step 4: Commit**

```bash
cd ~/dotfiles
git add .local/share/claude-ask/daemon.py
git commit -m "feat(claude-ask): add Mic notification action for voice replies"
```

---

### Task 10: Systemd Service

**Files:**
- Create: `~/.config/systemd/user/claude-voice.service`

**Step 1: Write the service file**

Create `~/.config/systemd/user/claude-voice.service`:

```ini
[Unit]
Description=Claude Voice daemon (wake word + STT for claude-ask)
After=claude-ask.service

[Service]
Type=simple
Environment=PYTHONUNBUFFERED=1
ExecStart=%h/.local/share/claude-voice/.venv/bin/python3 %h/.local/share/claude-voice/daemon.py
Restart=on-failure
RestartSec=5

[Install]
WantedBy=default.target
```

**Step 2: Stow and enable**

```bash
cd ~/dotfiles && stow .
systemctl --user daemon-reload
systemctl --user enable claude-voice
systemctl --user start claude-voice
```

**Step 3: Verify service is running**

```bash
systemctl --user status claude-voice
journalctl --user -u claude-voice -f
```

Expected: Service running, logs show "Wake word loop started" and "Control socket listening"

**Step 4: Commit**

```bash
cd ~/dotfiles
git add .config/systemd/user/claude-voice.service
git commit -m "feat(claude-voice): add systemd user service"
```

---

### Task 11: Integration Testing

End-to-end manual test of the full flow.

**Step 1: Verify both services running**

```bash
systemctl --user status claude-ask
systemctl --user status claude-voice
```

**Step 2: Test wake word flow**

1. Say "OK Computer" (or your wake word)
2. Verify notification appears: "Listening..."
3. Say a test query like "What time is it?"
4. Verify live transcription appears in notification
5. Stop talking — verify silence detection triggers (~2.5s)
6. Verify notification shows "Sending to Claude..."
7. Verify Claude's streaming response appears
8. Verify final notification has Reply, Mic, and Open in Emacs buttons

**Step 3: Test Mic button reply flow**

1. Click "Mic" on the notification
2. Verify notification shows "Listening..." again
3. Say a follow-up like "What about tomorrow?"
4. Verify it sends and Claude responds in the same conversation

**Step 4: Test force-send phrase**

1. Say "OK Computer"
2. Say "Tell me a joke send it"
3. Verify it sends immediately after "send it" without waiting for silence

**Step 5: Test smart silence toggle**

Edit `~/.config/claude-voice/config.yaml`, set `smart_silence: false`, restart service:
```bash
systemctl --user restart claude-voice
```
Verify fixed 2.5s timeout behavior.

**Step 6: Commit final state**

If any tweaks were needed during testing, commit them:
```bash
cd ~/dotfiles
git add -A
git commit -m "fix(claude-voice): adjustments from integration testing"
```
