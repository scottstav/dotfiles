"""Wake word detection module.

Wraps OpenWakeWord's API into a simple class the daemon can use to
detect a wake word (e.g. "ok computer") from streaming audio chunks.
"""

import logging
from pathlib import Path

from openwakeword.model import Model

log = logging.getLogger("claude-voice")

# Audio format constants — must match the microphone stream config.
RATE = 16000
CHANNELS = 1
CHUNK = 1280  # 80ms at 16kHz


class WakeWordListener:
    """Listens for a wake word in a stream of audio chunks.

    Each call to ``detect()`` feeds one audio chunk to the underlying
    OpenWakeWord model and returns True when the wake word is detected.
    """

    def __init__(self, model_path: str, threshold: float = 0.5):
        """Initialise the listener.

        Args:
            model_path: Path to the .onnx wake word model file.
                        Supports ``~`` expansion.
            threshold:  Detection threshold (0.0 -- 1.0).  Higher values
                        reduce false positives at the cost of sensitivity.
        """
        model_path = str(Path(model_path).expanduser())
        log.info("Loading wake word model: %s (threshold=%.2f)", model_path, threshold)
        self.model = Model(wakeword_model_paths=[model_path])
        self.threshold = threshold
        self._model_names = list(self.model.models.keys())
        log.info("Wake word model loaded — watching for: %s", self._model_names)

    def detect(self, audio_chunk) -> bool:
        """Feed an int16 numpy array and check for a wake word.

        Args:
            audio_chunk: A numpy int16 array of 1280 samples (80 ms at 16 kHz).

        Returns:
            True if any model score exceeds the threshold.
        """
        predictions = self.model.predict(audio_chunk)
        for name in self._model_names:
            score = predictions.get(name, 0)
            if score > self.threshold:
                log.info("Wake word detected (%s, score=%.3f)", name, score)
                self.model.reset()
                return True
        return False

    def reset(self) -> None:
        """Reset the model's internal state."""
        self.model.reset()
