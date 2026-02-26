"""Config loading for the claude-voice daemon.

Loads claude-voice settings from ~/.config/claude-voice/config.yaml and
Whisper model settings from ~/.config/voice-typing/config.yaml, applying
sensible defaults when files are missing or incomplete.
"""

import copy
import os

import yaml

DEFAULTS = {
    "wake_word": {
        "model_path": "~/.local/share/claude-voice/models/ok_computer.onnx",
        "threshold": 0.5,
        "pre_roll_seconds": 0.5,
    },
    "speech": {
        "smart_silence": True,
        "silence_timeout": 2.5,
        "no_speech_timeout": 3.0,
        "force_send_phrases": ["send it", "that's it"],
    },
}

WHISPER_DEFAULTS = {
    "model": "base",
    "device": "cpu",
    "input_device": None,
}

_DEFAULT_CONFIG_PATH = os.path.expanduser("~/.config/claude-voice/config.yaml")
_DEFAULT_WHISPER_PATH = os.path.expanduser("~/.config/voice-typing/config.yaml")


def _deep_merge(base: dict, override: dict) -> dict:
    """Deep merge override into base. Returns a new dict.

    Nested dicts are merged recursively. All other types (lists, scalars)
    in override replace the base value entirely.
    """
    result = base.copy()
    for key, value in override.items():
        if (
            key in result
            and isinstance(result[key], dict)
            and isinstance(value, dict)
        ):
            result[key] = _deep_merge(result[key], value)
        else:
            result[key] = value
    return result


def load_config(path: str | None = None) -> dict:
    """Load claude-voice config, merging user overrides over defaults.

    Args:
        path: Optional path to config YAML. Defaults to
              ~/.config/claude-voice/config.yaml.

    Returns:
        Merged config dict with all defaults filled in.
    """
    if path is None:
        path = _DEFAULT_CONFIG_PATH

    defaults = copy.deepcopy(DEFAULTS)

    try:
        with open(path) as f:
            user_config = yaml.safe_load(f)
    except FileNotFoundError:
        return defaults

    if not isinstance(user_config, dict):
        return defaults

    return _deep_merge(defaults, user_config)


def load_whisper_config(path: str | None = None) -> dict:
    """Load Whisper model settings from the voice-typing config.

    Extracts `model`, `device`, and `input_device` from the shared
    voice-typing config file, applying defaults for any missing keys.

    Args:
        path: Optional path to whisper config YAML. Defaults to
              ~/.config/voice-typing/config.yaml.

    Returns:
        Dict with 'model', 'device', and 'input_device' keys.
    """
    if path is None:
        path = _DEFAULT_WHISPER_PATH

    defaults = WHISPER_DEFAULTS.copy()

    try:
        with open(path) as f:
            raw = yaml.safe_load(f)
    except FileNotFoundError:
        return defaults

    if not isinstance(raw, dict):
        return defaults

    return {
        "model": raw.get("model", defaults["model"]),
        "device": raw.get("device", defaults["device"]),
        "input_device": raw.get("input_device", defaults["input_device"]),
    }
