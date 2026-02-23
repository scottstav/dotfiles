"""Tests for claude-voice config loading module."""

import yaml

from config import load_config, load_whisper_config, DEFAULTS


class TestLoadConfig:
    """Tests for load_config()."""

    def test_defaults_when_no_file_exists(self, tmp_path):
        """When config file doesn't exist, all defaults are returned."""
        missing = tmp_path / "nonexistent.yaml"
        cfg = load_config(path=str(missing))

        assert cfg["wake_word"]["model_path"] == DEFAULTS["wake_word"]["model_path"]
        assert cfg["wake_word"]["threshold"] == 0.5
        assert cfg["wake_word"]["pre_roll_seconds"] == 0.5
        assert cfg["speech"]["smart_silence"] is True
        assert cfg["speech"]["silence_timeout"] == 2.5
        assert cfg["speech"]["force_send_phrases"] == ["send it", "that's it"]

    def test_user_values_override_defaults(self, tmp_path):
        """User-specified values replace the corresponding defaults."""
        config_file = tmp_path / "config.yaml"
        user_config = {
            "wake_word": {
                "threshold": 0.8,
            },
            "speech": {
                "silence_timeout": 5.0,
            },
        }
        config_file.write_text(yaml.dump(user_config))

        cfg = load_config(path=str(config_file))

        assert cfg["wake_word"]["threshold"] == 0.8
        assert cfg["speech"]["silence_timeout"] == 5.0

    def test_unspecified_keys_retain_defaults(self, tmp_path):
        """Keys not present in user config keep their default values."""
        config_file = tmp_path / "config.yaml"
        user_config = {
            "wake_word": {
                "threshold": 0.9,
            },
        }
        config_file.write_text(yaml.dump(user_config))

        cfg = load_config(path=str(config_file))

        # wake_word keys not overridden should still be defaults
        assert cfg["wake_word"]["model_path"] == DEFAULTS["wake_word"]["model_path"]
        assert cfg["wake_word"]["pre_roll_seconds"] == 0.5

        # Entire speech section should be defaults
        assert cfg["speech"]["smart_silence"] is True
        assert cfg["speech"]["silence_timeout"] == 2.5
        assert cfg["speech"]["force_send_phrases"] == ["send it", "that's it"]

    def test_deep_merge_preserves_sibling_keys(self, tmp_path):
        """Deep merge adds new keys without removing existing defaults."""
        config_file = tmp_path / "config.yaml"
        user_config = {
            "wake_word": {
                "threshold": 0.7,
                "custom_option": "hello",
            },
        }
        config_file.write_text(yaml.dump(user_config))

        cfg = load_config(path=str(config_file))

        assert cfg["wake_word"]["threshold"] == 0.7
        assert cfg["wake_word"]["custom_option"] == "hello"
        assert cfg["wake_word"]["model_path"] == DEFAULTS["wake_word"]["model_path"]

    def test_list_values_are_replaced_not_merged(self, tmp_path):
        """Lists in user config replace default lists entirely."""
        config_file = tmp_path / "config.yaml"
        user_config = {
            "speech": {
                "force_send_phrases": ["go", "done"],
            },
        }
        config_file.write_text(yaml.dump(user_config))

        cfg = load_config(path=str(config_file))

        assert cfg["speech"]["force_send_phrases"] == ["go", "done"]

    def test_empty_file_returns_defaults(self, tmp_path):
        """An empty YAML file (parses as None) returns all defaults."""
        config_file = tmp_path / "config.yaml"
        config_file.write_text("")

        cfg = load_config(path=str(config_file))

        assert cfg == DEFAULTS

    def test_returns_copy_not_reference(self, tmp_path):
        """Returned config should be a copy, not a reference to DEFAULTS."""
        missing = tmp_path / "nonexistent.yaml"
        cfg = load_config(path=str(missing))
        cfg["wake_word"]["threshold"] = 999

        assert DEFAULTS["wake_word"]["threshold"] == 0.5


class TestLoadWhisperConfig:
    """Tests for load_whisper_config()."""

    def test_defaults_when_no_file_exists(self, tmp_path):
        """When whisper config file doesn't exist, defaults are returned."""
        missing = tmp_path / "nonexistent.yaml"
        cfg = load_whisper_config(path=str(missing))

        assert cfg["model"] == "base"
        assert cfg["device"] == "cpu"

    def test_custom_model_and_device(self, tmp_path):
        """Custom model and device are loaded from the YAML file."""
        config_file = tmp_path / "config.yaml"
        whisper_config = {
            "model": "large-v3",
            "device": "cuda",
            "other_setting": "ignored",
        }
        config_file.write_text(yaml.dump(whisper_config))

        cfg = load_whisper_config(path=str(config_file))

        assert cfg["model"] == "large-v3"
        assert cfg["device"] == "cuda"

    def test_only_extracts_model_and_device(self, tmp_path):
        """Only model and device are extracted, other keys are ignored."""
        config_file = tmp_path / "config.yaml"
        whisper_config = {
            "model": "small",
            "device": "cpu",
            "input_device": "pulse",
            "commands": True,
        }
        config_file.write_text(yaml.dump(whisper_config))

        cfg = load_whisper_config(path=str(config_file))

        assert set(cfg.keys()) == {"model", "device"}

    def test_partial_config_fills_defaults(self, tmp_path):
        """If only model is specified, device gets default and vice versa."""
        config_file = tmp_path / "config.yaml"
        config_file.write_text(yaml.dump({"model": "tiny"}))

        cfg = load_whisper_config(path=str(config_file))

        assert cfg["model"] == "tiny"
        assert cfg["device"] == "cpu"

    def test_empty_file_returns_defaults(self, tmp_path):
        """An empty YAML file returns default whisper config."""
        config_file = tmp_path / "config.yaml"
        config_file.write_text("")

        cfg = load_whisper_config(path=str(config_file))

        assert cfg["model"] == "base"
        assert cfg["device"] == "cpu"
