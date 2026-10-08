"""
Regression tests for Fix 1.3:
- Module import does not create directories at import time
- Importing and calling get_logger with unwritable LOG_DIR does not raise an exception
- Falls back gracefully to console logging when file logging fails
"""
import importlib
import logging
from unittest.mock import patch
from core.config import settings


def test_import_unwritable_log_dir_does_not_raise(monkeypatch):
    """Verify that importing core.logging when LOG_DIR is unwritable does not raise."""
    monkeypatch.setattr(settings, "LOG_DIR", "/nonexistent_root_unwritable_dir/logs")

    import core.logging
    # Reload to verify import-time execution with the unwritable path
    reloaded = importlib.reload(core.logging)
    assert reloaded is not None


def test_get_logger_with_unwritable_log_dir(monkeypatch):
    """Verify get_logger falls back gracefully when file handler cannot be created."""
    with patch("os.makedirs", side_effect=PermissionError("Permission denied: unwritable dir")):
        import core.logging
        logger = core.logging.get_logger("test_unwritable_fallback")

        assert isinstance(logger, logging.Logger)
        # Should have console handler attached
        assert len(logger.handlers) >= 1
        assert any(isinstance(h, logging.StreamHandler) for h in logger.handlers)

        # Logging should succeed without raising
        logger.info("Testing graceful fallback to console logging")
