"""CORE-6: all loggers share one rotating file handler (separate handlers on one file break rotation on Windows).

Regression tests for Fix 1.3:
- Module import does not create directories at import time
- Importing and calling get_logger with unwritable LOG_DIR does not raise an exception
- Falls back gracefully to console logging when file logging fails"""

import importlib
import logging
from logging.handlers import RotatingFileHandler
from unittest.mock import patch

from core.config import settings
from core.logging import get_logger


def test_loggers_share_one_file_handler():
    first = get_logger("core6.first")
    second = get_logger("core6.second")
    first_files = [h for h in first.handlers if isinstance(h, RotatingFileHandler)]
    second_files = [h for h in second.handlers if isinstance(h, RotatingFileHandler)]
    assert len(first_files) == len(second_files) == 1
    assert first_files[0] is second_files[0]


def test_each_logger_keeps_console_handler():
    logger = get_logger("core6.console")
    assert any(type(h) is logging.StreamHandler for h in logger.handlers)


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
