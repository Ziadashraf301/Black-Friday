"""CORE-6: all loggers share one rotating file handler (separate handlers on one file break rotation on Windows)."""
import logging
from logging.handlers import RotatingFileHandler

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
