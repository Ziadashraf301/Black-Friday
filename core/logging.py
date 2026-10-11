import os
import sys
import logging
from logging.handlers import RotatingFileHandler
from typing import Optional
from core.config import settings

LOG_FILE_PATH = os.path.join(settings.LOG_DIR, settings.LOG_FILE)


def _ensure_log_dir() -> Optional[str]:
    """Lazily creates the log directory with graceful error handling."""
    try:
        os.makedirs(settings.LOG_DIR, exist_ok=True)
        return os.path.join(settings.LOG_DIR, settings.LOG_FILE)
    except Exception:
        return None


_file_handler: Optional[RotatingFileHandler] = None


def _get_file_handler(formatter: logging.Formatter, log_level: int) -> Optional[RotatingFileHandler]:
    """Returns the process-wide rotating file handler, creating it on first use.

    One handler is shared by every logger: separate handlers on the same file fail to rotate on Windows
    and interleave writes. Returns None (console-only logging) when the file cannot be opened.
    """
    global _file_handler
    if _file_handler is None:
        try:
            log_path = _ensure_log_dir()
            if log_path:
                handler = RotatingFileHandler(
                    log_path,
                    maxBytes=15 * 1024 * 1024,  # 15 MB per file
                    backupCount=5,
                    encoding="utf-8"
                )
                handler.setLevel(log_level)
                handler.setFormatter(formatter)
                _file_handler = handler
        except Exception:
            # Fall back to console only if file permission is restricted
            pass
    return _file_handler


def get_logger(name: str) -> logging.Logger:
    """Configures and returns a structured logger instance with both console and rotating file output."""
    logger = logging.getLogger(name)

    if not logger.handlers:
        log_level = getattr(logging, settings.LOG_LEVEL.upper(), logging.INFO)
        logger.setLevel(log_level)

        log_format = "%(asctime)s | %(levelname)-7s | %(name)s:%(funcName)s:%(lineno)d - %(message)s"
        date_format = "%Y-%m-%d %H:%M:%S"
        formatter = logging.Formatter(fmt=log_format, datefmt=date_format)

        # 1. Console Stream Handler (stdout)
        console_handler = logging.StreamHandler(sys.stdout)
        console_handler.setLevel(log_level)
        console_handler.setFormatter(formatter)
        logger.addHandler(console_handler)

        # 2. Rotating File Handler (persists full execution logs to logs/app.log)
        file_handler = _get_file_handler(formatter, log_level)
        if file_handler is not None:
            logger.addHandler(file_handler)

        logger.propagate = False

    return logger
