import os
import sys
import logging
from logging.handlers import RotatingFileHandler
from core.config import settings

# Ensure persistent log directory exists
os.makedirs(settings.LOG_DIR, exist_ok=True)
LOG_FILE_PATH = os.path.join(settings.LOG_DIR, settings.LOG_FILE)


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
        try:
            file_handler = RotatingFileHandler(
                LOG_FILE_PATH,
                maxBytes=15 * 1024 * 1024,  # 15 MB per file
                backupCount=5,
                encoding="utf-8"
            )
            file_handler.setLevel(log_level)
            file_handler.setFormatter(formatter)
            logger.addHandler(file_handler)
        except Exception:
            # Fall back to console only if file permission is restricted
            pass

        logger.propagate = False

    return logger
