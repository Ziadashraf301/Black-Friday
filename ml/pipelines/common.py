"""Helpers shared by the ML pipelines."""

import os
import shutil

from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)

# Serving (apps/api/services/model_service.py) loads this file first, ahead of the per-architecture exports.
CHAMPION_ONNX_NAME = "champion_model.onnx"

# Cached payloads derived from the champion model: member prices and the catalog payloads that embed them.
MODEL_CACHE_PATTERNS = ("price:*", "shopper:*")


def onnx_dir() -> str:
    return os.path.join(settings.BASE_DIR, "models", "onnx")


def champion_onnx_path() -> str:
    return os.path.join(onnx_dir(), CHAMPION_ONNX_NAME)


def publish_champion_onnx(onnx_file: str) -> str:
    """Copies a promoted model's ONNX export to the file serving treats as the Production Champion.

    The copy is written next to the target and renamed into place, so a serving process never reads a half-written file.
    """
    target = champion_onnx_path()
    staging = f"{target}.tmp"
    shutil.copyfile(onnx_file, staging)
    os.replace(staging, target)
    logger.info(f"Published champion ONNX: {onnx_file} -> {target}")
    return target



def invalidate_caches(*patterns: str) -> None:
    """Deletes cached keys matching each pattern. Cache problems never fail a pipeline."""
    try:
        from core.cache import cache_manager

        for pattern in patterns:
            cache_manager.delete_pattern(pattern)
        logger.info(f"Invalidated cache entries: {', '.join(patterns)}")
    except Exception as e:
        logger.warning(f"Cache invalidation warning: {e}")
