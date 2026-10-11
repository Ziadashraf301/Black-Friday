"""Helpers shared by the ML pipelines."""

import os
import shutil

from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)

# Serving (apps/api/services/model_service.py) loads this file first, ahead of the per-architecture exports.
CHAMPION_ONNX_NAME = "champion_model.onnx"


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

