"""Helpers shared by the per-folder conftest files."""

from pathlib import Path

import pytest


def mark_folder(items, folder: Path, marker: str) -> None:
    """Apply the pytest marker `marker` to every collected test that lives under `folder`."""
    folder = Path(folder).resolve()
    for item in items:
        if folder in Path(str(item.path)).resolve().parents:
            item.add_marker(getattr(pytest.mark, marker))
