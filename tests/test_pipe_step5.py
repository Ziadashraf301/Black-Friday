"""Review round 1, step 5: ML pipelines and features (PIPE-*)."""

from unittest.mock import MagicMock

import pytest

from ml.pipelines import common, monitor


@pytest.fixture
def onnx_dir(tmp_path, monkeypatch):
    monkeypatch.setattr(common, "onnx_dir", lambda: str(tmp_path))
    monkeypatch.setattr(monitor, "onnx_dir", lambda: str(tmp_path))
    return tmp_path


def test_publish_champion_onnx_replaces_serving_file(onnx_dir):
    (onnx_dir / "random_forest.onnx").write_bytes(b"new")
    (onnx_dir / common.CHAMPION_ONNX_NAME).write_bytes(b"old")

    target = common.publish_champion_onnx(str(onnx_dir / "random_forest.onnx"))

    assert target == str(onnx_dir / common.CHAMPION_ONNX_NAME)
    assert (onnx_dir / common.CHAMPION_ONNX_NAME).read_bytes() == b"new"
    assert not (onnx_dir / f"{common.CHAMPION_ONNX_NAME}.tmp").exists()


def test_monitor_scores_with_published_champion_before_architecture_export(onnx_dir):
    (onnx_dir / "lgbm.onnx").write_bytes(b"x")
    (onnx_dir / common.CHAMPION_ONNX_NAME).write_bytes(b"x")

    assert monitor._resolve_scoring_onnx("lgbm", MagicMock()) == str(onnx_dir / common.CHAMPION_ONNX_NAME)


def test_monitor_falls_back_to_architecture_export_without_downloading(onnx_dir):
    (onnx_dir / "lgbm.onnx").write_bytes(b"x")
    tracker = MagicMock()

    assert monitor._resolve_scoring_onnx("lgbm", tracker) == str(onnx_dir / "lgbm.onnx")
    tracker.download_champion_onnx.assert_not_called()


def test_monitor_downloads_champion_when_nothing_is_local(onnx_dir):
    tracker = MagicMock()
    tracker.download_champion_onnx.side_effect = lambda **_: (onnx_dir / common.CHAMPION_ONNX_NAME).write_bytes(b"x")

    assert monitor._resolve_scoring_onnx("lgbm", tracker) == str(onnx_dir / common.CHAMPION_ONNX_NAME)
