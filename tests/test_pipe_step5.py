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


def test_retrain_scores_local_champion_on_the_candidate_holdout(onnx_dir, monkeypatch):
    import numpy as np
    import pandas as pd

    from ml.pipelines import retrain

    (onnx_dir / common.CHAMPION_ONNX_NAME).write_bytes(b"x")
    monkeypatch.setattr(retrain, "champion_onnx_path", lambda: str(onnx_dir / common.CHAMPION_ONNX_NAME))
    y = pd.Series([0.1, 0.2, 0.3])
    perfect = MagicMock()
    perfect.predict.return_value = y.to_numpy()
    monkeypatch.setattr(retrain, "ONNXPredictor", lambda path: perfect)

    scored = retrain._score_local_champion(pd.DataFrame({"a": [1, 2, 3]}), y, ["a"])

    assert scored["r2"] == 1.0
    assert scored["rmse"] == 0.0
    assert np.isclose(scored["mae"], 0.0)


def test_retrain_without_local_champion_keeps_registry_metrics(onnx_dir, monkeypatch):
    import pandas as pd

    from ml.pipelines import retrain

    monkeypatch.setattr(retrain, "champion_onnx_path", lambda: str(onnx_dir / "missing.onnx"))

    assert retrain._score_local_champion(pd.DataFrame({"a": [1]}), pd.Series([0.1]), ["a"]) is None


def test_retrain_ignores_unloadable_local_champion(onnx_dir, monkeypatch):
    import pandas as pd

    from ml.pipelines import retrain

    (onnx_dir / common.CHAMPION_ONNX_NAME).write_bytes(b"not an onnx file")
    monkeypatch.setattr(retrain, "champion_onnx_path", lambda: str(onnx_dir / common.CHAMPION_ONNX_NAME))

    assert retrain._score_local_champion(pd.DataFrame({"a": [1]}), pd.Series([0.1]), ["a"]) is None
