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


def test_warehouse_replace_batches_use_a_single_replacing_load():
    from unittest.mock import patch

    import pandas as pd

    from core.db.repositories.warehouse_repo import WarehouseRepository

    repo = WarehouseRepository(engine=MagicMock())
    df = pd.DataFrame({"a": [1]})
    with patch.object(repo, "insert_dataframe") as insert:
        repo.replace_raw_batch(df)
        repo.replace_cleaned_batch(df)

    assert [c.args[0] for c in insert.call_args_list] == ["raw_black_friday", "black_friday_cleaned"]
    assert all(c.kwargs["if_exists"] == "replace" for c in insert.call_args_list)


def test_failed_replace_load_keeps_previous_rows():
    import pandas as pd
    from sqlalchemy import text

    from core.db.repositories.base import BaseRepository

    table = "tmp_pipe3_replace"
    repo = BaseRepository()
    try:
        with repo.engine.begin() as conn:
            conn.execute(text(f"DROP TABLE IF EXISTS {table}"))
            conn.execute(text(f"CREATE TABLE {table} (id INTEGER)"))
            conn.execute(text(f"INSERT INTO {table} VALUES (1), (2)"))
    except Exception as exc:
        pytest.skip(f"PostgreSQL unavailable: {exc}")
    repo.VALID_TABLES = repo.VALID_TABLES | {table}
    try:
        with pytest.raises(Exception):
            repo.insert_dataframe(table, pd.DataFrame({"no_such_column": [9]}), if_exists="replace")
        with repo.engine.connect() as conn:
            assert conn.execute(text(f"SELECT count(*) FROM {table}")).scalar() == 2
    finally:
        with repo.engine.begin() as conn:
            conn.execute(text(f"DROP TABLE IF EXISTS {table}"))


def test_invalidate_caches_deletes_each_pattern(monkeypatch):
    fake = MagicMock()
    monkeypatch.setattr("core.cache.cache_manager", fake)

    common.invalidate_caches("price:*", "shopper:*")

    assert [c.args[0] for c in fake.delete_pattern.call_args_list] == ["price:*", "shopper:*"]


def test_invalidate_caches_never_raises(monkeypatch):
    fake = MagicMock()
    fake.delete_pattern.side_effect = RuntimeError("redis down")
    monkeypatch.setattr("core.cache.cache_manager", fake)

    common.invalidate_caches("price:*")


def test_cron_cycle_flushes_price_shopper_and_analytics_caches(monkeypatch):
    from ml.pipelines import cron_scheduler

    for name in (
        "run_customer_segmentation",
        "run_market_basket_pipeline",
        "run_production_drift_monitor",
        "run_seed_curated_catalog",
        "BlackFridayRepository",
    ):
        monkeypatch.setattr(cron_scheduler, name, MagicMock())
    flushed = MagicMock()
    monkeypatch.setattr(cron_scheduler, "invalidate_caches", flushed)

    cron_scheduler.run_cron_cycle()

    flushed.assert_called_once_with("price:*", "shopper:*", "analytics:*")


def test_cron_drift_batch_excludes_outliers_and_is_a_seeded_sample():
    import pandas as pd

    from ml.pipelines import cron_scheduler

    frame = pd.DataFrame({"x": range(cron_scheduler.DRIFT_BATCH_SIZE + 500)})
    repo = MagicMock()
    repo.get_cleaned_records_df.return_value = frame

    first = cron_scheduler._load_drift_batch(repo)
    second = cron_scheduler._load_drift_batch(repo)

    repo.get_cleaned_records_df.assert_called_with(split="test", exclude_outliers=True)
    assert len(first) == cron_scheduler.DRIFT_BATCH_SIZE
    assert first.equals(second)


def test_cron_cycle_survives_repository_construction_failure(monkeypatch):
    from ml.pipelines import cron_scheduler

    for name in ("run_customer_segmentation", "run_market_basket_pipeline", "run_seed_curated_catalog"):
        monkeypatch.setattr(cron_scheduler, name, MagicMock())
    monkeypatch.setattr(cron_scheduler, "BlackFridayRepository", MagicMock(side_effect=RuntimeError("db down")))
    monkeypatch.setattr(cron_scheduler, "invalidate_caches", MagicMock())

    cron_scheduler.run_cron_cycle()

    cron_scheduler.run_seed_curated_catalog.assert_called_once()


def test_daemon_keeps_looping_after_a_failed_cycle(monkeypatch):
    from ml.pipelines import cron_scheduler

    monkeypatch.setattr(cron_scheduler, "run_cron_cycle", MagicMock(side_effect=[RuntimeError("boom"), None]))
    sleeps = []

    def fake_sleep(seconds):
        sleeps.append(seconds)
        if len(sleeps) == 2:
            raise KeyboardInterrupt

    monkeypatch.setattr(cron_scheduler.time, "sleep", fake_sleep)

    with pytest.raises(KeyboardInterrupt):
        cron_scheduler.start_daemon_scheduler()

    assert cron_scheduler.run_cron_cycle.call_count == 2
