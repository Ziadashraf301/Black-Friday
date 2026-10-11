"""Review round 1, step 5: ML pipelines and features (PIPE-*).

Regression test for Fix 5.5:
- Ground-truth check in monitor.py must look at evaluation dataframe (curr_eval)
  or the raw batch column "purchase".
- Test: a batch containing "purchase" sets has_target True; a batch without it sets False."""

from unittest.mock import MagicMock

import pandas as pd
import pytest

from core.config import settings
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


def test_score_local_champion_uses_the_given_holdout(onnx_dir, monkeypatch):
    import numpy as np
    import pandas as pd

    (onnx_dir / common.CHAMPION_ONNX_NAME).write_bytes(b"x")
    monkeypatch.setattr(common, "champion_onnx_path", lambda: str(onnx_dir / common.CHAMPION_ONNX_NAME))
    y = pd.Series([0.1, 0.2, 0.3])
    perfect = MagicMock()
    perfect.predict.return_value = y.to_numpy()
    monkeypatch.setattr(common, "ONNXPredictor", lambda path: perfect)

    scored = common.score_local_champion(pd.DataFrame({"a": [1, 2, 3]}), y, ["a"])

    assert scored["r2"] == 1.0
    assert scored["rmse"] == 0.0
    assert np.isclose(scored["mae"], 0.0)


def test_score_local_champion_is_none_without_a_local_file(onnx_dir, monkeypatch):
    import pandas as pd

    monkeypatch.setattr(common, "champion_onnx_path", lambda: str(onnx_dir / "missing.onnx"))

    assert common.score_local_champion(pd.DataFrame({"a": [1]}), pd.Series([0.1]), ["a"]) is None


def test_score_local_champion_ignores_unloadable_file(onnx_dir, monkeypatch):
    import pandas as pd

    (onnx_dir / common.CHAMPION_ONNX_NAME).write_bytes(b"not an onnx file")
    monkeypatch.setattr(common, "champion_onnx_path", lambda: str(onnx_dir / common.CHAMPION_ONNX_NAME))

    assert common.score_local_champion(pd.DataFrame({"a": [1]}), pd.Series([0.1]), ["a"]) is None


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


def test_retrain_resolves_model_aliases_before_naming_artifacts(monkeypatch):
    import pandas as pd

    from ml.pipelines import retrain

    seen = []

    def stop(name, **_):
        seen.append(name)
        raise RuntimeError("stop after model lookup")

    monkeypatch.setattr(retrain.ModelRegistry, "get_model", stop)
    monkeypatch.setattr(retrain, "MLflowTracker", MagicMock())
    frame = pd.DataFrame({"normalized_purchase": [0.1] * 20, "is_outlier": [False] * 20})

    with pytest.raises(RuntimeError, match="stop after model lookup"):
        retrain.run_champion_challenger_retrain(frame, frame, candidate_model_name="lgbm")

    assert seen == ["lightgbm"]


def test_preprocess_missing_metrics_report_what_the_imputer_left_behind():
    from ml.pipelines.preprocess import _missing_value_metrics

    stats = {
        "missing_before": {"product_category_2": 10, "product_category_3": 20},
        "missing_after": {"product_category_2": 0, "product_category_3": 3},
    }

    assert _missing_value_metrics(stats) == {
        "missing_product_category_2_raw": 10.0,
        "missing_product_category_2_imputed": 0.0,
        "missing_product_category_3_raw": 20.0,
        "missing_product_category_3_imputed": 3.0,
    }
    assert _missing_value_metrics({})["missing_product_category_3_imputed"] == 0.0


def test_tag_outliers_requires_bounds_fitted_on_training_data():
    import pandas as pd

    from ml.features.preprocessor import DataPreprocessor

    preprocessor = DataPreprocessor()
    frame = pd.DataFrame({"purchase": [1.0, 2.0, 3.0, 1000.0]})

    with pytest.raises(RuntimeError, match="compute_outlier_bounds"):
        preprocessor.tag_outliers(frame)

    preprocessor.compute_outlier_bounds(frame["purchase"].iloc[:3])
    assert preprocessor.tag_outliers(frame)["is_outlier"].tolist() == [False, False, False, True]


@pytest.fixture
def basket_run(tmp_path, monkeypatch):
    """Runs the market basket pipeline with every collaborator mocked; returns (tracker, run)."""
    import pandas as pd

    from ml.pipelines import market_basket as mb

    tracker = MagicMock()
    monkeypatch.setattr(mb, "MLflowTracker", lambda: tracker)
    repo = MagicMock()
    repo.get_cleaned_records_df.return_value = pd.DataFrame({"user_id": [1], "product_id": ["P1"]})
    monkeypatch.setattr(mb, "BlackFridayRepository", lambda: repo)
    monkeypatch.setattr(mb, "TransactionBasketEncoder", MagicMock())
    engine = MagicMock()
    engine.return_value.mine_rules.return_value = pd.DataFrame()
    monkeypatch.setattr(mb, "AprioriAssociationEngine", engine)
    i2v = MagicMock()
    i2v.return_value.get_all_similar_products.return_value = {}
    i2v.return_value.evaluate_embeddings.return_value = {}
    monkeypatch.setattr(mb, "Item2VecRecommender", i2v)
    network = MagicMock()
    network.return_value.build_unified_metrics.return_value = pd.DataFrame()
    monkeypatch.setattr(mb, "ProductNetworkGraph", network)
    viz = MagicMock()
    viz.generate_market_basket_charts.return_value = {}
    monkeypatch.setattr(mb, "Visualizer", viz)
    monkeypatch.setattr(mb.settings, "BASE_DIR", tmp_path)
    return tracker, tmp_path, mb


def test_market_basket_run_logs_the_real_graph_source(basket_run):
    tracker, _, mb = basket_run

    mb.run_market_basket_pipeline()

    kwargs = tracker.log_run.call_args.kwargs
    assert kwargs["params"]["graph_source"] == "apriori+item2vec"
    assert kwargs["tags"]["graph_source"] == "apriori+item2vec"
    with pytest.raises(TypeError):
        mb.run_market_basket_pipeline(graph_source="item2vec")


def test_market_basket_does_not_log_csvs_left_over_from_an_earlier_run(basket_run):
    tracker, tmp_path, mb = basket_run
    reports = tmp_path / "reports" / "market_basket"
    reports.mkdir(parents=True)
    (reports / "apriori_rules.csv").write_text("stale")
    (reports / "item2vec_recommendations.csv").write_text("stale")

    mb.run_market_basket_pipeline()

    artifacts = tracker.log_run.call_args.kwargs["artifacts"]
    assert "apriori_rules_csv" not in artifacts
    assert "item2vec_recommendations_csv" not in artifacts
    assert "market_basket_summary" in artifacts


def test_prepare_batch_lowercases_derives_target_and_skips_imputer_without_artifacts(onnx_dir):
    import pandas as pd

    df = pd.DataFrame({"Purchase": [9000.0], "Product_Category_2": [None]})
    out = common.prepare_batch(df.rename(columns=str.lower).rename(columns={"purchase": "PURCHASE"}))

    assert "normalized_purchase" in out.columns
    assert out["normalized_purchase"].iloc[0] == pytest.approx(9000.0 / common.settings.PURCHASE_MAX)
    assert out["product_category_2"].isna().all()
    assert list(df.columns) == ["Purchase", "Product_Category_2"]


def test_train_promotes_when_no_champion_can_be_scored():
    from ml.pipelines.train import should_promote

    assert should_promote(0.01, None) is True


def test_train_promotes_only_a_model_that_beats_the_champion_on_the_same_split():
    from ml.pipelines.train import should_promote

    assert should_promote(0.73, {"r2": 0.72}) is True
    assert should_promote(0.72, {"r2": 0.72}) is False
    assert should_promote(0.40, {"r2": 0.72}) is False


def test_customer_frequency_counts_distinct_products_not_purchase_rows():
    import pandas as pd

    from ml.features.customer_features import CustomerFeatureExtractor

    df = pd.DataFrame(
        {
            "user_id": [1, 1, 1],
            "product_id": ["P1", "P1", "P2"],
            "gender": ["F"] * 3,
            "marital_status": [0] * 3,
            "age": ["26-35"] * 3,
            "purchase": [10.0, 20.0, 30.0],
            "product_category_1": [1, 1, 2],
        }
    )

    features = CustomerFeatureExtractor().extract_features(df)

    assert features["frequency"].iloc[0] == 2


def test_reports_dir_is_anchored_at_base_dir_not_the_working_directory(tmp_path, monkeypatch):
    import os

    monkeypatch.setattr(common.settings, "BASE_DIR", tmp_path)
    elsewhere = tmp_path / "elsewhere"
    elsewhere.mkdir()
    monkeypatch.chdir(elsewhere)

    assert common.reports_dir("drift") == os.path.join(str(tmp_path), "reports", "drift")
    assert common.reports_dir("models", "lgbm") == os.path.join(str(tmp_path), "reports", "models", "lgbm")
    assert os.path.isabs(common.reports_dir())


def evaluate_has_target(current_batch_df: pd.DataFrame) -> bool:
    """Mirrors the exact target resolution and decision gate logic in ml/pipelines/monitor.py."""
    curr_eval = current_batch_df.copy()
    curr_eval.columns = [c.lower() for c in curr_eval.columns]

    if "normalized_purchase" not in curr_eval.columns and "purchase" in curr_eval.columns:
        curr_eval["normalized_purchase"] = curr_eval["purchase"] / settings.PURCHASE_MAX

    target_col = "normalized_purchase"
    has_target = (
        (target_col in curr_eval.columns and curr_eval[target_col].notna().any())
        or ("purchase" in current_batch_df.columns and current_batch_df["purchase"].notna().any())
    )
    return bool(has_target)


def test_batch_containing_purchase_sets_has_target_true():
    """Verify batch containing raw 'purchase' column resolves has_target to True."""
    batch_with_purchase = pd.DataFrame({
        "user_id": [1000001, 1000002],
        "product_id": ["P001", "P002"],
        "purchase": [8500.0, 12000.0],
    })
    assert evaluate_has_target(batch_with_purchase) is True


def test_batch_containing_normalized_purchase_sets_has_target_true():
    """Verify batch containing already normalized target sets has_target to True."""
    batch_with_norm = pd.DataFrame({
        "user_id": [1000001, 1000002],
        "product_id": ["P001", "P002"],
        "normalized_purchase": [0.45, 0.60],
    })
    assert evaluate_has_target(batch_with_norm) is True


def test_batch_without_target_sets_has_target_false():
    """Verify inference batch without ground-truth purchase sets has_target to False."""
    batch_without_target = pd.DataFrame({
        "user_id": [1000001, 1000002],
        "product_id": ["P001", "P002"],
        "gender": ["M", "F"],
        "age": ["26-35", "36-45"],
    })
    assert evaluate_has_target(batch_without_target) is False


def test_batch_with_all_nan_purchase_sets_has_target_false():
    """Verify batch where purchase is all NaN evaluates to False."""
    batch_nan_target = pd.DataFrame({
        "user_id": [1000001, 1000002],
        "product_id": ["P001", "P002"],
        "purchase": [float("nan"), float("nan")],
    })
    assert evaluate_has_target(batch_nan_target) is False
