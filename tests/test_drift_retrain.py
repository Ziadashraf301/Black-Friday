import pytest
import numpy as np
import pandas as pd
from unittest.mock import MagicMock, patch
from ml.tracking.drift_monitor import DriftMonitor, DriftResult
from ml.pipelines.retrain import run_champion_challenger_retrain, RetrainResult
from ml.pipelines.monitor import run_production_drift_monitor


@pytest.fixture
def sample_datasets():
    """Generates baseline reference (old) data and synthetic drifted (new) data."""
    np.random.seed(42)
    n = 200

    old_df = pd.DataFrame({
        "product_category_1": np.random.randint(1, 5, size=n),
        "product_category_2": np.random.randint(1, 10, size=n),
        "product_category_3": np.random.randint(1, 15, size=n),
        "product_id": [f"P_{i % 20}" for i in range(n)],
        "normalized_purchase": np.random.normal(loc=0.5, scale=0.1, size=n),
        "user_id": np.random.randint(1000, 2000, size=n),
        "gender": np.random.choice(["M", "F"], size=n),
        "age": np.random.choice(["26-35", "36-45"], size=n),
        "occupation": np.random.randint(0, 10, size=n),
        "city_category": np.random.choice(["A", "B", "C"], size=n),
        "stay_in_current_city_years": np.random.choice(["1", "2", "3"], size=n),
        "marital_status": np.random.choice([0, 1], size=n),
        "purchase": np.random.normal(loc=9000, scale=2000, size=n),
        "is_outlier": False
    })

    # Strongly drifted new data: shifted categories and shifted target
    new_df = pd.DataFrame({
        "product_category_1": np.random.randint(15, 20, size=n),  # completely drifted categories
        "product_category_2": np.random.randint(20, 30, size=n),
        "product_category_3": np.random.randint(25, 40, size=n),
        "product_id": [f"P_NEW_{i % 20}" for i in range(n)],
        "normalized_purchase": np.random.normal(loc=0.9, scale=0.05, size=n),  # strongly drifted target
        "user_id": np.random.randint(5000, 6000, size=n),
        "gender": np.random.choice(["M", "F"], size=n),
        "age": np.random.choice(["46-50", "51-55"], size=n),
        "occupation": np.random.randint(15, 20, size=n),
        "city_category": np.random.choice(["B", "C"], size=n),
        "stay_in_current_city_years": np.random.choice(["3", "4+"], size=n),
        "marital_status": np.random.choice([0, 1], size=n),
        "purchase": np.random.normal(loc=18000, scale=1000, size=n),
        "is_outlier": False
    })

    return old_df, new_df


def test_drift_monitor_detects_drift(sample_datasets, tmp_path):
    old_df, new_df = sample_datasets
    cols = ["product_category_1", "product_category_2", "product_category_3", "normalized_purchase"]

    monitor = DriftMonitor(reference_df=old_df[cols])
    result = monitor.evaluate_drift(current_df=new_df[cols], output_dir=str(tmp_path / "drift"))

    assert isinstance(result, DriftResult)
    assert result.drifted_features > 0
    assert result.drifted_feature_share > 0.0
    assert result.should_trigger_retrain(dataset_threshold=0.30) is True
    assert bool(result) is True
    assert result.report_path is not None
    assert result.json_path is not None


def test_drift_monitor_stable_data(sample_datasets, tmp_path):
    old_df, _ = sample_datasets
    cols = ["product_category_1", "product_category_2", "product_category_3", "normalized_purchase"]

    # Compare old_df with another sample from same distribution
    stable_df = old_df.sample(frac=0.5, random_state=123)
    monitor = DriftMonitor(reference_df=old_df[cols])
    result = monitor.evaluate_drift(current_df=stable_df[cols], output_dir=str(tmp_path / "drift_stable"))

    assert isinstance(result, DriftResult)
    assert result.should_trigger_retrain(dataset_threshold=0.50) is False


@patch("ml.pipelines.retrain.MLflowTracker")
def test_retrain_on_old_plus_new_data(mock_tracker_class, sample_datasets):
    mock_tracker = MagicMock()
    mock_tracker.get_champion_metrics.return_value = {"r2": 0.50, "rmse": 0.20}
    mock_tracker.log_run.return_value = "mock_run_12345"
    mock_tracker_class.return_value = mock_tracker

    old_df, new_df = sample_datasets

    # Provide prior drift result
    drift_result = DriftResult(
        dataset_drift_detected=True,
        drifted_feature_share=0.75,
        drifted_features=3,
        total_features=4,
        target_drift_detected=True,
        prediction_drift_detected=False,
        report_path=None,
        engine="evidently",
        drifted_columns=["product_category_1", "product_category_2", "normalized_purchase"]
    )

    result = run_champion_challenger_retrain(
        candidate_model_name="linear_regression",
        old_data_df=old_df,
        new_data_df=new_df,
        drift_result=drift_result,
        trigger_reason="drift_detected",
        min_r2_threshold=0.0,
        min_improvement_delta=0.0
    )

    assert isinstance(result, RetrainResult)
    assert result.data_summary["old_records_count"] == len(old_df)
    assert result.data_summary["new_records_count"] == len(new_df)
    assert result.data_summary["combined_total_records"] == len(old_df) + len(new_df)
    assert result.drift_result is not None
    assert result.drift_result.drifted_features == 3

    # Check MLflow logged metrics included drift and data sizes
    logged_calls = mock_tracker.log_run.call_args_list
    assert len(logged_calls) > 0
    _, kwargs = logged_calls[0]
    metrics = kwargs["metrics"]
    tags = kwargs["tags"]
    assert "data_old_rows" in metrics
    assert "data_new_rows" in metrics
    assert "drift_drifted_feature_share" in metrics
    assert tags["trigger_reason"] == "drift_detected"
    assert tags["dataset_drift_detected"] == "True"


@patch("ml.pipelines.monitor.MLflowTracker")
@patch("ml.pipelines.retrain.MLflowTracker")
def test_production_drift_monitor_triggers_retrain(mock_retrain_tracker, mock_monitor_tracker, sample_datasets):
    mock_mon = MagicMock()
    mock_mon.log_run.return_value = "mon_run_123"
    mock_monitor_tracker.return_value = mock_mon

    mock_ret = MagicMock()
    mock_ret.get_champion_metrics.return_value = {"r2": 0.10, "rmse": 0.50}
    mock_ret.log_run.return_value = "ret_run_456"
    mock_retrain_tracker.return_value = mock_ret

    old_df, new_df = sample_datasets

    drift_res = run_production_drift_monitor(
        current_batch_df=new_df,
        reference_df=old_df,
        candidate_model_name="linear_regression",
        dataset_drift_threshold=0.20,
        auto_trigger_retrain=True
    )

    assert drift_res.should_trigger_retrain(dataset_threshold=0.20) is True
    assert drift_res.retrain_result is not None
    assert isinstance(drift_res.retrain_result, RetrainResult)
    assert drift_res.retrain_result.data_summary["old_records_count"] == len(old_df)
    assert drift_res.retrain_result.data_summary["new_records_count"] == len(new_df)


@patch("ml.pipelines.monitor.MLflowTracker")
def test_production_drift_monitor_without_target_does_not_trigger_retrain(mock_monitor_tracker, sample_datasets):
    mock_mon = MagicMock()
    mock_mon.log_run.return_value = "mon_run_123"
    mock_monitor_tracker.return_value = mock_mon

    old_df, new_df = sample_datasets

    # Incoming batch lacks ground-truth target (unlabeled batch)
    unlabeled_new_df = new_df.drop(columns=["normalized_purchase", "purchase"])

    drift_res = run_production_drift_monitor(
        current_batch_df=unlabeled_new_df,
        reference_df=old_df,
        candidate_model_name="linear_regression",
        dataset_drift_threshold=0.20,
        auto_trigger_retrain=True
    )

    # Drift is detected on features/prediction, BUT retraining cannot proceed without labels
    assert drift_res.should_trigger_retrain(dataset_threshold=0.20) is True
    assert drift_res.retrain_result is None


def test_retrain_fails_without_required_inputs(sample_datasets):
    old_df, new_df = sample_datasets

    # Missing old_data_df or new_data_df
    with pytest.raises(ValueError, match="old_data_df is required"):
        run_champion_challenger_retrain(
            old_data_df=None,
            new_data_df=new_df
        )

    with pytest.raises(ValueError, match="new_data_df is required"):
        run_champion_challenger_retrain(
            old_data_df=old_df,
            new_data_df=None
        )

    # Missing target column (neither normalized_purchase nor raw purchase present)
    unlabeled_df = new_df.drop(columns=["normalized_purchase"])
    if "purchase" in unlabeled_df.columns:
        unlabeled_df = unlabeled_df.drop(columns=["purchase"])

    fake_drift = DriftResult(
        dataset_drift_detected=True,
        drifted_feature_share=0.5,
        drifted_features=2,
        total_features=4,
        target_drift_detected=False,
        prediction_drift_detected=False,
        report_path=None
    )
    with pytest.raises(ValueError, match="must contain ground-truth 'normalized_purchase'"):
        run_champion_challenger_retrain(
            old_data_df=old_df,
            new_data_df=unlabeled_df,
            drift_result=fake_drift
        )
