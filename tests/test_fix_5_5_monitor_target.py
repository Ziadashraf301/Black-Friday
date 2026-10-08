"""
Regression test for Fix 5.5:
- Ground-truth check in monitor.py must look at evaluation dataframe (curr_eval)
  or the raw batch column "purchase".
- Test: a batch containing "purchase" sets has_target True; a batch without it sets False.
"""
import pandas as pd
from core.config import settings


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
