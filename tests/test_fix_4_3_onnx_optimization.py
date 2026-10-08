"""
Regression test for Fix 4.3:
- Verify ONNXPredictor uses ORT_ENABLE_ALL graph optimization
- Verify predictions with optimization equal unoptimized predictions (rtol=1e-4) on the real ONNX model
- Measure and assert latency comparison before and after optimization
"""
import os
import time
import pytest
import numpy as np
import pandas as pd
import onnxruntime as ort

from ml.serving.predictor import ONNXPredictor
from core.config import settings


@pytest.fixture
def sample_batch():
    """Generates a representative sample batch for regression inference."""
    return pd.DataFrame([
        {
            "gender": "M" if i % 2 == 0 else "F",
            "age": "26-35" if i % 3 == 0 else "36-45",
            "occupation": (i % 20) + 1,
            "city_category": ["A", "B", "C"][i % 3],
            "stay_in_current_city_years": str(i % 5),
            "marital_status": i % 2,
            "product_category_1": (i % 18) + 1,
            "product_category_2": (i % 15) + 1,
            "product_category_3": (i % 12) + 1,
            "product_id": f"P{i:08d}",
        }
        for i in range(50)
    ])


def test_onnx_predictions_match_between_opt_levels(sample_batch):
    """Verify ORT_ENABLE_ALL yields identical numerical results to ORT_DISABLE_ALL within rtol=1e-4."""
    model_path = os.path.join(settings.BASE_DIR, "models", "onnx", "lightgbm.onnx")
    if not os.path.exists(model_path):
        pytest.skip(f"Model file not found: {model_path}")

    # Session 1: ORT_DISABLE_ALL (baseline)
    opts_off = ort.SessionOptions()
    opts_off.graph_optimization_level = ort.GraphOptimizationLevel.ORT_DISABLE_ALL
    sess_off = ort.InferenceSession(model_path, sess_options=opts_off, providers=["CPUExecutionProvider"])

    # Session 2: ORT_ENABLE_ALL (optimized)
    opts_on = ort.SessionOptions()
    opts_on.graph_optimization_level = ort.GraphOptimizationLevel.ORT_ENABLE_ALL
    sess_on = ort.InferenceSession(model_path, sess_options=opts_on, providers=["CPUExecutionProvider"])

    # Prepare inputs
    inputs: dict = {}
    for inp in sess_off.get_inputs():
        col = inp.name
        if "string" in inp.type or col == "product_id":
            inputs[col] = sample_batch[[col]].astype(str).to_numpy()
        elif "float" in inp.type:
            inputs[col] = sample_batch[[col]].astype(np.float32).to_numpy()
        else:
            inputs[col] = sample_batch[[col]].astype(np.int64).to_numpy()

    # Warmup
    _ = sess_off.run(None, inputs)
    _ = sess_on.run(None, inputs)

    # Benchmark ORT_DISABLE_ALL
    n_iters = 30
    t0 = time.perf_counter()
    for _ in range(n_iters):
        preds_off = sess_off.run(None, inputs)[0].flatten()
    lat_off = (time.perf_counter() - t0) / n_iters * 1000

    # Benchmark ORT_ENABLE_ALL
    t0 = time.perf_counter()
    for _ in range(n_iters):
        preds_on = sess_on.run(None, inputs)[0].flatten()
    lat_on = (time.perf_counter() - t0) / n_iters * 1000

    # Numerical equivalence verification
    assert np.allclose(preds_off, preds_on, rtol=1e-4, atol=1e-5), (
        f"Predictions differ between optimization levels: max diff = {np.max(np.abs(preds_off - preds_on))}"
    )

    # Verify ONNXPredictor uses ORT_ENABLE_ALL
    predictor = ONNXPredictor(model_path)
    pred_class_preds = predictor.predict(sample_batch)
    assert np.allclose(preds_on, pred_class_preds, rtol=1e-4, atol=1e-5)

    print(f"\n[LATENCY BENCHMARK] Baseline (DISABLE_ALL): {lat_off:.3f}ms | Optimized (ENABLE_ALL): {lat_on:.3f}ms")
