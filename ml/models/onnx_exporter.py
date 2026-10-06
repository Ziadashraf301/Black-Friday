import os
import json
import numpy as np
import pandas as pd
from typing import Dict, Any, List, Union
from skl2onnx import convert_sklearn, update_registered_converter
from skl2onnx.common.shape_calculator import calculate_linear_regressor_output_shapes
from skl2onnx.common.data_types import StringTensorType, Int64TensorType, FloatTensorType
import onnxruntime as ort

from core.logging import get_logger
from ml.serving.imputer import ONNXMissForestImputer
from ml.serving.predictor import ONNXPredictor

logger = get_logger(__name__)

# Register LightGBM converter with skl2onnx if installed
try:
    from lightgbm import LGBMRegressor
    from onnxmltools.convert.lightgbm.operator_converters.LightGbm import convert_lightgbm
    update_registered_converter(
        LGBMRegressor,
        "LightGbmLGBMRegressor",
        calculate_linear_regressor_output_shapes,
        convert_lightgbm
    )
except ImportError:
    pass


class ONNXExporter:
    """Exports scikit-learn pipelines and imputer models to ONNX and verifies runtime parity."""

    DEFAULT_OPSET: Dict[str, int] = {"": 15, "ai.onnx.ml": 3}

    @classmethod
    def _save_onnx(
        cls,
        model: Any,
        initial_types: list,
        output_filepath: str,
        target_opset: Union[int, Dict[str, int]] = 15
    ) -> str:
        """Helper to convert model to ONNX graph and serialize to disk (DRY)."""
        os.makedirs(os.path.dirname(output_filepath), exist_ok=True)

        # Ensure compatibility with LightGBM and tree ensemble operators
        if isinstance(target_opset, int):
            opset = {"": target_opset, "ai.onnx.ml": 3}
        else:
            opset = target_opset

        onnx_model = convert_sklearn(
            model,
            initial_types=initial_types,
            target_opset=opset
        )
        with open(output_filepath, "wb") as f:
            f.write(onnx_model.SerializeToString())
        return output_filepath

    @classmethod
    def export_regression_pipeline(
        cls,
        pipeline,
        feature_names: list,
        output_filepath: str,
        target_opset: Union[int, Dict[str, int]] = 15
    ) -> str:
        """Converts a fitted scikit-learn pipeline into an ONNX graph."""
        logger.info(f"Converting pipeline to ONNX format...")

        initial_types = []
        for feature in feature_names:
            if feature in ["product_id", "gender", "age", "city_category", "stay_in_current_city_years"]:
                initial_types.append((feature, StringTensorType([None, 1])))
            else:
                initial_types.append((feature, Int64TensorType([None, 1])))

        cls._save_onnx(
            model=pipeline,
            initial_types=initial_types,
            output_filepath=output_filepath,
            target_opset=target_opset
        )

        logger.info(f"ONNX model saved successfully to: {output_filepath}")
        return output_filepath

    @classmethod
    def run_onnx_inference(cls, onnx_path: str, df: pd.DataFrame) -> np.ndarray:
        """Executes fast ONNX runtime inference on input DataFrame using serving ONNXPredictor."""
        predictor = ONNXPredictor(onnx_model_path=onnx_path)
        return predictor.predict(df)

    @classmethod
    def verify_parity(
        cls,
        sklearn_model,
        onnx_path: str,
        sample_df: pd.DataFrame,
        tolerance: float = 1e-4
    ) -> bool:
        """Validates that ONNX Runtime inference matches Scikit-Learn predictions within tolerance and logs latency performance comparison."""
        logger.info(f"Verifying ONNX inference parity against Scikit-Learn on {len(sample_df)} sample rows...")
        import time

        predictor = ONNXPredictor(onnx_model_path=onnx_path)

        # Warm-up pass
        predictor.predict(sample_df.head(1))

        # Measure Scikit-Learn / Python inference timing
        t0_sk = time.perf_counter()
        sk_preds = sklearn_model.predict(sample_df)
        t_sk_ms = (time.perf_counter() - t0_sk) * 1000.0

        # Measure ONNX Runtime C++ inference timing
        t0_onnx = time.perf_counter()
        onnx_preds = predictor.predict(sample_df)
        t_onnx_ms = (time.perf_counter() - t0_onnx) * 1000.0

        n_samples = len(sample_df)
        sk_per_item_ms = t_sk_ms / max(1, n_samples)
        onnx_per_item_ms = t_onnx_ms / max(1, n_samples)
        speedup = t_sk_ms / max(1e-6, t_onnx_ms)

        max_diff = float(np.max(np.abs(sk_preds - onnx_preds)))
        is_parity_ok = max_diff < tolerance

        logger.info(f"=== Model Serving Latency Benchmark ({n_samples} samples) ===")
        logger.info(f"Scikit-Learn / Python Runtime Latency : {t_sk_ms:.2f} ms total ({sk_per_item_ms:.4f} ms/sample)")
        logger.info(f"ONNX Runtime C++ Engine Latency        : {t_onnx_ms:.2f} ms total ({onnx_per_item_ms:.4f} ms/sample)")
        logger.info(f"ONNX Graph Acceleration Ratio          : {speedup:.2f}x Speedup")
        logger.info(f"Parity check completed. Max absolute difference: {max_diff:.6f}. Passed: {is_parity_ok}")

        if not is_parity_ok:
            raise ValueError(f"ONNX parity check failed! Max difference {max_diff} exceeds tolerance {tolerance}.")
        return is_parity_ok

    @classmethod
    def export_imputer(
        cls,
        missforest_imputer,
        output_dir: str = "models/onnx/imputer",
        target_opset: Union[int, Dict[str, int]] = 15
    ) -> Dict[str, str]:
        """Converts fitted MissForest iterative estimators (Category 2 & 3) to compact ONNX graphs.

        Saves:
        - imputer_product_category_2.onnx
        - imputer_product_category_3.onnx
        - imputer_metadata.json (initial statistics, column mapping, bounds)
        Returns dictionary of saved artifact paths.
        """
        os.makedirs(output_dir, exist_ok=True)
        inner = missforest_imputer.imputer
        cols = getattr(missforest_imputer, "cols", ["product_category_1", "product_category_2", "product_category_3", "purchase"])
        category_mappings = getattr(missforest_imputer, "category_mappings", {})

        metadata = {
            "columns": cols,
            "category_mappings": category_mappings,
            "initial_statistics": inner.initial_imputer_.statistics_.tolist(),
            "min_value": [float(v) for v in inner._min_value],
            "max_value": [float(v) for v in inner._max_value],
            "steps": []
        }
        saved_files = {}

        for i, seq in enumerate(inner.imputation_sequence_):
            feat_idx = int(seq.feat_idx)
            col_name = cols[feat_idx]
            if col_name in ["product_category_2", "product_category_3"]:
                onnx_filename = f"imputer_{col_name}.onnx"
                onnx_filepath = os.path.join(output_dir, onnx_filename)
                neighbor_indices = [int(idx) for idx in seq.neighbor_feat_idx]

                initial_type = [("float_input", FloatTensorType([None, len(neighbor_indices)]))]
                cls._save_onnx(
                    model=seq.estimator,
                    initial_types=initial_type,
                    output_filepath=onnx_filepath,
                    target_opset=target_opset
                )

                step_info = {
                    "step_index": i,
                    "target_col": col_name,
                    "target_idx": feat_idx,
                    "neighbor_cols": [cols[idx] for idx in neighbor_indices],
                    "neighbor_indices": neighbor_indices,
                    "onnx_file": onnx_filename
                }
                metadata["steps"].append(step_info)
                saved_files[col_name] = onnx_filepath
                logger.info(f"Exported ONNX imputer model for '{col_name}' to: {onnx_filepath} ({os.path.getsize(onnx_filepath)/1024/1024:.2f} MB)")

        meta_path = os.path.join(output_dir, "imputer_metadata.json")
        with open(meta_path, "w", encoding="utf-8") as f:
            json.dump(metadata, f, indent=2)
        saved_files["metadata"] = meta_path
        saved_files["dir"] = output_dir

        logger.info(f"Saved ONNX imputer metadata to: {meta_path}")
        return saved_files


__all__ = ["ONNXExporter", "ONNXMissForestImputer"]
