import os
import json
import numpy as np
import pandas as pd
import onnxruntime as ort
from typing import Dict, Any, List
from core.logging import get_logger

logger = get_logger(__name__)


class ONNXMissForestImputer:
    """Fast ONNX Runtime inference engine for MissForest missing value imputation in serving."""

    def __init__(self, model_dir: str):
        self.model_dir = model_dir
        meta_path = os.path.join(model_dir, "imputer_metadata.json")
        if not os.path.exists(meta_path):
            raise FileNotFoundError(f"Imputer metadata file not found at: {meta_path}")

        with open(meta_path, "r", encoding="utf-8") as f:
            self.meta = json.load(f)

        self.cols: List[str] = self.meta["columns"]
        self.initial_stats = np.array(self.meta["initial_statistics"], dtype=np.float32)
        self.sessions: Dict[str, Any] = {}

        for step in self.meta["steps"]:
            session_opts = ort.SessionOptions()
            session_opts.graph_optimization_level = ort.GraphOptimizationLevel.ORT_ENABLE_BASIC
            sess = ort.InferenceSession(
                os.path.join(model_dir, step["onnx_file"]),
                sess_options=session_opts,
                providers=["CPUExecutionProvider"]
            )
            self.sessions[step["target_col"]] = (step, sess)

    def transform(self, df: pd.DataFrame) -> pd.DataFrame:
        """Imputes missing Product_Category_2 and Product_Category_3 using ONNX Runtime models."""
        result = df.copy()

        # Handle unlabelled batches where target (e.g. purchase) is omitted
        temp_added_cols: List[str] = []
        for j, c in enumerate(self.cols):
            if c not in result.columns:
                result[c] = self.initial_stats[j]
                temp_added_cols.append(c)

        arr = result[self.cols].to_numpy(dtype=np.float32, copy=True)
        missing_mask = np.isnan(arr)

        # 1. Fill missing values with initial statistics (mean/median)
        for j in range(arr.shape[1]):
            arr[missing_mask[:, j], j] = self.initial_stats[j]

        # 2. Iteratively impute using ONNX regression models
        for col_name, (step, session) in self.sessions.items():
            feat_idx = step["target_idx"]
            col_missing = missing_mask[:, feat_idx]
            if not np.any(col_missing):
                continue
            neighbor_indices = step["neighbor_indices"]
            X_input = arr[col_missing][:, neighbor_indices]
            input_name = session.get_inputs()[0].name
            preds = session.run(None, {input_name: X_input})[0].flatten()

            min_val = self.meta["min_value"][feat_idx]
            max_val = self.meta["max_value"][feat_idx]
            if not np.isneginf(min_val) or not np.isposinf(max_val):
                preds = np.clip(preds, min_val, max_val)

            arr[col_missing, feat_idx] = preds

        # 3. Post-process integer categories
        for j, c in enumerate(self.cols):
            if c in temp_added_cols:
                continue
            if c in ["product_category_2", "product_category_3"]:
                result[c] = np.round(arr[:, j]).astype(int)
            else:
                result[c] = arr[:, j]

        for c in temp_added_cols:
            if c in result.columns:
                result.drop(columns=[c], inplace=True)

        return result
