import os
import numpy as np
import pandas as pd
import onnxruntime as ort
from typing import Dict, Any, Optional
from core.logging import get_logger

logger = get_logger(__name__)


class ONNXPredictor:
    """High-performance ONNX Runtime inference engine for regression models in FastAPI serving."""

    def __init__(self, onnx_model_path: str):
        if not os.path.exists(onnx_model_path):
            raise FileNotFoundError(f"ONNX model file not found at: {onnx_model_path}")

        self.onnx_model_path = onnx_model_path
        opts = ort.SessionOptions()
        opts.graph_optimization_level = ort.GraphOptimizationLevel.ORT_ENABLE_BASIC
        self.session = ort.InferenceSession(
            onnx_model_path,
            sess_options=opts,
            providers=["CPUExecutionProvider"]
        )

    def predict(self, df: pd.DataFrame) -> np.ndarray:
        """Executes fast ONNX runtime inference on input DataFrame."""
        model_inputs = {inp.name: inp for inp in self.session.get_inputs()}
        inputs: Dict[str, np.ndarray] = {}
        for col, inp in model_inputs.items():
            if col in df.columns:
                if "string" in inp.type or col == "product_id":
                    inputs[col] = df[[col]].astype(str).to_numpy()
                elif "float" in inp.type:
                    inputs[col] = df[[col]].astype(np.float32).to_numpy()
                else:
                    inputs[col] = df[[col]].astype(np.int64).to_numpy()

        preds = self.session.run(None, inputs)[0].flatten()
        return preds
