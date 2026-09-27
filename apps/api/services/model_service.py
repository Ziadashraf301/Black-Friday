"""
Model service — local ONNX model management and inference.

Loads the champion ONNX regressor and MissForest imputer strictly from the local
`models/onnx/` directory once on startup. Zero remote MLflow network calls.
"""
import os
import pandas as pd
from typing import Optional, Dict, Any

from apps.api.serving.predictor import ONNXPredictor
from apps.api.serving.imputer import ONNXMissForestImputer
from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)


class ModelService:
    """Manages local ONNX serving pipelines for price prediction."""

    def __init__(self):
        self.predictor: Optional[ONNXPredictor] = None
        self.imputer: Optional[ONNXMissForestImputer] = None
        self.model_name: str = "random_forest (local)"
        self._is_loaded: bool = False

    def load_models(self) -> None:
        """Load regression and imputer ONNX models once from local storage."""
        if self._is_loaded:
            return

        logger.info("ModelService: loading models from local storage...")
        self._load_local_regression()
        self._load_local_imputer()
        self._is_loaded = True
        logger.info(
            f"ModelService: ready. Model='{self.model_name}', "
            f"Imputer={'ready' if self.imputer else 'passthrough'}"
        )

    def _load_local_regression(self) -> None:
        onnx_dir = os.path.join(settings.BASE_DIR, "models", "onnx")
        preferred = ["random_forest", "lightgbm", "lgbm", "decision_tree", "linear_regression"]

        for name in preferred:
            model_path = os.path.join(onnx_dir, f"{name}.onnx")
            if os.path.exists(model_path):
                try:
                    self.predictor = ONNXPredictor(model_path)
                    self.model_name = f"{name} (local)"
                    logger.info(f"ModelService: loaded regression model -> {model_path}")
                    return
                except Exception as e:
                    logger.error(f"ModelService: error loading {model_path}: {e}")

        logger.warning("ModelService: no local ONNX regression model found in models/onnx/")

    def _load_local_imputer(self) -> None:
        imputer_dir = os.path.join(settings.BASE_DIR, "models", "onnx", "imputer")
        meta_file = os.path.join(imputer_dir, "imputer_metadata.json")

        if os.path.exists(meta_file):
            try:
                self.imputer = ONNXMissForestImputer(model_dir=imputer_dir)
                logger.info(f"ModelService: loaded imputer -> {imputer_dir}")
            except Exception as e:
                logger.warning(f"ModelService: failed to load local imputer: {e}")
        else:
            logger.info("ModelService: local imputer directory not found; using fallback fill.")

    def predict_price(
        self,
        product_id: str,
        cat1: int,
        cat2: Optional[int] = None,
        cat3: Optional[int] = None,
    ) -> Dict[str, Any]:
        """
        Executes full serving pipeline:
          1. Assemble DataFrame
          2. Impute missing category 2 / 3 via local MissForest ONNX imputer
          3. Predict normalized price via local ONNX regressor
          4. Denormalize to USD ($)
        """
        if self.predictor is None:
            raise RuntimeError("ModelService: Predictor is not loaded.")

        row = {
            "product_category_1": cat1,
            "product_category_2": cat2,
            "product_category_3": cat3,
            "product_id": product_id,
        }
        df = pd.DataFrame([row])

        # Step 1: Imputation
        if self.imputer is not None and (cat2 is None or cat3 is None):
            for col in ["product_category_2", "product_category_3"]:
                df[col] = pd.to_numeric(df[col], errors="coerce")
            df = self.imputer.transform(df)
        else:
            if df["product_category_2"].isna().any():
                df["product_category_2"] = df["product_category_1"]
            if df["product_category_3"].isna().any():
                df["product_category_3"] = df["product_category_1"]

        for col in ["product_category_1", "product_category_2", "product_category_3"]:
            df[col] = df[col].astype(int)

        # Step 2: Prediction
        norm_pred = float(self.predictor.predict(df)[0])

        # Step 3: Denormalize from normalized (0-1) → raw INR → USD
        # PURCHASE_MAX is the max Black Friday purchase in INR (Indian Rupees).
        # Exchange rate ≈ 80 INR/USD (contemporary approximation for display).
        INR_TO_USD = 80.0
        raw_inr = max(0.0, float(norm_pred * settings.PURCHASE_MAX))
        usd_pred = raw_inr / INR_TO_USD

        return {
            "usd": round(usd_pred, 2),
            "normalized": round(norm_pred, 5),
            "model_used": self.model_name,
        }


# Global singleton instance
model_service = ModelService()
