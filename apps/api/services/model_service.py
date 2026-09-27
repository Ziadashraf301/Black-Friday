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
        self.model_name: str = "MLflow Production Champion (local)"
        self._is_loaded: bool = False

    def load_models(self) -> None:
        """Load Production Champion regression and imputer ONNX models strictly from local storage."""
        if self._is_loaded:
            return

        logger.info("ModelService: loading Production Champion models from local storage...")
        self._load_local_regression()
        self._load_local_imputer()
        self._is_loaded = True
        logger.info(
            f"ModelService: ready. Champion Model='{self.model_name}', "
            f"Imputer={'ready' if self.imputer else 'passthrough'}"
        )

    def _load_local_regression(self) -> None:
        onnx_dir = os.path.join(settings.BASE_DIR, "models", "onnx")
        
        # Priority order for Production Champion:
        # 1. champion_model.onnx (direct export from MLflow champion promotion)
        # 2. lightgbm.onnx (current top benchmark model)
        candidates = ["champion_model.onnx", "lightgbm.onnx", "lgbm.onnx"]
        
        for candidate in candidates:
            model_path = os.path.join(onnx_dir, candidate)
            if os.path.exists(model_path):
                try:
                    self.predictor = ONNXPredictor(model_path)
                    name_stem = candidate.replace(".onnx", "").replace("_model", "")
                    self.model_name = f"{name_stem} (Production Champion)"
                    logger.info(f"ModelService: successfully loaded Production Champion regression model -> {model_path}")
                    return
                except Exception as e:
                    logger.error(f"ModelService: failed to load candidate model {model_path}: {e}")

        raise RuntimeError(
            f"ModelService startup failure: No MLflow Production Champion ONNX regression model found in '{onnx_dir}'. "
            "Execute 'make train' to train models and export the production champion."
        )

    def _load_local_imputer(self) -> None:
        imputer_dir = os.path.join(settings.BASE_DIR, "models", "onnx", "imputer")
        meta_file = os.path.join(imputer_dir, "imputer_metadata.json")

        if os.path.exists(meta_file):
            try:
                self.imputer = ONNXMissForestImputer(model_dir=imputer_dir)
                logger.info(f"ModelService: loaded MissForest imputer -> {imputer_dir}")
            except Exception as e:
                logger.error(f"ModelService: failed to load local imputer: {e}")
                raise RuntimeError(f"ModelService startup failure: Failed to initialize MissForest imputer: {e}")
        else:
            logger.info("ModelService: local imputer metadata not found; using exact product category passthrough.")

    def predict_price(
        self,
        product_id: str,
        cat1: int,
        cat2: Optional[int] = None,
        cat3: Optional[int] = None,
        gender: Optional[str] = None,
        age: Optional[str] = None,
        occupation: Optional[int] = None,
        city_category: Optional[str] = None,
        stay_in_current_city_years: Optional[str] = None,
        marital_status: Optional[int] = None,
    ) -> Dict[str, Any]:
        """
        Executes full serving pipeline:
          1. Validate strict presence of all required user demographics and product features
          2. Impute missing category 2 / 3 via local MissForest ONNX imputer
          3. Predict normalized price via local Production Champion ONNX regressor
          4. Denormalize to USD ($)
        """
        if self.predictor is None:
            raise RuntimeError("ModelService error: Production Champion Predictor is not loaded.")

        # Strict validation: Fail immediately if any demographic or product feature is missing
        required_features = {
            "product_id": product_id,
            "product_category_1": cat1,
            "gender": gender,
            "age": age,
            "occupation": occupation,
            "city_category": city_category,
            "stay_in_current_city_years": stay_in_current_city_years,
            "marital_status": marital_status,
        }
        
        missing = [feat for feat, val in required_features.items() if val is None or str(val).strip() == ""]
        if missing:
            raise ValueError(
                f"Missing required feature(s) for model inference: {missing}. "
                "Must provide user_id to look up demographics from DB or pass a complete feature vector."
            )

        row = {
            "gender": str(gender),
            "age": str(age),
            "occupation": int(occupation),
            "city_category": str(city_category),
            "stay_in_current_city_years": str(stay_in_current_city_years),
            "marital_status": int(marital_status),
            "product_category_1": int(cat1),
            "product_category_2": cat2,
            "product_category_3": cat3,
            "product_id": str(product_id),
        }
        df = pd.DataFrame([row])

        # Step 1: Imputation for missing category 2 or 3
        if self.imputer is not None and (cat2 is None or cat3 is None):
            for col in ["product_category_2", "product_category_3"]:
                df[col] = pd.to_numeric(df[col], errors="coerce")
            df = self.imputer.transform(df)
        else:
            if df["product_category_2"].isna().any():
                df["product_category_2"] = df["product_category_1"]
            if df["product_category_3"].isna().any():
                df["product_category_3"] = df["product_category_1"]

        for col in ["occupation", "marital_status", "product_category_1", "product_category_2", "product_category_3"]:
            df[col] = df[col].astype(int)

        # Step 2: Prediction via Production Champion
        norm_pred = float(self.predictor.predict(df)[0])

        # Step 3: Denormalize from normalized (0-1) → raw INR → USD
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
