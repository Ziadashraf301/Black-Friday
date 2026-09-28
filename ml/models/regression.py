import numpy as np
import pandas as pd
from typing import Dict, Any, List, Optional
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder, OrdinalEncoder
from sklearn.compose import ColumnTransformer
from sklearn.linear_model import LinearRegression
from sklearn.tree import DecisionTreeRegressor
from sklearn.ensemble import RandomForestRegressor
from ml.models.base import AbstractBaseModel
from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)

try:
    from lightgbm import LGBMRegressor
    HAS_LIGHTGBM = True
except ImportError:
    HAS_LIGHTGBM = False
    logger.warning("LightGBM is not installed in the local environment.")


ALL_FEATURES: List[str] = [
    "gender",
    "age",
    "occupation",
    "city_category",
    "stay_in_current_city_years",
    "marital_status",
    "product_category_1",
    "product_category_2",
    "product_category_3",
    "product_id"
]

NUMERIC_FEATS: List[str] = ["occupation", "marital_status", "product_category_1", "product_category_2", "product_category_3"]
CATEGORICAL_FEATS: List[str] = ["gender", "age", "city_category", "stay_in_current_city_years"]


class LinearRegressionModel(AbstractBaseModel):
    """Linear regression baseline predicting Purchase from demographic and product category variables."""

    def __init__(self):
        self.features = ALL_FEATURES
        self.preprocessor = ColumnTransformer(
            transformers=[
                ("cat", OneHotEncoder(handle_unknown="ignore", sparse_output=False), [
                    "gender", "age", "city_category", "stay_in_current_city_years",
                    "occupation", "marital_status", "product_category_1", "product_category_2", "product_category_3"
                ])
            ],
            remainder="drop"
        )
        self.pipeline = Pipeline([
            ("preprocessor", self.preprocessor),
            ("regressor", LinearRegression())
        ])

    def fit(self, X: pd.DataFrame, y: pd.Series) -> "LinearRegressionModel":
        logger.info("Fitting Linear Regression model on categorical and product features...")
        self.pipeline.fit(X[self.features], y)
        return self

    def predict(self, X: pd.DataFrame) -> np.ndarray:
        return self.pipeline.predict(X[self.features])

    def get_params(self) -> Dict[str, Any]:
        return {"model_type": "LinearRegression", "features": self.features}


class DecisionTreeModel(AbstractBaseModel):
    """Decision Tree regressor predicting Purchase from all demographic and product variables."""

    def __init__(
        self,
        max_depth: Optional[int] = settings.DT_MAX_DEPTH,
        min_samples_leaf: int = settings.DT_MIN_SAMPLES_LEAF,
        random_state: int = settings.RANDOM_SEED
    ):
        self.features = ALL_FEATURES
        self.max_depth = max_depth if max_depth is not None else 18
        self.min_samples_leaf = min_samples_leaf
        self.random_state = random_state
        self.preprocessor = ColumnTransformer(
            transformers=[
                ("num", "passthrough", NUMERIC_FEATS),
                ("cat", OrdinalEncoder(handle_unknown="use_encoded_value", unknown_value=-1), CATEGORICAL_FEATS + ["product_id"])
            ],
            remainder="drop"
        )
        self.pipeline = Pipeline([
            ("preprocessor", self.preprocessor),
            ("regressor", DecisionTreeRegressor(
                max_depth=self.max_depth,
                min_samples_leaf=self.min_samples_leaf,
                random_state=self.random_state
            ))
        ])

    def fit(self, X: pd.DataFrame, y: pd.Series) -> "DecisionTreeModel":
        logger.info(f"Fitting Decision Tree Regressor (max_depth={self.max_depth}, min_samples_leaf={self.min_samples_leaf}) on 10 features...")
        self.pipeline.fit(X[self.features], y)
        return self

    def predict(self, X: pd.DataFrame) -> np.ndarray:
        return self.pipeline.predict(X[self.features])

    def get_params(self) -> Dict[str, Any]:
        return {
            "model_type": "DecisionTreeRegressor",
            "features": self.features,
            "max_depth": self.max_depth,
            "min_samples_leaf": self.min_samples_leaf,
            "random_state": self.random_state
        }


def _parse_max_features(val: Any) -> Any:
    if val is None or str(val).lower() in ("none", "null"):
        return 1.0
    if isinstance(val, int):
        return val
    if isinstance(val, float):
        return int(val) if val.is_integer() and val >= 1 else val
    try:
        f_val = float(val)
        if 0.0 < f_val <= 1.0:
            return f_val
        if f_val.is_integer() and f_val >= 1:
            return int(f_val)
    except (ValueError, TypeError):
        pass
    if str(val).lower() in ("sqrt", "log2"):
        return str(val).lower()
    return 1.0


class RandomForestModel(AbstractBaseModel):
    """Random Forest regressor predicting Purchase from all demographic and product variables."""

    def __init__(
        self,
        n_estimators: int = settings.RF_N_ESTIMATORS,
        max_features: Optional[Any] = settings.RF_MAX_FEATURES,
        max_depth: Optional[int] = settings.RF_MAX_DEPTH,
        min_samples_leaf: int = settings.RF_MIN_SAMPLES_LEAF,
        n_jobs: int = settings.RF_N_JOBS,
        random_state: int = settings.RANDOM_SEED
    ):
        self.features = ALL_FEATURES
        self.n_estimators = n_estimators
        self.max_features = _parse_max_features(max_features)
        self.max_depth = max_depth
        self.min_samples_leaf = min_samples_leaf
        self.n_jobs = n_jobs
        self.random_state = random_state
        self.preprocessor = ColumnTransformer(
            transformers=[
                ("num", "passthrough", NUMERIC_FEATS),
                ("cat", OrdinalEncoder(handle_unknown="use_encoded_value", unknown_value=-1), CATEGORICAL_FEATS + ["product_id"])
            ],
            remainder="drop"
        )
        self.pipeline = Pipeline([
            ("preprocessor", self.preprocessor),
            ("regressor", RandomForestRegressor(
                n_estimators=self.n_estimators,
                max_features=self.max_features,
                max_depth=self.max_depth,
                min_samples_leaf=self.min_samples_leaf,
                n_jobs=self.n_jobs,
                random_state=self.random_state
            ))
        ])

    def fit(self, X: pd.DataFrame, y: pd.Series) -> "RandomForestModel":
        logger.info(
            f"Fitting Random Forest Regressor ({self.n_estimators} trees, max_features={self.max_features}, "
            f"max_depth={self.max_depth}, min_samples_leaf={self.min_samples_leaf}) on 10 features..."
        )
        self.pipeline.fit(X[self.features], y)
        return self

    def predict(self, X: pd.DataFrame) -> np.ndarray:
        return self.pipeline.predict(X[self.features])

    def get_params(self) -> Dict[str, Any]:
        return {
            "model_type": "RandomForestRegressor",
            "n_estimators": self.n_estimators,
            "max_features": str(self.max_features),
            "max_depth": self.max_depth,
            "min_samples_leaf": self.min_samples_leaf,
            "features": self.features,
            "random_state": self.random_state
        }


class LightGBMModel(AbstractBaseModel):
    """High-performance Gradient Boosted Decision Trees benchmark using LightGBM on 10 features."""

    def __init__(
        self,
        n_estimators: int = settings.LGBM_N_ESTIMATORS,
        learning_rate: float = settings.LGBM_LEARNING_RATE,
        num_leaves: int = settings.LGBM_NUM_LEAVES,
        max_depth: int = settings.LGBM_MAX_DEPTH,
        subsample: float = settings.LGBM_SUBSAMPLE,
        colsample_bytree: float = settings.LGBM_COLSAMPLE_BYTREE,
        n_jobs: int = settings.LGBM_N_JOBS,
        random_state: int = settings.RANDOM_SEED
    ):
        self.features = ALL_FEATURES
        self.n_estimators = n_estimators
        self.learning_rate = learning_rate
        self.num_leaves = num_leaves
        self.max_depth = max_depth
        self.subsample = subsample
        self.colsample_bytree = colsample_bytree
        self.n_jobs = n_jobs
        self.random_state = random_state

        self.preprocessor = ColumnTransformer(
            transformers=[
                ("num", "passthrough", NUMERIC_FEATS),
                ("cat", OrdinalEncoder(handle_unknown="use_encoded_value", unknown_value=-1), CATEGORICAL_FEATS + ["product_id"])
            ],
            remainder="drop"
        )

        if HAS_LIGHTGBM:
            self.pipeline = Pipeline([
                ("preprocessor", self.preprocessor),
                ("regressor", LGBMRegressor(
                    n_estimators=self.n_estimators,
                    learning_rate=self.learning_rate,
                    num_leaves=self.num_leaves,
                    max_depth=self.max_depth,
                    subsample=self.subsample,
                    colsample_bytree=self.colsample_bytree,
                    random_state=self.random_state,
                    n_jobs=self.n_jobs,
                    verbose=-1
                ))
            ])
        else:
            self.pipeline = None

    def fit(self, X: pd.DataFrame, y: pd.Series) -> "LightGBMModel":
        if not HAS_LIGHTGBM:
            raise RuntimeError("Cannot fit LightGBM: package is not installed.")
        logger.info(f"Fitting LightGBM Regressor ({self.n_estimators} trees, lr={self.learning_rate}, num_leaves={self.num_leaves}) on 10 features...")
        self.pipeline.fit(X[self.features], y)
        return self

    def predict(self, X: pd.DataFrame) -> np.ndarray:
        if not HAS_LIGHTGBM:
            raise RuntimeError("Cannot predict with LightGBM: package is not installed.")
        return self.pipeline.predict(X[self.features])

    def get_params(self) -> Dict[str, Any]:
        return {
            "model_type": "LightGBMRegressor",
            "n_estimators": self.n_estimators,
            "learning_rate": self.learning_rate,
            "num_leaves": self.num_leaves,
            "max_depth": self.max_depth,
            "subsample": self.subsample,
            "colsample_bytree": self.colsample_bytree,
            "features": self.features,
            "random_state": self.random_state
        }

