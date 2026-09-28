from typing import Dict, Type, Any, List
from ml.models.base import AbstractBaseModel
from ml.models.regression import (
    LinearRegressionModel,
    DecisionTreeModel,
    RandomForestModel,
    LightGBMModel
)
from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)


class ModelRegistry:
    """Model Factory providing config-agnostic instantiation and model swapping (SSOT)."""

    _MODELS: Dict[str, Type[AbstractBaseModel]] = {
        "linear_regression": LinearRegressionModel,
        "decision_tree": DecisionTreeModel,
        "random_forest": RandomForestModel,
        "lightgbm": LightGBMModel
    }

    _ALIAS_MAP: Dict[str, str] = {
        "lgbm": "lightgbm",
        "rf": "random_forest",
        "dt": "decision_tree",
        "lr": "linear_regression"
    }

    @classmethod
    def resolve_name(cls, model_name: str) -> str:
        name = (model_name or settings.DEFAULT_REGRESSION_MODEL).lower()
        return cls._ALIAS_MAP.get(name, name)

    @classmethod
    def get_default_params(cls, model_name: str) -> Dict[str, Any]:
        """Resolves default model hyperparameters dynamically from settings at runtime."""
        name = cls.resolve_name(model_name)
        if name == "decision_tree":
            return {
                "max_depth": settings.DT_MAX_DEPTH,
                "min_samples_leaf": settings.DT_MIN_SAMPLES_LEAF,
                "random_state": settings.RANDOM_SEED
            }
        elif name == "random_forest":
            return {
                "n_estimators": settings.RF_N_ESTIMATORS,
                "max_features": settings.RF_MAX_FEATURES,
                "max_depth": settings.RF_MAX_DEPTH,
                "min_samples_leaf": settings.RF_MIN_SAMPLES_LEAF,
                "n_jobs": settings.RF_N_JOBS,
                "random_state": settings.RANDOM_SEED
            }
        elif name == "lightgbm":
            return {
                "n_estimators": settings.LGBM_N_ESTIMATORS,
                "learning_rate": settings.LGBM_LEARNING_RATE,
                "num_leaves": settings.LGBM_NUM_LEAVES,
                "max_depth": settings.LGBM_MAX_DEPTH,
                "n_jobs": settings.LGBM_N_JOBS,
                "random_state": settings.RANDOM_SEED
            }
        elif name == "linear_regression":
            return {}
        return {}

    @classmethod
    def get_model(cls, model_name: str = None, **kwargs) -> AbstractBaseModel:
        """Instantiates requested model using dynamic settings defaults merged with caller kwargs."""
        selected_name = cls.resolve_name(model_name)
        if selected_name not in cls._MODELS:
            available = list(cls._MODELS.keys())
            raise ValueError(f"Unknown model '{selected_name}'. Available registered models: {available}")

        defaults = cls.get_default_params(selected_name)
        merged_kwargs = {**defaults, **kwargs}

        logger.info(f"Instantiating model from registry: '{selected_name}' with kwargs={merged_kwargs}")
        model_class = cls._MODELS[selected_name]
        return model_class(**merged_kwargs)

    @classmethod
    def list_available_models(cls) -> List[str]:
        return list(cls._MODELS.keys())
