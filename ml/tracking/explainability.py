import pandas as pd
from typing import Optional, Tuple, Any
from core.logging import get_logger

logger = get_logger(__name__)

try:
    import shap
    HAS_SHAP = True
except ImportError:
    HAS_SHAP = False
    logger.warning("SHAP library is not installed in the local environment.")


class ModelExplainability:
    """Computes SHAP feature attributions for both Tree-based and Linear regression models."""

    def __init__(self, model_pipeline, background_sample: pd.DataFrame):
        self.pipeline = model_pipeline
        self.background_sample = background_sample
        self.explainer = None

        if HAS_SHAP:
            try:
                regressor = model_pipeline.named_steps.get("regressor")
                preprocessor = model_pipeline.named_steps.get("preprocessor")
                transformed_bg = preprocessor.transform(background_sample)

                model_type_name = type(regressor).__name__.lower()
                if "linear" in model_type_name or "ridge" in model_type_name or "lasso" in model_type_name:
                    logger.info(f"Initializing shap.LinearExplainer for {type(regressor).__name__}...")
                    self.explainer = shap.LinearExplainer(regressor, transformed_bg)
                else:
                    logger.info(f"Initializing shap.TreeExplainer for {type(regressor).__name__}...")
                    try:
                        self.explainer = shap.TreeExplainer(regressor)
                    except Exception as tree_err:
                        logger.warning(f"TreeExplainer default initialization failed: {tree_err}, trying background sample...")
                        self.explainer = shap.TreeExplainer(regressor, transformed_bg)
            except Exception as e:
                logger.warning(f"Could not initialize SHAP explainer for {type(regressor).__name__}: {e}")

    def explain(self, eval_df: pd.DataFrame) -> Tuple[Optional[Any], Optional[Any]]:
        """Transforms eval_df and computes SHAP attribution values.

        Returns:
            Tuple of (shap_values, transformed_eval_features)
        """
        if not HAS_SHAP or not self.explainer:
            logger.warning("SHAP is unavailable. Skipping feature attribution.")
            return None, None

        try:
            preprocessor = self.pipeline.named_steps.get("preprocessor")
            X_eval = preprocessor.transform(eval_df)
            shap_values = self.explainer.shap_values(X_eval)
            return shap_values, X_eval
        except Exception as e:
            logger.error(f"Failed to compute SHAP attributions: {e}", exc_info=True)
            return None, None
