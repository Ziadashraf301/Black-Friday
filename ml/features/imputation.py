import time
import numpy as np
import pandas as pd
from typing import Optional, Dict, Any, List
from sklearn.experimental import enable_iterative_imputer  # noqa
from sklearn.impute import IterativeImputer
from sklearn.ensemble import ExtraTreesRegressor
from sklearn.preprocessing import OrdinalEncoder
from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)

class MissForestImputer:
    """Imputes missing Product_Category_2 and Product_Category_3 using ExtraTrees regression
    leveraging all demographic, product, and target variables in the feature matrix.
    
    Replicates R's missForest library behavior:
    - Estimator: ExtraTreesRegressor
    - Fits on encoded feature matrix (demographics, product_id, category_1..3, purchase).
    - Preserves exact OOB error metrics and exports compact ONNX models.
    """

    FEATURE_COLS: List[str] = [
        "gender",
        "age",
        "occupation",
        "city_category",
        "stay_in_current_city_years",
        "marital_status",
        "product_category_1",
        "product_category_2",
        "product_category_3",
        "product_id",
        "purchase"
    ]
    STRING_COLS: List[str] = ["gender", "age", "city_category", "stay_in_current_city_years", "product_id"]

    def __init__(
        self,
        n_estimators: int = settings.IMPUTER_N_ESTIMATORS,
        max_iter: int = settings.IMPUTER_MAX_ITER,
        sample_size: Optional[int] = settings.IMPUTER_SAMPLE_SIZE,
        max_depth: Optional[int] = settings.IMPUTER_MAX_DEPTH,
        min_samples_leaf: int = settings.IMPUTER_MIN_SAMPLES_LEAF,
        n_jobs: int = settings.IMPUTER_N_JOBS,
        random_state: int = settings.RANDOM_SEED
    ):
        self.n_estimators = n_estimators
        self.max_iter = max_iter
        self.sample_size = sample_size
        self.max_depth = max_depth
        self.min_samples_leaf = min_samples_leaf
        self.n_jobs = n_jobs
        self.random_state = random_state
        self.imputer = None
        self.encoder = None
        self.cols: List[str] = []
        self.category_mappings: Dict[str, Dict[str, int]] = {}
        self.stats: Dict[str, Any] = {}

    def get_params(self) -> Dict[str, Any]:
        """Returns hyperparameters and configuration for MLflow tracking."""
        return {
            "imputer_type": "MissForest_IterativeImputer",
            "imputer_estimator": "ExtraTreesRegressor",
            "imputer_n_estimators": self.n_estimators,
            "imputer_max_iter": self.max_iter,
            "imputer_sample_size": self.sample_size if self.sample_size else "full_dataset",
            "imputer_max_depth": self.max_depth if self.max_depth else "none",
            "imputer_min_samples_leaf": self.min_samples_leaf,
            "imputer_random_state": self.random_state,
            "features_used": self.cols
        }

    def _prepare_numeric_matrix(self, df: pd.DataFrame, is_fit: bool = True) -> pd.DataFrame:
        """Converts raw dataframe into a full numerical matrix by encoding string columns."""
        present_cols = [c for c in self.FEATURE_COLS if c in df.columns]
        if is_fit:
            self.cols = present_cols
            string_cols = [c for c in self.STRING_COLS if c in present_cols]
            self.encoder = OrdinalEncoder(handle_unknown="use_encoded_value", unknown_value=-1)
            if string_cols:
                self.encoder.fit(df[string_cols].astype(str))
                for idx, col in enumerate(string_cols):
                    cats = self.encoder.categories_[idx]
                    self.category_mappings[col] = {str(cat): i for i, cat in enumerate(cats)}

        matrix = df[self.cols].copy()
        string_cols = [c for c in self.STRING_COLS if c in self.cols]
        if string_cols and self.encoder is not None:
            encoded = self.encoder.transform(matrix[string_cols].astype(str))
            for i, col in enumerate(string_cols):
                matrix[col] = encoded[:, i]

        return matrix.astype(np.float32)

    def fit_transform(self, df: pd.DataFrame) -> pd.DataFrame:
        """Fits imputer on all feature columns (demographics + product + target) and returns imputed dataframe."""
        logger.info(
            f"Initializing MissForest IterativeImputer on all variables (trees={self.n_estimators}, max_iter={self.max_iter}, "
            f"max_depth={self.max_depth}, min_samples_leaf={self.min_samples_leaf}, "
            f"sample_size={self.sample_size or 'full'}, bootstrap=True, oob_score=True)..."
        )

        start_time = time.time()
        missing_cols = ["product_category_2", "product_category_3"]
        missing_before = {col: int(df[col].isna().sum()) for col in missing_cols if col in df.columns}

        numeric_df = self._prepare_numeric_matrix(df, is_fit=True)

        estimator = ExtraTreesRegressor(
            n_estimators=self.n_estimators,
            max_depth=self.max_depth,
            min_samples_leaf=self.min_samples_leaf,
            bootstrap=True,
            oob_score=True,
            random_state=self.random_state,
            n_jobs=self.n_jobs
        )

        self.imputer = IterativeImputer(
            estimator=estimator,
            max_iter=self.max_iter,
            random_state=self.random_state,
            verbose=1
        )

        if self.sample_size and len(numeric_df) > self.sample_size:
            logger.info(
                f"Dataset has {len(numeric_df):,} rows. Fitting MissForest imputer on representative sample "
                f"of {self.sample_size:,} records..."
            )
            sample_subset = numeric_df.sample(n=self.sample_size, random_state=self.random_state)
            self.imputer.fit(sample_subset)
            logger.info("Transforming full dataset with fitted MissForest imputer...")
            imputed_array = self.imputer.transform(numeric_df)
        else:
            logger.info(f"Fitting and transforming MissForest on all {len(numeric_df):,} records...")
            imputed_array = self.imputer.fit_transform(numeric_df)

        imputed_df = pd.DataFrame(imputed_array, columns=self.cols, index=df.index)

        # Round categories to nearest valid integer category
        result_df = df.copy()
        result_df["product_category_2"] = np.round(imputed_df["product_category_2"]).astype(int)
        result_df["product_category_3"] = np.round(imputed_df["product_category_3"]).astype(int)

        duration = time.time() - start_time
        missing_after = {col: int(result_df[col].isna().sum()) for col in missing_cols if col in result_df.columns}

        # Extract Out-Of-Bag (OOB) error scores from fitted estimators
        oob_scores: Dict[str, float] = {}
        oob_errors: Dict[str, float] = {}
        if hasattr(self.imputer, "imputation_sequence_"):
            for triplet in self.imputer.imputation_sequence_:
                feat_idx = triplet.feat_idx
                col_name = self.cols[feat_idx] if feat_idx < len(self.cols) else f"feature_{feat_idx}"
                if hasattr(triplet.estimator, "oob_score_") and triplet.estimator.oob_score_ is not None:
                    score = float(triplet.estimator.oob_score_)
                    oob_scores[col_name] = round(score, 4)
                    oob_errors[col_name] = round(max(0.0, 1.0 - score), 4)

        self.stats = {
            "imputation_duration_seconds": round(duration, 2),
            "missing_before": missing_before,
            "missing_after": missing_after,
            "imputed_columns": missing_cols,
            "total_records_imputed": len(result_df),
            "oob_scores": oob_scores,
            "oob_errors": oob_errors
        }

        logger.info(
            f"Imputation successfully completed in {duration:.1f}s. "
            f"OOB Errors: {oob_errors}. Remaining missing: {missing_after}"
        )
        return result_df

    def transform(self, df: pd.DataFrame) -> pd.DataFrame:
        """Applies fitted imputer to new observations."""
        if self.imputer is None:
            raise RuntimeError("Imputer has not been fitted yet.")
        numeric_df = self._prepare_numeric_matrix(df, is_fit=False)
        imputed_array = self.imputer.transform(numeric_df)
        imputed_df = pd.DataFrame(imputed_array, columns=self.cols, index=df.index)

        result_df = df.copy()
        result_df["product_category_2"] = np.round(imputed_df["product_category_2"]).astype(int)
        result_df["product_category_3"] = np.round(imputed_df["product_category_3"]).astype(int)
        return result_df

