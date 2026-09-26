import pandas as pd
import numpy as np
from typing import Tuple, Dict, Any
from sklearn.model_selection import train_test_split
from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)

class DataPreprocessor:
    """Handles IQR outlier detection, max-scaling, and train-test splits."""

    def __init__(self, purchase_max: float = settings.PURCHASE_MAX, random_state: int = settings.RANDOM_SEED):
        self.purchase_max = purchase_max
        self.random_state = random_state
        self.outlier_bounds: Dict[str, float] = {}

    def compute_outlier_bounds(self, series: pd.Series) -> Tuple[float, float]:
        """Calculates Q1 - 1.5*IQR and Q3 + 1.5*IQR."""
        q1 = series.quantile(0.25)
        q3 = series.quantile(0.75)
        iqr = q3 - q1
        lower_bound = max(0.0, float(q1 - (1.5 * iqr)))
        upper_bound = float(q3 + (1.5 * iqr))
        self.outlier_bounds = {"lower": lower_bound, "upper": upper_bound}
        logger.info(f"Computed IQR outlier bounds: Lower={lower_bound:.2f}, Upper={upper_bound:.2f}")
        return lower_bound, upper_bound

    def tag_outliers(self, df: pd.DataFrame) -> pd.DataFrame:
        """Flags records outside IQR bounds as outliers (identifying predominantly Category 10 orders)."""
        if not self.outlier_bounds:
            self.compute_outlier_bounds(df["purchase"])

        result_df = df.copy()
        is_outlier = (result_df["purchase"] < self.outlier_bounds["lower"]) | (result_df["purchase"] > self.outlier_bounds["upper"])
        result_df["is_outlier"] = is_outlier
        outlier_count = is_outlier.sum()
        logger.info(f"Tagged {outlier_count} outliers ({outlier_count / len(df) * 100:.2f}% of data).")
        return result_df

    def normalize_target(self, df: pd.DataFrame) -> pd.DataFrame:
        """Scales Purchase by dividing by Purchase_max (scaling to [0, 1])."""
        result_df = df.copy()
        result_df["normalized_purchase"] = result_df["purchase"] / self.purchase_max
        return result_df

    def denormalize_prediction(self, normalized_pred: np.ndarray) -> np.ndarray:
        """Rescales normalized predictions back to USD."""
        return normalized_pred * self.purchase_max

    def split_train_test(
        self, df: pd.DataFrame, test_size: float = 0.10, exclude_outliers: bool = True
    ) -> Tuple[pd.DataFrame, pd.DataFrame]:
        if exclude_outliers and "is_outlier" in df.columns:
            is_outlier_bool = df["is_outlier"].fillna(False).astype(bool)
            data = df[~is_outlier_bool].copy()
        else:
            data = df.copy()
        train_df, test_df = train_test_split(data, test_size=test_size, random_state=self.random_state)
        logger.info(f"Split data into Train ({len(train_df):,} rows) and Test ({len(test_df):,} rows).")
        return train_df, test_df
