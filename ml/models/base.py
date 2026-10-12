from abc import ABC, abstractmethod
from typing import Any, Dict

import numpy as np
import pandas as pd


class AbstractBaseModel(ABC):
    """Abstract Base Class enforcing Strategy Pattern for all ML estimators."""

    @abstractmethod
    def fit(self, X: pd.DataFrame, y: pd.Series) -> "AbstractBaseModel":
        """Fits the underlying regression estimator."""

    @abstractmethod
    def predict(self, X: pd.DataFrame) -> np.ndarray:
        """Generates predictions on new data."""

    @abstractmethod
    def get_params(self) -> Dict[str, Any]:
        """Returns hyperparameters dictionary."""
