from abc import ABC, abstractmethod
import numpy as np
import pandas as pd
from typing import Dict, Any

class AbstractBaseModel(ABC):
    """Abstract Base Class enforcing Strategy Pattern for all ML estimators."""

    @abstractmethod
    def fit(self, X: pd.DataFrame, y: pd.Series) -> "AbstractBaseModel":
        """Fits the underlying regression estimator."""
        pass

    @abstractmethod
    def predict(self, X: pd.DataFrame) -> np.ndarray:
        """Generates predictions on new data."""
        pass

    @abstractmethod
    def get_params(self) -> Dict[str, Any]:
        """Returns hyperparameters dictionary."""
        pass
