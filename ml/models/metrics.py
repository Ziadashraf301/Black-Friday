import numpy as np
import pandas as pd
from typing import Dict, Any, List, Optional
from scipy import stats
from sklearn.model_selection import KFold
from sklearn.metrics import mean_squared_error, r2_score
from ml.models.base import AbstractBaseModel
from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)


def evaluate_holdout(y_true: np.ndarray, y_pred: np.ndarray) -> Dict[str, float]:
    """Calculates RMSE, MAE, MSE, and R-squared on holdout set."""
    mse = float(mean_squared_error(y_true, y_pred))
    rmse = float(np.sqrt(mse))
    r2 = float(r2_score(y_true, y_pred))
    mae = float(np.mean(np.abs(y_true - y_pred)))
    return {
        "rmse": round(rmse, 4),
        "r2": round(r2, 4),
        "mae": round(mae, 4),
        "mse": round(mse, 4)
    }


def cross_validate(
    model: AbstractBaseModel,
    X: pd.DataFrame,
    y: pd.Series,
    n_splits: int = 10,
    random_state: int = settings.RANDOM_SEED
) -> Dict[str, Any]:
    """Runs k-fold cross validation mirroring R rsample/yardstick fit_resamples."""
    kf = KFold(n_splits=n_splits, shuffle=True, random_state=random_state)
    fold_rmses: List[float] = []
    fold_r2s: List[float] = []

    logger.info(f"Running {n_splits}-fold cross-validation...")
    for fold, (train_idx, val_idx) in enumerate(kf.split(X), start=1):
        X_train, X_val = X.iloc[train_idx], X.iloc[val_idx]
        y_train, y_val = y.iloc[train_idx], y.iloc[val_idx]

        model.fit(X_train, y_train)
        preds = model.predict(X_val)

        fold_rmse = np.sqrt(mean_squared_error(y_val, preds))
        fold_r2 = r2_score(y_val, preds)

        fold_rmses.append(fold_rmse)
        fold_r2s.append(fold_r2)

    mean_rmse = float(np.mean(fold_rmses))
    std_rmse = float(np.std(fold_rmses))
    mean_r2 = float(np.mean(fold_r2s))
    std_r2 = float(np.std(fold_r2s))

    logger.info(f"CV Results - Mean RMSE: {mean_rmse:.4f} (+/- {std_rmse:.4f}), Mean R2: {mean_r2 * 100:.2f}%")
    return {
        "cv_mean_rmse": round(mean_rmse, 4),
        "cv_std_rmse": round(std_rmse, 4),
        "cv_mean_r2": round(mean_r2, 4),
        "cv_std_r2": round(std_r2, 4),
        "folds": n_splits
    }


def evaluate_fairness(
    model: Any,
    test_df: pd.DataFrame,
    target_col: str = "normalized_purchase"
) -> Dict[str, Any]:
    """Calculates RMSE and R2 across demographic slices (Gender & Age) to evaluate demographic parity."""
    fairness_metrics = {}
    y_true = test_df[target_col].values
    y_pred = model.predict(test_df)

    # 1. Gender Disparity
    gender_slices = {}
    for g in ["M", "F"]:
        mask = test_df["gender"] == g
        if mask.sum() > 0:
            rmse = float(np.sqrt(mean_squared_error(y_true[mask], y_pred[mask])))
            r2 = float(r2_score(y_true[mask], y_pred[mask]))
            gender_slices[g] = {
                "sample_size": int(mask.sum()),
                "rmse": round(rmse, 4),
                "r2": round(r2, 4)
            }
    fairness_metrics["gender_slices"] = gender_slices

    # 2. Age Group Disparity
    age_slices = {}
    for a in sorted(test_df["age"].unique()):
        mask = test_df["age"] == a
        if mask.sum() > 0:
            rmse = float(np.sqrt(mean_squared_error(y_true[mask], y_pred[mask])))
            r2 = float(r2_score(y_true[mask], y_pred[mask]))
            age_slices[a] = {
                "sample_size": int(mask.sum()),
                "rmse": round(rmse, 4),
                "r2": round(r2, 4)
            }
    fairness_metrics["age_slices"] = age_slices

    return fairness_metrics


def evaluate_imputation(y_true: np.ndarray, y_pred: np.ndarray, prefix: str = "test") -> Dict[str, float]:
    """Calculates NRMSE, PFC (Proportion of Falsely Classified), Accuracy, MAE, and RMSE for imputation."""
    mse = float(np.mean((y_true - y_pred) ** 2))
    var_true = float(np.var(y_true))
    nrmse = float(np.sqrt(mse / (var_true + 1e-9)))
    mae = float(np.mean(np.abs(y_true - y_pred)))
    rmse = float(np.sqrt(mse))
    pfc = float(np.mean(y_true != y_pred))
    accuracy = float(1.0 - pfc)
    return {
        f"{prefix}_nrmse": round(nrmse, 4),
        f"{prefix}_pfc": round(pfc, 4),
        f"{prefix}_accuracy": round(accuracy, 4),
        f"{prefix}_mae": round(mae, 4),
        f"{prefix}_rmse": round(rmse, 4),
    }


def evaluate_imputation_holdout(
    imputer: Any,
    test_df: pd.DataFrame,
    sample_size: int = 50000,
    random_state: int = settings.RANDOM_SEED
) -> Dict[str, Any]:
    """Evaluates imputation error on completely observed test holdout records.

    Artificially masks Product_Category_2 and Product_Category_3 to NaN,
    predicts their values using the fitted imputer, and computes NRMSE, PFC,
    Categorical Accuracy, MAE, and RMSE.
    """
    complete_mask = test_df["product_category_2"].notna() & test_df["product_category_3"].notna()
    valid_test = test_df[complete_mask].copy()

    if valid_test.empty:
        logger.warning("No completely observed records found in test dataframe for evaluation.")
        return {}

    if len(valid_test) > sample_size:
        valid_test = valid_test.sample(n=sample_size, random_state=random_state)

    y_true_cat2 = valid_test["product_category_2"].astype(int).to_numpy()
    y_true_cat3 = valid_test["product_category_3"].astype(int).to_numpy()

    masked_test = valid_test.copy()
    masked_test["product_category_2"] = np.nan
    masked_test["product_category_3"] = np.nan
    # Remove target leakage during holdout imputation evaluation (Fix 6.5)
    for target_col in ["purchase", "normalized_purchase"]:
        if target_col in masked_test.columns:
            masked_test.drop(columns=[target_col], inplace=True)

    imputed_test = imputer.transform(masked_test)
    y_pred_cat2 = imputed_test["product_category_2"].to_numpy()
    y_pred_cat3 = imputed_test["product_category_3"].to_numpy()

    m2 = evaluate_imputation(y_true_cat2, y_pred_cat2, "test_cat2")
    m3 = evaluate_imputation(y_true_cat3, y_pred_cat3, "test_cat3")

    mean_nrmse = round((m2["test_cat2_nrmse"] + m3["test_cat3_nrmse"]) / 2.0, 4)
    mean_pfc = round((m2["test_cat2_pfc"] + m3["test_cat3_pfc"]) / 2.0, 4)
    mean_acc = round((m2["test_cat2_accuracy"] + m3["test_cat3_accuracy"]) / 2.0, 4)

    metrics = {
        **m2,
        **m3,
        "test_mean_nrmse": mean_nrmse,
        "test_mean_pfc": mean_pfc,
        "test_mean_accuracy": mean_acc,
        "evaluated_sample_size": float(len(valid_test)),
    }

    logger.info(
        f"Holdout Imputation Evaluation (N={len(valid_test):,}): "
        f"Cat2 NRMSE={m2['test_cat2_nrmse']}, Accuracy={m2['test_cat2_accuracy']:.1%}; "
        f"Cat3 NRMSE={m3['test_cat3_nrmse']}, Accuracy={m3['test_cat3_accuracy']:.1%}"
    )

    return {
        "metrics": metrics,
        "y_true_cat2": y_true_cat2,
        "y_pred_cat2": y_pred_cat2,
        "y_true_cat3": y_true_cat3,
        "y_pred_cat3": y_pred_cat3,
    }


def evaluate_hypothesis_welch(
    sample_a: np.ndarray,
    sample_b: np.ndarray,
    alpha: float = 0.05
) -> Dict[str, Any]:
    """Executes Welch's Two-Sample t-test for unequal variances between two independent groups."""
    res = stats.ttest_ind(sample_a, sample_b, equal_var=False)
    t_stat = float(res.statistic)
    p_val = float(res.pvalue)
    reject = bool(p_val < alpha)

    return {
        "t_statistic": round(t_stat, 4),
        "p_value": p_val,
        "alpha": alpha,
        "reject_null": reject,
        "mean_a": round(float(np.mean(sample_a)), 4),
        "mean_b": round(float(np.mean(sample_b)), 4),
        "diff_in_means": round(float(np.mean(sample_a) - np.mean(sample_b)), 4)
    }


class ModelEvaluator:
    """Centralized evaluation engine for regression performance, cross-validation,
    demographic fairness parity, imputation quality, and statistical hypothesis testing.
    """

    def __init__(self, n_splits: int = 10, random_state: int = settings.RANDOM_SEED):
        self.n_splits = n_splits
        self.random_state = random_state

    def evaluate_holdout(self, y_true: np.ndarray, y_pred: np.ndarray) -> Dict[str, float]:
        return evaluate_holdout(y_true, y_pred)

    def cross_validate(self, model: AbstractBaseModel, X: pd.DataFrame, y: pd.Series) -> Dict[str, Any]:
        return cross_validate(model, X, y, n_splits=self.n_splits, random_state=self.random_state)

    evaluate_fairness = staticmethod(evaluate_fairness)
    evaluate_imputation = staticmethod(evaluate_imputation)
    evaluate_imputation_holdout = staticmethod(evaluate_imputation_holdout)
    evaluate_hypothesis_welch = staticmethod(evaluate_hypothesis_welch)
