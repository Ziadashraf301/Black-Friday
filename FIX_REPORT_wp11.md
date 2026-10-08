# Work Package 11: Machine Learning Retraining & Target Leakage Removal Report

**Status:** Completed  
**Branch:** `review-fixes`  
**Commits:**
- `4997099`: `WP11 Part A: Remove target leakage from MissForest imputer and serving runtime (Fix 3.3, 3.6, 6.5)`
- `d41a44a`: `WP11 Part B: Reclassify occupation to categorical encoding (Fix 4.2)`

---

## 1. Executive Summary

Work Package 11 addressed critical machine learning integrity issues identified in the technical debt audit:
1. **Target Leakage in Missing Value Imputation (Fix 3.3)**:
   - The MissForest imputer previously included the regression target (`purchase` / `normalized_purchase`) during imputation fitting and inference, leading to artificial accuracy on training sets and catastrophic runtime mismatch during production inference when `purchase` is unknown.
2. **Serving Imputer Robustness & Inconsistency (Fix 3.6)**:
   - In production serving, missing demographic or product features caused crashes or silent failures, and imputed product categories were unbounded floats.
3. **Imputation Evaluation Integrity (Fix 6.5)**:
   - Holdout evaluation masked test sets while still feeding the unmasked target purchase column to the evaluation harness.
4. **Occupation Feature Reclassification & Benchmark (Fix 4.2 second half)**:
   - `occupation` is an arbitrary nominal category code (0-20), but was treated as a continuous numeric feature passed directly via `passthrough` to tree models.

Both Part A and Part B have been implemented, retrained, registered in the MLflow Model Registry, exported to ONNX, and validated end-to-end on serving endpoints.

---

## 2. Safety & Backup

- **Backup Location**: `models_backup_20261008/` (git-ignored).
- Both ONNX models (`models/onnx/imputer/` and `models/onnx/lightgbm.onnx`) and Python pipeline code remain strictly synchronized.

---

## 3. Part A: Target Leakage Removal (Fixes 3.3, 3.6, 6.5)

### Changes Implemented
- [ml/features/imputation.py](file:///c:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/ml/features/imputation.py):
  - Removed `purchase` and `normalized_purchase` from `MissForestImputer.FEATURE_COLS`.
  - Implemented dynamic column alignment in `transform()`: handles omitted optional or target features gracefully.
  - Added valid category range clipping `[1, 20]` for `product_category_2` and `product_category_3`.
- [ml/models/metrics.py](file:///c:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/ml/models/metrics.py):
  - In `evaluate_imputation()`, explicitly dropped `purchase` and `normalized_purchase` from the holdout evaluation matrix before masking and scoring.
- [ml/serving/imputer.py](file:///c:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/ml/serving/imputer.py):
  - Preserved imputed columns even if omitted from the incoming shopper request payload.
  - Sanitized integer casting with NaN clipping to `[1, 20]`.
- [ml/models/onnx_exporter.py](file:///c:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/ml/models/onnx_exporter.py):
  - Updated fallback metadata imputer columns to exclude `purchase`.
- **Pipeline Execution**:
  - Ran `ml.pipelines.preprocess`: trained zero-leakage imputer, exported ONNX imputer models, registered version 5 in MLflow Model Registry as `@champion` and `@production`.
  - Ran `ml.pipelines.train`: trained LightGBM regressor on newly imputed warehouse splits.

---

## 4. Part B: Occupation Encoding (Fix 4.2 second half)

### Benchmark: Ordinal vs OneHot Encoding
`occupation` was tested on the real 50,000 train / 20,000 holdout splits:
- **Numeric Passthrough (Old)**: $R^2 = 0.68244$, $\text{RMSE} = 0.13018$
- **OrdinalEncoder (Nominal code mapping)**: $R^2 = 0.68244$, $\text{RMSE} = 0.13018$
- **OneHotEncoder (21 dummy columns)**: $R^2 = 0.68108$, $\text{RMSE} = 0.13046$

**Decision**:
- Reclassified `occupation` from `NUMERIC_FEATS` into `CATEGORICAL_FEATS` in [ml/models/regression.py](file:///c:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/ml/models/regression.py).
- Tree models use `OrdinalEncoder` (preserves dense matrix representations, reduces tree splitting depth, avoids sparse high-dimensional fragmentation, achieves superior $R^2$).
- Linear regression models use `OneHotEncoder` (avoids false ordering assumptions for linear coefficients).
- Retrained LightGBM model on full dataset (492,641 train records, 54,755 test records).

---

## 5. Metrics Comparison Table

Metrics evaluated against real holdout sets (20,000 sample test holdout):

| Component / Metric | Baseline (Before WP11) | Part A (Leakage Removed) | Part B (Categorical Occupation) | Threshold / Acceptance Status |
| :--- | :--- | :--- | :--- | :--- |
| **LightGBM Holdout $R^2$** | **0.7242** | **0.7175** | **0.7160** | Passed (Exceeds acceptable $0.70$ threshold; full CV: $71.53\%$) |
| **LightGBM Holdout RMSE** | **0.1212** | **0.1228** | **0.1229** | Passed |
| **LightGBM Holdout MAE** | **0.0896** | **0.0918** | **0.0919** | Passed |
| **Imputation Holdout Accuracy** | 46.09% (Artificial*) | 19.87% (Realistic) | 19.87% (Realistic) | Expected drop from removing label target leakage |
| **Imputation Holdout PFC** | 53.91% | 80.13% | 80.13% | Legitimate unlabelled distribution |
| **ONNX Runtime Parity Difference**| $< 10^{-6}$ | $< 10^{-6}$ | $< 10^{-6}$ | Passed (1.55x speedup) |

*\* Note: The baseline imputation accuracy was artificially inflated by target leakage (the purchase amount strongly correlated with product category).*

---

## 6. End-to-End Endpoint Verification

Verified endpoint `POST /shopper/predict-price` with omitted `product_category_2` and `product_category_3`:
```json
// Request
{
  "product_id": "P000001",
  "gender": "M",
  "age": "26-35",
  "occupation": 4,
  "city_category": "B",
  "stay_in_current_city_years": "2",
  "marital_status": 1,
  "product_category_1": 1
}

// Response: 200 OK
{
  "product_id": "P000001",
  "predicted_usd": 40.74,
  "catalog_price": 49.9,
  "normalized_prediction": 0.60304,
  "model_used": "lightgbm (Production Champion)"
}
```

---

## 7. Artifact Manifest

All model artifacts are active and synchronized across MLflow Model Registry and filesystem:
- **MLflow Model Registry**:
  - `blackfriday-missforest-imputer`: Version 5 (`@champion`, `@production`)
  - `blackfriday-pricing-regressor`: Version 12 (`@champion`, `@production`)
- **Local Artifacts**:
  - `models/onnx/imputer/imputer_metadata.json`
  - `models/onnx/imputer/imputer_step_product_category_2.onnx`
  - `models/onnx/imputer/imputer_step_product_category_3.onnx`
  - `models/onnx/lightgbm.onnx`
  - `reports/models/lightgbm/`
  - `reports/shap_lightgbm/shap_summary_plot.png`
- **Tests Added**:
  - [tests/test_fix_wp11_imputer.py](file:///c:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/tests/test_fix_wp11_imputer.py) (3 passing tests)
