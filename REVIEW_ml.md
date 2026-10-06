# Session 2 Code Review Report: ML Pipeline & Feature Engineering Domain

> **Scope**: Machine Learning Pipeline, Feature Engineering, Association Mining, Clustering, Tracking, and Governance (`ml/`)  
> **Target Artifact**: `REVIEW_ml.md`  
> **Review Date**: 2026-10-06  
> **Author**: Antigravity Audit Agent  
> **Input Scope**: All files in `ml/` and associated contracts/models  

---

## 1. Executive Summary

A comprehensive, line-by-line static audit of the machine learning and analytics subsystem (`ml/`) was conducted. The domain is architecturally well-structured, featuring clean scikit-learn pipelines, ONNX Runtime acceleration, Evidently AI drift monitoring, and MLflow Model Registry integration.

However, several critical logic defects, statistical validity flaws, and architectural discrepancies were identified:
1. **Critical Target Leakage in MissForest Imputation**: `purchase` is included in the imputer feature set during offline training and holdout evaluation, but is absent at serving time, causing train/serving skew and fatal runtime exceptions during inference.
2. **Critical Logic Inversion in Champion-Challenger Gate**: In `ml/pipelines/retrain.py:190`, the decision condition subtracts `min_improvement_delta` instead of adding it, allowing inferior candidate models with worse R² scores than the champion to be promoted to Production.
3. **Broken Model Alias Resolution in CLI**: `ml/pipelines/train.py` bypasses `ModelRegistry._ALIAS_MAP`, causing `make train` (`--model lgbm`) to crash with `ValueError`.
4. **Target Detection Defect in Drift Monitoring**: `ml/pipelines/monitor.py` verifies target presence against `current_batch_df` instead of the normalized batch `curr_eval`, aborting automated continuous retraining even when valid purchase data is present.
5. **Specification Discrepancies in `REVIEW_HANDOFF.md`**: Several handoff hypotheses were rejected based on the actual codebase (e.g., non-existent files like `champion_challenger.py` and `profiling.py`, missing CatBoost models, and false claims of SQL column check constraints).

---

## 2. Evaluation of Handoff Hypotheses (Touches `ml/`)

Each finding or claim from `REVIEW_HANDOFF.md` touching the `ml/` domain was treated as an unverified hypothesis and audited against the codebase:

### H-01: Target Leakage and Train/Serving Skew in MissForest Imputer
- **Location**: `ml/features/imputation.py:24-36` vs `apps/api/serving/imputer.py:43-56`
- **Severity**: High
- **Status**: **CONFIRMED**
- **Problem**: In `ml/features/imputation.py:35`, `"purchase"` is explicitly included in `MissForestImputer.FEATURE_COLS`. During training, the ExtraTrees regressors learn to impute `product_category_2` and `product_category_3` using the prediction target. At serving time (`apps/api/serving/imputer.py:47-50`), `purchase` is unavailable and filled with the training mean (`self.initial_stats[j]`), introducing severe train/serving skew. Furthermore, calling `MissForestImputer.transform()` directly on unlabelled inference data crashes with `KeyError: 'purchase'`.
- **Concrete Fix**: Exclude `"purchase"` from `FEATURE_COLS` in `ml/features/imputation.py` so imputation relies exclusively on demographic and product features.
```python
# ml/features/imputation.py
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
    "product_id"
    # REMOVED: "purchase"
]
```

---

### H-02: Schema Duplication Between SQL Models and Validation Contracts
- **Location**: `core/db/models/warehouse.py:15-70` vs `ml/features/data_contract.py:18-65`
- **Severity**: Low
- **Status**: **REJECTED**
- **Problem**: The handoff claimed: *"Demographic valid ranges (Gender in M/F, Age brackets, City_Category in A/B/C) are defined independently as SQLAlchemy column constraints and again as Pandera schema checks."*  
In the actual code (`core/db/models/warehouse.py:17-22` and `docker/postgres/init_schema.sql:14-22`), columns are defined as plain `String(1)`, `String(16)`, `Integer` without any `CheckConstraint` or enum restrictions. Range and categorical validation exists solely within `ml/features/data_contract.py`.
- **Concrete Fix**: No deduplication needed for SQL models. However, `CleanedTransactionSchema` in `ml/features/data_contract.py` should be updated to enforce validation on `stay_in_current_city_years` which is currently omitted (see N-05).

---

### H-03: In-Memory Fallback Threading in Drift Scheduler
- **Location**: `ml/pipelines/cron_scheduler.py:61-68`
- **Severity**: Med
- **Status**: **REJECTED (Inaccurate Description)**
- **Problem**: The handoff claimed: *"ml/pipelines/cron_scheduler.py uses a simple Python threading.Thread loop for periodic drift checks."*  
In reality, `ml/pipelines/cron_scheduler.py` contains zero `threading.Thread` instances. `start_daemon_scheduler()` runs a blocking synchronous `while True:` loop with `time.sleep(21600)`. Running `python -m ml.pipelines.cron_scheduler` executes a single synchronous run of `run_cron_cycle()` and terminates.
- **Concrete Fix**: To run as a robust production service, replace the blocking sleep loop with an async task queue (e.g. Celery / ARQ / APScheduler) or container cron orchestrator.
```python
# ml/pipelines/cron_scheduler.py
from apscheduler.schedulers.blocking import BlockingScheduler

def start_daemon_scheduler():
    scheduler = BlockingScheduler()
    scheduler.add_job(run_cron_cycle, "interval", hours=6)
    logger.info("Starting APScheduler 6-hour cron engine...")
    scheduler.start()
```

---

### H-04: Non-Existent Files & Class Signatures in Section 3.2 and Section 8
- **Location**: `ml/models/champion_challenger.py`, `ml/segmentation/profiling.py`, `ml/market_basket/apriori.py`, `ml/market_basket/graph_analytics.py`
- **Severity**: Med
- **Status**: **REJECTED**
- **Problem**: The handoff document documented several non-existent files and interfaces:
  - Claimed: `ml/models/champion_challenger.py` & `ChampionChallengerComparator.compare_and_promote()`.  
    *Reality*: Champion promotion is implemented in `ml/pipelines/retrain.py` (`run_champion_challenger_retrain`) and `ml/tracking/mlflow_tracker.py` (`promote_to_champion`), while model factory is in `ml/models/registry.py`.
  - Claimed: `ml/segmentation/profiling.py`.  
    *Reality*: Persona profiling is embedded in `ml/segmentation/clustering.py`.
  - Claimed: `ml/market_basket/apriori.py` and `ml/market_basket/graph_analytics.py`.  
    *Reality*: Actual files are `ml/market_basket/apriori_engine.py` and `ml/market_basket/network_graph.py`.
  - Claimed: `ml/models/regression.py` includes CatBoost regressor wrappers.  
    *Reality*: Only `LinearRegressionModel`, `DecisionTreeModel`, `RandomForestModel`, and `LightGBMModel` exist.
- **Concrete Fix**: Align project documentation and run guides with actual repository filenames and classes.

---

## 3. Comprehensive Line-by-Line Review Findings (New Issues)

```
Format: file:line | severity (High/Med/Low) | CONFIRMED/REJECTED/NEW | problem | concrete fix with code
```

---

### [N-01] Critical Logic Inversion in Champion-Challenger Decision Gate
- **Location**: `ml/pipelines/retrain.py:190`
- **Severity**: **High**
- **Status**: **NEW**
- **Problem**: In `run_champion_challenger_retrain()`, line 190 checks:
  ```python
  beats_champion_margin = candidate_r2 >= (champion_r2 - min_improvement_delta)
  ```
  `settings.CHAMPION_MIN_IMPROVEMENT_DELTA` is a positive float (`0.0050`). Subtracting `min_improvement_delta` means that a challenger performing **worse** than the active champion by up to 0.0050 R² will evaluate to `True` and be promoted as the new production champion! The challenger must **exceed** the champion by at least `min_improvement_delta` (`champion_r2 + min_improvement_delta`).
- **Concrete Fix**:
```python
# ml/pipelines/retrain.py:190
# BEFORE:
beats_champion_margin = candidate_r2 >= (champion_r2 - min_improvement_delta)

# AFTER:
beats_champion_margin = candidate_r2 >= (champion_r2 + min_improvement_delta)
```

---

### [N-02] CLI `--model` Filter Bypasses ModelRegistry Alias Map
- **Location**: `ml/pipelines/train.py:43-46`
- **Severity**: **High**
- **Status**: **NEW**
- **Problem**: When `python -m ml.pipelines.train --model lgbm` (the exact command specified in the project `Makefile` line 77) is executed:
  ```python
  available_model_names = ModelRegistry.list_available_models()  # ['linear_regression', 'decision_tree', 'random_forest', 'lightgbm']
  if model_names:
      available_model_names = [m for m in model_names if m in available_model_names]
      if not available_model_names:
          raise ValueError(f"Requested models {model_names} not found...")
  ```
  `"lgbm"` is in `ModelRegistry._ALIAS_MAP`, but not in `_MODELS.keys()`. Because `train.py` does not resolve aliases via `ModelRegistry.resolve_name(m)`, `available_model_names` resolves to empty `[]` and immediately raises a `ValueError`.
- **Concrete Fix**:
```python
# ml/pipelines/train.py:43-46
# BEFORE:
if model_names:
    available_model_names = [m for m in model_names if m in available_model_names]

# AFTER:
if model_names:
    resolved_names = [ModelRegistry.resolve_name(m) for m in model_names]
    available_model_names = [m for m in resolved_names if m in available_model_names]
```

---

### [N-03] Ground-Truth Target Verification Checks Wrong DataFrame in Drift Monitor
- **Location**: `ml/pipelines/monitor.py:198-200`
- **Severity**: **High**
- **Status**: **NEW**
- **Problem**: The decision gate checking whether the new batch can trigger supervised retraining performs:
  ```python
  target_col = "normalized_purchase"
  has_target = target_col in current_batch_df.columns and current_batch_df[target_col].notna().any()
  ```
  Incoming production batches contain raw `"purchase"` in USD. In lines 82-84, `"normalized_purchase"` is calculated and added to `curr_eval`, but line 199 checks raw `current_batch_df`. As a result, `has_target` evaluates to `False`, logging a warning that labels are missing and aborting continuous retraining even though labels were present in the batch.
- **Concrete Fix**:
```python
# ml/pipelines/monitor.py:198-200
# BEFORE:
target_col = "normalized_purchase"
has_target = target_col in current_batch_df.columns and current_batch_df[target_col].notna().any()

# AFTER:
target_col = "normalized_purchase"
has_target = (target_col in curr_eval.columns and curr_eval[target_col].notna().any()) or \
             ("purchase" in current_batch_df.columns and current_batch_df["purchase"].notna().any())
```

---

### [N-04] MissForest Inference Crash on Unlabelled Data
- **Location**: `ml/features/imputation.py:89, 180-184`
- **Severity**: **High**
- **Status**: **NEW**
- **Problem**: In `_prepare_numeric_matrix()`, `self.cols` is saved during `is_fit=True` (which includes `"purchase"`). When `imputer.transform(df)` is called on a real unlabelled inference batch where `purchase` is not provided:
  ```python
  matrix = df[self.cols].copy()
  ```
  Pandas raises `KeyError: "['purchase'] not in index"`.
- **Concrete Fix**: In addition to removing `purchase` from `FEATURE_COLS` (H-01), allow `transform` to safely extract only features that the imputer model was configured to predict from:
```python
# ml/features/imputation.py:89
# BEFORE:
matrix = df[self.cols].copy()

# AFTER:
missing_cols = [c for c in self.cols if c not in df.columns]
if missing_cols and not is_fit:
    matrix = df[[c for c in self.cols if c in df.columns]].copy()
    for col in missing_cols:
        matrix[col] = np.nan
else:
    matrix = df[self.cols].copy()
```

---

### [N-05] Missing Column Validation in Cleaned Data Contract
- **Location**: `ml/features/data_contract.py:29-48`
- **Severity**: **Med**
- **Status**: **NEW**
- **Problem**: `CleanedTransactionSchema` defines schema checks for `user_id`, `product_id`, `gender`, `age`, `occupation`, `city_category`, `marital_status`, `product_category_1..3`, `purchase`, `is_outlier`, `normalized_purchase`, `split`.  
It completely omits `stay_in_current_city_years`! However, the database table `black_friday_cleaned` (`core/db/models/warehouse.py:40`) specifies `stay_in_current_city_years VARCHAR(8) NOT NULL`. Because `strict=False` on the Pandera contract, validation passes, but any null or corrupted values in `stay_in_current_city_years` slip through unvalidated into the database.
- **Concrete Fix**:
```python
# ml/features/data_contract.py:37
CleanedTransactionSchema = DataFrameSchema(
    {
        # ...
        "city_category": Column(str, Check.isin(["A", "B", "C"]), nullable=False),
        "stay_in_current_city_years": Column(str, Check.isin(["0", "1", "2", "3", "4+"]), nullable=False),
        "marital_status": Column(int, Check.isin([0, 1]), nullable=False),
        # ...
    },
    coerce=True,
    strict=False
)
```

---

### [N-06] Repetitive Preprocessor Definition in Regression Models (DRY Violation)
- **Location**: `ml/models/regression.py:85-91, 158-164, 224-230`
- **Severity**: **Med**
- **Status**: **NEW**
- **Problem**: `DecisionTreeModel`, `RandomForestModel`, and `LightGBMModel` all duplicate the exact same `ColumnTransformer` block verbatim across 20+ lines:
  ```python
  self.preprocessor = ColumnTransformer(
      transformers=[
          ("num", "passthrough", NUMERIC_FEATS),
          ("cat", OrdinalEncoder(handle_unknown="use_encoded_value", unknown_value=-1), CATEGORICAL_FEATS + ["product_id"])
      ],
      remainder="drop"
  )
  ```
- **Concrete Fix**: Extract a reusable transformer factory method:
```python
# ml/models/regression.py
def create_tree_preprocessor() -> ColumnTransformer:
    return ColumnTransformer(
        transformers=[
            ("num", "passthrough", NUMERIC_FEATS),
            ("cat", OrdinalEncoder(handle_unknown="use_encoded_value", unknown_value=-1), CATEGORICAL_FEATS + ["product_id"])
        ],
        remainder="drop"
    )

# Inside DecisionTreeModel, RandomForestModel, LightGBMModel:
self.preprocessor = create_tree_preprocessor()
```

---

### [N-07] Questionable Numeric Encoding for Discrete Nominal Features
- **Location**: `ml/models/regression.py:37`
- **Severity**: **Med**
- **Status**: **NEW**
- **Problem**: `NUMERIC_FEATS` is defined as:
  ```python
  NUMERIC_FEATS: List[str] = ["occupation", "marital_status", "product_category_1", "product_category_2", "product_category_3"]
  ```
  `occupation` (values 0–20) represents nominal occupational categories with no natural ordering. Passing it through unencoded treats occupation 19 as "greater than" occupation 2. Decision trees and linear models treat these as continuous numerical magnitudes, potentially creating suboptimal or arbitrary split boundaries.
- **Concrete Fix**: Include nominal categories like `occupation` in categorical encoders (e.g. `OrdinalEncoder` or target encoding):
```python
# ml/models/regression.py
NUMERIC_FEATS: List[str] = ["product_category_1", "product_category_2", "product_category_3"]
CATEGORICAL_FEATS: List[str] = ["gender", "age", "city_category", "stay_in_current_city_years", "occupation", "marital_status"]
```

---

### [N-08] Inverted Architectural Dependency (DIP / Clean Architecture Violation)
- **Location**: `ml/models/onnx_exporter.py:12-13` and `ml/pipelines/seed_curated.py:12`
- **Severity**: **Med**
- **Status**: **NEW**
- **Problem**:
  1. `ml/models/onnx_exporter.py` imports directly from the API presentation layer:
     ```python
     from apps.api.serving.imputer import ONNXMissForestImputer
     from apps.api.serving.predictor import ONNXPredictor
     ```
  2. `ml/pipelines/seed_curated.py` imports from the assistant conversational bot subsystem:
     ```python
     from ai.services.embedding_service import embedding_service, EmbeddingService
     ```
  In clean layered architecture, the core domain and offline training layer (`ml/`) must never depend on the HTTP serving layer (`apps/api/`) or conversational bot application layer (`ai/`). If `apps/api` dependencies fail to load or have import cycles, offline model training and ONNX export will crash.
- **Concrete Fix**: Move `ONNXPredictor` into a shared runtime module under `ml/serving/` or `core/serving/`, and inject the embedder into `seed_curated.py` as an interface rather than a concrete import.

---

### [N-09] Hardcoded MinIO S3 Host Breaks Containerized Pipelines
- **Location**: `ml/tracking/mlflow_tracker.py:32` and `ml/eval/benchmark_router.py:28`
- **Severity**: **Med**
- **Status**: **NEW**
- **Problem**: Both files execute:
  ```python
  os.environ.setdefault("MLFLOW_S3_ENDPOINT_URL", "http://localhost:9000")
  ```
  Inside a Docker container (where MinIO runs as a service named `minio`), `localhost:9000` refers to the local container, causing S3 artifact uploads and model downloads (`tracker.download_champion_onnx()`) to fail with connection errors.
- **Concrete Fix**: Read from `settings.S3_ENDPOINT_URL` (or `settings.MINIO_ENDPOINT`):
```python
# ml/tracking/mlflow_tracker.py:32
endpoint_url = os.getenv("MLFLOW_S3_ENDPOINT_URL", f"http://{settings.MINIO_HOST if hasattr(settings, 'MINIO_HOST') else 'localhost'}:9000")
os.environ.setdefault("MLFLOW_S3_ENDPOINT_URL", endpoint_url)
```

---

### [N-10] Target Leakage in Holdout Imputation Evaluation
- **Location**: `ml/models/evaluate.py:155-160`
- **Severity**: **Med**
- **Status**: **NEW**
- **Problem**: `ModelEvaluator.evaluate_imputation_holdout()` artificially masks `product_category_2` and `product_category_3` to `np.nan` on unseen test data, and calls `imputer.transform(masked_test)`. However, `masked_test` retains the ground-truth target column `purchase`. Because `MissForestImputer` uses `purchase` to predict categories, the computed evaluation metrics (NRMSE, PFC, Accuracy) are artificially inflated by target leakage.
- **Concrete Fix**: Mask or remove `purchase` from `masked_test` during holdout imputation evaluation:
```python
# ml/models/evaluate.py:158
masked_test = valid_test.copy()
masked_test["product_category_2"] = np.nan
masked_test["product_category_3"] = np.nan
if "purchase" in masked_test.columns:
    masked_test.drop(columns=["purchase"], inplace=True)
```

---

### [N-11] Unbounded Category Rounding in Imputer
- **Location**: `ml/features/imputation.py:146-147, 189-190`
- **Severity**: **Low**
- **Status**: **NEW**
- **Problem**: Imputed values are rounded via `np.round(imputed_df["product_category_2"]).astype(int)` without boundary clipping. If the regression tree predicts a float outside the valid category domain (e.g. 0.2 or 22.4), rounding produces invalid product category IDs.
- **Concrete Fix**:
```python
# ml/features/imputation.py:146-147
result_df["product_category_2"] = np.clip(np.round(imputed_df["product_category_2"]), 1, 20).astype(int)
result_df["product_category_3"] = np.clip(np.round(imputed_df["product_category_3"]), 1, 20).astype(int)
```

---

### [N-12] Dead Code / Misleading Docstring in Cron Scheduler
- **Location**: `ml/pipelines/cron_scheduler.py:5, 23-58`
- **Severity**: **Low**
- **Status**: **NEW**
- **Problem**: The docstring claims: *"flushes 6-hour Redis caches every 6 hours"*. However, `run_cron_cycle()` contains zero Redis client imports, cache managers, or flush commands.
- **Concrete Fix**: Either implement the Redis cache invalidation call or remove the misleading docstring statement:
```python
# ml/pipelines/cron_scheduler.py:55
from core.cache.redis_client import cache_manager
if cache_manager.is_available:
    cache_manager.flush_by_pattern("bot:*")
    logger.info("Flushed 6-hour bot response Redis cache.")
```

---

### [N-13] Incomplete Public Package Exports in `ml/models/__init__.py`
- **Location**: `ml/models/__init__.py:12-21`
- **Severity**: **Low**
- **Status**: **NEW**
- **Problem**: `ModelRegistry` is the primary entry point for all model instantiations across pipelines, but it is not imported or exported in `ml/models/__init__.py` (`__all__`). Consumers are forced to import from the private submodule `from ml.models.registry import ModelRegistry`.
- **Concrete Fix**:
```python
# ml/models/__init__.py
from ml.models.registry import ModelRegistry

__all__ = [
    "AbstractBaseModel",
    "LinearRegressionModel",
    "DecisionTreeModel",
    "RandomForestModel",
    "LightGBMModel",
    "ModelEvaluator",
    "ONNXExporter",
    "ModelRegistry",  # ADDED
]
```

---

### [N-14] Model Card Feature Documentation Discrepancy
- **Location**: `ml/tracking/model_card.py:31`
- **Severity**: **Low**
- **Status**: **NEW**
- **Problem**: The auto-generated Model Card documents:
  `- **Input Features:** product_category_1, product_category_2, product_category_3, product_id` (4 features).  
  In reality, all trained models in `ml/models/regression.py` train on 10 features (`gender`, `age`, `occupation`, `city_category`, `stay_in_current_city_years`, `marital_status`, `product_category_1..3`, `product_id`).
- **Concrete Fix**:
```python
# ml/tracking/model_card.py:31
- **Input Features:** `gender`, `age`, `occupation`, `city_category`, `stay_in_current_city_years`, `marital_status`, `product_category_1`, `product_category_2`, `product_category_3`, `product_id` (10 demographic & catalog features)
```

---

## 4. Top 5 Refactors Ranked by Impact vs Effort

| Rank | Refactor Description | Target Files | Impact | Effort | Rationale |
| :---: | :--- | :--- | :---: | :---: | :--- |
| **1** | **Fix Champion-Challenger Decision Gate Logic** | `ml/pipelines/retrain.py:190` | **CRITICAL** | **LOW** (1 line) | Reversing `-` to `+` prevents degrading the production serving model with inferior models during continuous retraining. |
| **2** | **Eliminate Target Leakage from MissForest Imputer** | `ml/features/imputation.py:24-36`, `ml/models/evaluate.py:155` | **CRITICAL** | **LOW** (~5 lines) | Eliminates target leakage, resolves train/serving skew, and prevents `KeyError` crashes on inference batches. |
| **3** | **Fix Ground-Truth Target Check in Drift Retrain Trigger** | `ml/pipelines/monitor.py:198-202` | **HIGH** | **LOW** (2 lines) | Allows incoming batches with raw `purchase` column to properly trigger the continuous training loop. |
| **4** | **Support Model Aliases in Training CLI** | `ml/pipelines/train.py:43-46` | **HIGH** | **LOW** (2 lines) | Restores compatibility with `make train` (`--model lgbm`) and `ModelRegistry._ALIAS_MAP`. |
| **5** | **Decouple ML Core from Serving/AI Subsystems (DIP)** | `ml/models/onnx_exporter.py`, `ml/pipelines/seed_curated.py` | **MEDIUM** | **MEDIUM** (~30 lines) | Removes cross-layer imports from `apps/api` and `ai/` into `ml/`, preventing circular dependencies and container startup failures. |

---

## 5. Cross-Domain Dependencies for Future Sessions

The following dependencies and integration points must be validated during subsequent review sessions:

1. **Session 1 (Core Domain)**:
   - Check `CustomerSegment` ORM model in `core/db/models/warehouse.py:51-65`: it is missing `purchase_amount_variability`, `popular_category`, and `age_group` that are generated by `CustomerFeatureExtractor` and present in PostgreSQL schema `init_schema.sql:65-69`.
   - Add `MLFLOW_S3_ENDPOINT_URL` and `MINIO_HOST` to `core/config.py` settings to eliminate hardcoded `localhost:9000` in tracking clients.
2. **Session 3 (Backend REST API & Serving)**:
   - Check `apps/api/serving/imputer.py:47-50`: when `purchase` is removed from `MissForestImputer.FEATURE_COLS`, the serving imputer ONNX graph and preprocessing logic must also drop `purchase` dummy fills.
   - Verify `apps/api/serving/predictor.py`: ensure feature list and types match `ALL_FEATURES` in `ml/models/regression.py`.
3. **Session 4 (AI Subsystem)**:
   - Review `ai/services/embedding_service.py` consumed by `ml/pipelines/seed_curated.py:12`. Refactor vector embedding generation into a shared utility or repository concern.
   - Review `ml/eval/benchmark_router.py`: this file evaluates `ai/router/` and `ai/extractor/` models. Determine whether router benchmarking belongs in `ai/eval/` rather than `ml/eval/`.
4. **Session 6 (Tests & CI/CD)**:
   - Review `tests/test_drift_retrain.py` and `tests/test_features.py`: update mock tests once `purchase` is removed from `MissForestImputer.FEATURE_COLS` and `beats_champion_margin` is corrected.
