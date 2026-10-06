# Work Package 0 (WP0) Fix Report: Architecture Restructuring & Layering Remediation

> **Work Package**: WP0 (Restructure)  
> **Branch**: `review-fixes`  
> **Status**: Completed  
> **Date**: 2026-10-06  

---

## 1. Summary of Changes & Fixes Addressed

This work package establishes clean architectural layering, removes cross-layer inverted dependencies, centralizes MLflow lifecycle management under `core/tracking/`, extracts a top-level `evaluation/` package, and introduces an automated AST boundary enforcement test (`tests/test_architecture.py`).

| Fix ID | Description | Status | Verification Evidence |
| :--- | :--- | :--- | :--- |
| **Fix 6.6** | S3 / MinIO storage endpoint configuration & tracker isolation | **DONE** | Added `MINIO_HOST`, `S3_ENDPOINT_URL`, `MLFLOW_S3_ENDPOINT_URL`, and dynamic `s3_endpoint_url` property in `core/config.py`. Created `core/tracking/client.py` setting `os.environ["MLFLOW_S3_ENDPOINT_URL"]`. Purged all hardcoded `http://localhost:9000` instances. |
| **Fix 7.5** | Decouple bot embedding import from `ml/pipelines/seed_curated.py` | **DONE** | Moved `ai/services/embedding_service.py` to `core/embeddings/service.py`. Rewired `ml/pipelines/seed_curated.py` and `ai/services/` to consume `core.embeddings`. |
| **Fix 8.4** | Decouple serving imports from `ml/models/onnx_exporter.py` | **DONE** | Moved `apps/api/serving/predictor.py` and `apps/api/serving/imputer.py` to `ml/serving/`. Rewired `ml/models/onnx_exporter.py`, `ml/pipelines/monitor.py`, `ml/pipelines/retrain.py`, and `apps/api/services/model_service.py`. |
| **Fix 5.3 (Structural)** | Relocate rate limiter to eliminate `apps/api/core/` package shadowing | **DONE** | Moved `apps/api/core/rate_limiter.py` to `apps/api/rate_limiting/rate_limiter.py`. Deleted directory `apps/api/core/`. Updated `apps/api/services/rate_limiter_service.py` and `tests/test_phase1_infra_frontend.py`. |
| **Fix 10.6** | Relocate router benchmark harness out of `ml/eval/` | **DONE** | Moved `ml/eval/benchmark_router.py` and its artifacts to `evaluation/ai/`. Rewired to `core.tracking`. |
| **Evaluation Separation** | Top-level `evaluation/` package for ML and AI benchmarks | **DONE** | Moved `ml/models/evaluate.py` to `evaluation/ml/evaluate.py`. Created runnable CLI entry points `python -m evaluation.ml.evaluate` and `python -m evaluation.ai.benchmark_router`. Added `eval`, `eval-ml`, `eval-ai` targets to `Makefile`. |
| **Architecture Gate** | Automated AST-based architecture scan in `tests/test_architecture.py` | **DONE** | Validates dependency rules: `core/` imports nothing outside core; `ml/` imports core only; `ai/` imports core only; `apps/` imports core/ml/ai; `import mlflow` strictly allowed only in `core/tracking/`. `pytest tests/test_architecture.py` PASSED (1/1). |

---

## 2. Verification & Test Evidence

1. **Import Smoke Verification**:
   ```bash
   python -c "import core; import ml; import ai; import evaluation; import apps.api.main; print('Import smoke successful!')"
   ```
   **Result**: Success (exit code 0). All top-level packages initialize cleanly.

2. **AST Architecture Quality Gate**:
   ```bash
   pytest tests/test_architecture.py -v
   ```
   **Result**: `1 passed in 0.94s` (exit code 0). Zero forbidden imports; zero `mlflow` imports outside `core/tracking/`.

3. **Standalone Evaluation Entry Points**:
   ```bash
   python -m evaluation.ml.evaluate
   ```
   **Result**: Success (exit code 0). Standalone evaluation verification outputs:
   - `rmse: 6.6144`
   - `r2: 0.986`
   - `mae: 6.25`
   - `mse: 43.75`

   ```bash
   python -c "import evaluation.ai.benchmark_router; print('benchmark_router imported successfully')"
   ```
   **Result**: Success (exit code 0). Router benchmark harness imports and configures correctly.

4. **Full Pytest Suite vs. Baseline**:
   - Baseline (`baseline_tests.txt`): `2 failed, 78 passed` (pre-existing Redis cache collision failures in `test_phase3_langgraph_agent.py`).
   - Current run: `2 failed, 79 passed` (78 baseline passed + 1 new passed `test_architecture.py`).
   - **Zero new failures introduced**.

5. **Docker Compose Configuration**:
   ```bash
   docker compose config
   ```
   **Result**: Success (exit code 0). Validates syntax and volume mounts (`- ./evaluation:/app/evaluation`).

6. **Target Removal Grep Checks**:
   ```bash
   git grep -n "apps.api.core"
   git grep -n "ml.eval"
   ```
   **Result**: Both returned exit code 1 (zero matching lines).

---

## 3. Files Changed and Moved

Refer to [RESTRUCTURE_MAP.md](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/RESTRUCTURE_MAP.md) for the complete path mapping.

### Files Moved:
- `ml/eval/benchmark_router.py` -> `evaluation/ai/benchmark_router.py`
- `ml/eval/artifacts/*` -> `evaluation/ai/artifacts/*`
- `ml/models/evaluate.py` -> `evaluation/ml/evaluate.py`
- `ai/services/embedding_service.py` -> `core/embeddings/service.py`
- `apps/api/serving/predictor.py` -> `ml/serving/predictor.py`
- `apps/api/serving/imputer.py` -> `ml/serving/imputer.py`
- `apps/api/core/rate_limiter.py` -> `apps/api/rate_limiting/rate_limiter.py`

### Files Created:
- `core/tracking/client.py`
- `core/tracking/__init__.py`
- `core/embeddings/__init__.py`
- `ml/serving/__init__.py`
- `evaluation/__init__.py`
- `evaluation/ml/__init__.py`
- `evaluation/ai/__init__.py`
- `apps/api/rate_limiting/__init__.py`
- `ai/observability/mlflow_tracer.py` (shim)
- `tests/test_architecture.py`
- `baseline_tests.txt`
- `RESTRUCTURE_MAP.md`
- `FIX_REPORT_wp0.md`

### Files Updated (Imports / Config):
- `core/config.py`: Added `MINIO_HOST`, `S3_ENDPOINT_URL`, `MLFLOW_S3_ENDPOINT_URL`, `s3_endpoint_url`.
- `ml/tracking/mlflow_tracker.py`: Rewired to `core.tracking`.
- `ai/observability/tracing.py`: Rewired to `core.tracking`.
- `evaluation/ai/benchmark_router.py`: Rewired to `core.tracking`, updated artifacts path.
- `evaluation/ml/evaluate.py`: Added `__main__` entry point.
- `ml/pipelines/train.py`: Updated imports to `evaluation.ml` and `ml.serving`.
- `ml/pipelines/retrain.py`: Updated imports to `evaluation.ml` and `ml.serving`.
- `ml/pipelines/preprocess.py`: Updated imports to `evaluation.ml`.
- `ml/pipelines/monitor.py`: Updated imports to `ml.serving`.
- `ml/pipelines/seed_curated.py`: Updated imports to `core.embeddings`.
- `ml/models/__init__.py`: Removed `ModelEvaluator`.
- `ml/models/onnx_exporter.py`: Updated imports to `ml.serving`.
- `apps/api/serving/__init__.py`: Updated imports to `ml.serving`.
- `apps/api/services/model_service.py`: Updated imports to `ml.serving`.
- `apps/api/services/rate_limiter_service.py`: Updated imports to `apps.api.rate_limiting`.
- `ai/services/__init__.py`: Updated imports to `core.embeddings`.
- `ai/services/cache_service.py`: Updated imports to `core.embeddings`.
- `ai/services/search_service.py`: Updated imports to `core.embeddings`.
- `tests/test_models.py`: Updated imports to `ml.serving`.
- `tests/test_phase1_infra_frontend.py`: Updated imports to `apps.api.rate_limiting`.
- `docker/Dockerfile.api`: Added `COPY evaluation/ /app/evaluation/`.
- `docker-compose.yml`: Added `./evaluation:/app/evaluation` volume.
- `Makefile`: Added `eval`, `eval-ml`, `eval-ai` targets and updated `lint`.

---

## 4. Key Architectural Decisions

1. **MLflow Isolation in `core/tracking/`**: `mlflow` is now imported strictly within `core/tracking/`. Any module needing experiment management or tracing imports from `core.tracking`. All hardcoded MinIO URLs (`http://localhost:9000`) were replaced with dynamic resolution from `core.config.settings`.
2. **Behavioral Invariance in WP0**: In accordance with the instructions, behavior was kept unchanged. Training pipelines (`train.py`, `retrain.py`, `preprocess.py`) import `ModelEvaluator` from `evaluation.ml`. These 3 call sites are allow-listed in `tests/test_architecture.py` until later work packages decouple evaluation logic from training.
3. **Serving Runtimes Moved to `ml/serving/`**: `ONNXPredictor` and `ONNXMissForestImputer` are ML model inference runtimes. Moving them to `ml/serving/` adheres to Dependency Inversion: presentation layers (`apps/`) may depend on `ml/`, but `ml/` never imports `apps/`.
4. **Backward-Compatibility Shims**: Lightweight re-export shims were placed in `ai/services/embedding_service.py`, `apps/api/serving/predictor.py`, and `apps/api/serving/imputer.py` (marked with `# TODO-remove`) so external callers or in-flight branches continue functioning seamlessly.

---

## 5. Noticed but Not Fixed (Out of Scope for WP0)

1. **Pre-existing LangGraph Redis Test Failures**: `test_phase3_langgraph_agent.py::test_search_agent_node_hybrid_execution` and `test_details_agent_node_retrieval` fail because Redis has active cached keys for those queries from previous runs, triggering fast-path routing directly to `END`. Fix scheduled for subsequent WPs.
2. **MissForest Target Leakage in evaluate.py**: `evaluate.py:155` retains `"purchase"` in `masked_test` (Fix 6.5). Scheduled for WP11.
3. **Champion-Challenger Delta Sign**: Inverted minus sign in `ml/pipelines/retrain.py:279` (Fix 2.4). Scheduled for Batch 2.
4. **RateLimiter Atomic Operations**: Redis rate limiter check-and-increment operations are not currently atomic (Fix 5.3 logic). Scheduled for WP5.
