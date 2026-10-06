# Architecture Restructuring Map: Black Friday v2

This document records all module moves, extractions, and rewirings performed to eliminate layering violations, isolate evaluation, centralize tracking, and enforce clean architecture boundaries.

## File Moves

| Old Path | New Path | Reason |
| :--- | :--- | :--- |
| `ml/eval/benchmark_router.py` | `evaluation/ai/benchmark_router.py` | Move AI router and guardrail evaluation harness from `ml/` to dedicated top-level `evaluation/ai/` package (Fix 10.6). |
| `ml/eval/artifacts/` | `evaluation/ai/artifacts/` | Maintain benchmark evaluation charts and metrics artifacts alongside their runner script in `evaluation/ai/`. |
| `ml/models/evaluate.py` | `evaluation/ml/evaluate.py` | Decouple model performance and MissForest imputation evaluation into top-level `evaluation/ml/` package. |
| `ai/services/embedding_service.py` | `core/embeddings/service.py` | Promote shared multimodal embedding service to `core/embeddings/` so offline pipelines (`ml/pipelines/seed_curated.py`) do not import conversational bot layer (`ai/`) (Fix 7.5). |
| `apps/api/serving/predictor.py` | `ml/serving/predictor.py` | Relocate ONNX Runtime inference engine from API presentation layer to `ml/serving/` so presentation depends on ML, not the reverse (Fix 8.4). |
| `apps/api/serving/imputer.py` | `ml/serving/imputer.py` | Relocate ONNX MissForest imputer runtime from API presentation layer to `ml/serving/` so presentation depends on ML, not the reverse (Fix 8.4). |
| `apps/api/core/rate_limiter.py` | `apps/api/rate_limiting/rate_limiter.py` | Eliminate nested `apps/api/core` package causing namespace shadowing with top-level `core/` (Fix 5.3). |

## New Packages & Modules Created

1. **`core/tracking/` (`client.py`, `__init__.py`)**:
   - Centralizes MLflow client initialization, tracking URI configuration, and experiment creation.
   - Sets S3/MinIO endpoint environment variables dynamically from `core.config.settings` (`S3_ENDPOINT_URL`, `MLFLOW_S3_ENDPOINT_URL`, `MINIO_HOST`, `MINIO_PORT`), eliminating hardcoded `http://localhost:9000` (Fix 6.6).
   - Provides tracing helpers (`enable_langchain_autolog`, `trace`, `update_current_trace`, `SpanType`).
   - Is the single SSOT in the codebase allowed to perform `import mlflow`.

2. **`core/embeddings/` (`service.py`, `__init__.py`)**:
   - Houses `BaseEmbeddingProvider`, `GeminiEmbeddingProvider`, `DeterministicSemanticProvider`, and `EmbeddingService`.
   - Directly imported by both `core`, `ml`, and `ai` layers without violating layering rules.

3. **`ml/serving/` (`predictor.py`, `imputer.py`, `__init__.py`)**:
   - Owns `ONNXPredictor` and `ONNXMissForestImputer` inference engines.
   - Imported by `apps/api/services/model_service.py` and `ml/models/onnx_exporter.py`.

4. **`evaluation/` (`__init__.py`, `evaluation/ml/`, `evaluation/ai/`)**:
   - Top-level evaluation package completely separate from production training and serving code.
   - Provides runnable entry points: `python -m evaluation.ml.evaluate` and `python -m evaluation.ai.benchmark_router`.

5. **`apps/api/rate_limiting/` (`rate_limiter.py`, `__init__.py`)**:
   - Resolves namespace shadowing of `core/`.

6. **`tests/test_architecture.py`**:
   - AST-based scan enforcing architectural boundary rules across all project modules.
   - Validates that direct `import mlflow` is strictly confined to `core/tracking/`.

## Compatibility Shims Maintained (TODO-remove)

- `ai/services/embedding_service.py`: Re-exports from `core.embeddings`. Marked `TODO-remove`.
- `apps/api/serving/predictor.py`: Re-exports `ONNXPredictor` from `ml.serving.predictor`. Marked `TODO-remove`.
- `apps/api/serving/imputer.py`: Re-exports `ONNXMissForestImputer` from `ml.serving.imputer`. Marked `TODO-remove`.
- `ai/observability/mlflow_tracer.py`: Re-exports `AgentTracer` from `ai.observability.tracing`. Marked `TODO-remove`.
