# WP0: Restructure (runs FIRST, alone)

Read prompts/fix_common.md and follow it. Then read FIX_PLAN.md, REVIEW_ml.md, REVIEW_ai.md, REVIEW_backend.md, REVIEW_core.md.

Goal: put shared things where every domain can use them, separate evaluation, and remove cross-layer imports. Behaviour must NOT change in this WP: it is moves + import updates only (plus the small config additions noted). Run the baseline tests first and save the result as baseline_tests.txt if it does not exist.

## Moves to perform
1. **Shared MLflow -> core/tracking/.** Create core/tracking/ with a client module that owns: tracking URI, S3/MinIO endpoint env setup (read from core.config settings; add S3_ENDPOINT_URL / MLFLOW_S3_ENDPOINT_URL to core/config.py if missing; this is Fix 6.6), experiment creation helper, client factory, and tracing helpers. Rewire ml/tracking/mlflow_tracker.py and ai/observability/mlflow_tracer.py (and every other direct mlflow setup in ml/ pipelines or ai/) to use core.tracking. Keep ML-specific code (model_card, explainability, drift monitoring) in ml/tracking/. No hardcoded http://localhost:9000 may remain.
2. **Evaluation -> top-level evaluation/ package** (not named eval). Move ml/eval/* (including benchmark_router.py), ml/models/evaluate.py, and any ai evaluation/benchmark/golden-set runner code into evaluation/ml/ and evaluation/ai/. Give each runnable script a `python -m evaluation....` entry point and update Makefile targets and docs. Training code stays in ml/. (Fix 10.6 is resolved by this move; the Fix 6.5 logic change to evaluate.py happens later in WP11, not here.)
3. **Shared embeddings -> core/embeddings/.** ai/services/embedding_service.py is imported by ml/pipelines/seed_curated.py (a layering violation, Fix 7.5). Move it to core/embeddings/ and update both ai and ml callers.
4. **ml must not import apps (Fix 8.4).** ml/models/onnx_exporter.py imports from apps.api.serving. Find why. Move whatever is shared to a layer both may import (ml/ or core/) so apps imports from ml, never the reverse.
5. **Nested package apps/api/core/ -> apps/api/rate_limiting/.** Move rate_limiter.py there, update all imports. Do NOT change its logic or the RateLimiterService (WP5 does that).
6. Search the repo for any other violation of the dependency rule in fix_common.md and fix it by moving code to the correct layer.

## Also
- Create tests/test_architecture.py: an AST-based scan of all .py files (excluding legacy_project/, .web/, tests/) that fails on any forbidden import per the dependency rule, and fails on `import mlflow` outside core/tracking. Print the violating file:line on failure. It must pass at the end of this WP. If a violation truly cannot be removed now, allow-list it with a comment and list it in the report.
- Update Dockerfile.api, Dockerfile.ui, docker-compose.yml volumes, Makefile, CI workflow, README.md, system.md and docs/ for new paths (e.g. COPY evaluation/ if needed).

## Verify
- `python -c "import core, ml, ai, evaluation, apps.api.main"` style import smoke for every top-level package.
- Full `pytest tests/ -q`: results must equal baseline_tests.txt (no new failures).
- `docker compose config` succeeds.
- `git grep -n "apps.api.core"` and `git grep -n "ml.eval"` return nothing.
Report: FIX_REPORT_wp0.md plus RESTRUCTURE_MAP.md (complete move list).
