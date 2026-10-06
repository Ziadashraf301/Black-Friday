# Comprehensive Remediation Plan: Black Friday v2

> **Document**: `FIX_PLAN.md`  
> **Source Inputs**: `REVIEW_HANDOFF.md`, `REVIEW_core.md`, `REVIEW_ml.md`, `REVIEW_backend.md`, `REVIEW_ai.md`, `REVIEW_frontend.md`, `REVIEW_devops.md`  
> **Status**: Approved Remediation Plan  
> **Scope**: Merged, deduplicated, and dependency-ordered fixes across all project domains.  
> **Rule Enforcement**: All rejected findings dropped; cross-report duplicates merged into single fixes; each batch contains strictly independent files.

---

## 1. Summary of Dropped / Rejected Findings

The following preliminary findings from `REVIEW_HANDOFF.md` were audited and **REJECTED** by domain reviewers with concrete codebase evidence, and are excluded from the fix batches:

| Rejected Finding | Claimed Target | Rejecting Review(s) | Ground Truth in Codebase |
| :--- | :--- | :--- | :--- |
| **SQL Schema Duplication with Pandera** | `core/db/models/warehouse.py` vs `ml/features/data_contract.py` | `REVIEW_core.md` (H-4), `REVIEW_ml.md` (H-02) | `warehouse.py` defines plain column types without check constraints; validation exists solely in `data_contract.py`. |
| **Async Session Claim** | `core/db/session.py` | `REVIEW_core.md` (H-5) | `AsyncSessionLocal` does not exist in `session.py`; sync session is used throughout. |
| **Missing Core Dependencies** | `requirements.txt` | `REVIEW_ai.md` (H5), `REVIEW_devops.md` (§1) | `langgraph>=1.2.11`, `langchain-core>=1.6.2`, and `loguru>=0.7.3` are already present in `requirements.txt:72-74`. |
| **Dockerfile.api Missing `ai/` & Copying `./config`** | `docker/Dockerfile.api` | `REVIEW_ai.md` (H6), `REVIEW_devops.md` (§1) | `Dockerfile.api:23` already copies `ai/`; no `./config` copy directive exists. |
| **docker-compose Mounting `./src`** | `docker-compose.yml` | `REVIEW_devops.md` (§1) | `docker-compose.yml:151-158` sets `PYTHONPATH: /app` and mounts existing project directories. |
| **Hardcoded `API_BASE_URL` in State** | `apps/reflex_app/reflex_app/state.py` | `REVIEW_frontend.md` (Claim 1), `REVIEW_devops.md` (§1) | `state.py:15` already uses `os.getenv("API_BASE_URL", "http://127.0.0.1:8000")`. |
| **Duplicated Security Lockout in Route** | `apps/api/routes/bot.py` vs `security_ban_middleware.py` | `REVIEW_backend.md` (H2), `REVIEW_ai.md` (H2, F-06) | HTTP SSE route does not repeat ban checks; WebSocket route checks ban because `BaseHTTPMiddleware` ignores WebSockets. |
| **Silent Exception Swallowing in Search Relaxation** | `ai/services/search_relaxation_service.py` | `REVIEW_ai.md` (H7) | File does not exist; actual `search_service.py` delegates to repository and does not swallow exceptions. |
| **Fictional Bot Drawer Components** | `apps/reflex_app/reflex_app/components/bot_drawer.py` | `REVIEW_frontend.md` (Claim 2) | Audio player and policy accordions were imagined handoff claims, not in code. |
| **Non-Existent ML Files & CatBoost** | `ml/models/champion_challenger.py`, `profiling.py`, etc. | `REVIEW_ml.md` (H-04) | Documented files do not exist; logic resides in `retrain.py`, `clustering.py`, and `apriori_engine.py`. |
| **Serving File Discrepancy** | `apps/api/serving/onnx_session.py` | `REVIEW_backend.md` (H6) | Non-existent name in handoff doc; code already correctly uses `apps/api/serving/predictor.py`. |

---

## 2. Remediation Batches

Fixes are organized into **10 strictly ordered batches**. Batch 1 establishes foundational infrastructure and settings upon which subsequent layers rely. Within every batch, **each fix touches a completely distinct file**, ensuring zero file conflicts and allowing safe concurrent edits within a batch.

---

### Batch 1: Core Foundation, CI & Environment Configuration

Foundational environment settings, container orchestration mappings, testing service containers, and schema definitions that other modules depend on.

#### Fix 1.1: Provision Database & Cache Service Containers in CI Pipeline
- **Files Touched**: `.github/workflows/ci-cd.yml`
- **One-line Description**: Add `pgvector/pgvector:pg16` and `redis:7-alpine` service containers with health checks to GitHub Actions workflow so integration tests execute against real services.
- **Severity**: High
- **Verification Needed**: Confirm service container ports (5432, 6379), healthcheck commands (`pg_isready`, `redis-cli ping`), and matching environment variables (`POSTGRES_HOST: localhost`, `REDIS_HOST: localhost`).
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_devops.md` (§2).

#### Fix 1.2: Make `GEMINI_API_KEY` Optional and Centralize Configuration Properties
- **Files Touched**: `core/config.py`
- **One-line Description**: Set `GEMINI_API_KEY: Optional[str] = None` to unblock offline execution, URL-encode connection passwords, consolidate integer field validators, and add `MINIO_HOST` / `S3_ENDPOINT_URL` settings.
- **Severity**: High
- **Verification Needed**: Verify `Settings()` instantiates without error when `GEMINI_API_KEY` is omitted from the environment, and verify database URLs escape special characters using `urllib.parse.quote_plus`.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_core.md` (N-2, N-14, N-16), `REVIEW_ml.md` (N-09).

#### Fix 1.3: Defer Log Directory Creation to Prevent Import-Time Side Effects
- **Files Touched**: `core/logging.py`
- **One-line Description**: Wrap `os.makedirs(settings.LOG_DIR)` inside initialization functions with error handling to prevent import crashes in read-only environments.
- **Severity**: Low
- **Verification Needed**: Verify importing `core.logging` succeeds in environments with read-only root filesystems.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_core.md` (N-17).

#### Fix 1.4: Consolidate Application Tables into Database Initialization DDL
- **Files Touched**: `docker/postgres/init_schema.sql`
- **One-line Description**: Add DDL for `app_users`, `user_purchases`, `user_carts`, and `semantic_query_cache` (with HNSW index), and remove hardcoded `\connect fridayblack;`.
- **Severity**: Med
- **Verification Needed**: Test executing `init_schema.sql` on a fresh database container and verify all tables, foreign keys, and vector indices are created cleanly.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_devops.md` (§2), `REVIEW_core.md` (N-15, §5).

#### Fix 1.5: Add Missing Load Testing Dependency
- **Files Touched**: `requirements.txt`
- **One-line Description**: Add `locust>=2.24.0` to requirements to allow executing load tests in `tests/locustfile.py`.
- **Severity**: Low
- **Verification Needed**: Verify `python -c "import locust"` imports without `ModuleNotFoundError` after installation.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_devops.md` (§2).

#### Fix 1.6: Mount Data Directory Volume in Docker Compose
- **Files Touched**: `docker-compose.yml`
- **One-line Description**: Add `- ./data:/app/data` volume mount under `model-api` in `docker-compose.yml` so store policies and curated product assets are accessible in containerized deployments.
- **Severity**: High
- **Verification Needed**: Verify container path `/app/data/store_policies.json` exists inside the running container.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_devops.md` (§2), `REVIEW_frontend.md` (§5).

#### Fix 1.7: Support Dynamic API URL in Reflex Configuration
- **Files Touched**: `apps/reflex_app/rxconfig.py`
- **One-line Description**: Configure `api_url` dynamically using `os.getenv("REFLEX_API_URL", "http://localhost:8001")` instead of hardcoding localhost.
- **Severity**: Med
- **Verification Needed**: Verify Reflex boots and binds to custom backend URLs when `REFLEX_API_URL` is set in production.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_devops.md` (§4), `REVIEW_frontend.md` (Claim 1).

---

### Batch 2: Database Foundation, Caching & Core Pipeline Logic

Core database base repository behavior, connection pooling, security hashing, model export manifests, and ML promotion gating.

#### Fix 2.1: Fix Destructive Fallback and Type Coercion in Base Repository
- **Files Touched**: `core/db/repositories/base.py`
- **One-line Description**: Replace table-dropping `if_exists="replace"` in `to_sql` fallback with `TRUNCATE` + `if_exists="append"` to preserve vector/text indexes, remove heuristic float-to-Int64 type mutation, and add `user_carts` / `semantic_query_cache` to `VALID_TABLES`.
- **Severity**: High
- **Verification Needed**: Verify custom vector indexes (`hnsw`) and GIN indexes survive failed COPY operations falling back to `to_sql`.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_core.md` (N-4, N-11, N-15).

#### Fix 2.2: Expose Public Redis Client, Reconnection Cooldown, and Non-Blocking Cache Deletion
- **Files Touched**: `core/cache/redis_client.py`
- **One-line Description**: Expose a thread-safe public `client` property, implement a 15-second reconnection cooldown on failure to prevent multi-second request hangs, and replace blocking `keys()` with `scan_iter()` in `delete_pattern()`.
- **Severity**: High
- **Verification Needed**: Verify `is_available` returns `False` immediately without hanging when Redis is offline, and verify pattern deletion uses batched scan iterations.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_core.md` (H-2, N-9, N-10), `REVIEW_backend.md` (H3), `REVIEW_ai.md` (F-01, F-02, §6).

#### Fix 2.3: Standardize Password Hashing with Pre-Hashing
- **Files Touched**: `core/security.py`
- **One-line Description**: Pre-hash plain-text passwords with SHA-256 before bcrypt hashing to safely handle arbitrary password lengths without UTF-8 byte boundary corruption or silent 72-byte truncation.
- **Severity**: Low
- **Verification Needed**: Verify password verification succeeds with SHA-256 pre-hashed passwords.
- **Human Decision / Retraining**: **Human Decision**: Verify whether existing dev user credentials in test databases require re-seeding after updating the hashing scheme.
- **Mentioned in**: `REVIEW_core.md` (N-12).

#### Fix 2.4: Invert Decision Logic in Champion-Challenger Promotion Gate
- **Files Touched**: `ml/pipelines/retrain.py`
- **One-line Description**: Invert subtraction to addition in `candidate_r2 >= (champion_r2 + min_improvement_delta)` so inferior candidate models cannot be promoted to Production champion.
- **Severity**: High
- **Verification Needed**: **Check metric direction!** R² is higher-is-better, and `min_improvement_delta` is a positive float (`0.0050`). Verify that a challenger with R² lower than `champion_r2 + 0.0050` evaluates to `False`.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_ml.md` (N-01).

#### Fix 2.5: Export ModelRegistry in Public Package Manifest
- **Files Touched**: `ml/models/__init__.py`
- **One-line Description**: Add `ModelRegistry` to `__all__` in `ml/models/__init__.py` so pipelines can import it directly from the package root.
- **Severity**: Low
- **Verification Needed**: Verify `from ml.models import ModelRegistry` imports successfully.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_ml.md` (N-13).

#### Fix 2.6: Copy Data Assets and Add Healthcheck in Backend Dockerfile
- **Files Touched**: `docker/Dockerfile.api`
- **One-line Description**: Add `COPY data/ /app/data/` and a `HEALTHCHECK` directive probing `/health` in `docker/Dockerfile.api`.
- **Severity**: High
- **Verification Needed**: Verify the container builds, passes health check, and `/app/data/` files are present.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_devops.md` (§2).

---

### Batch 3: Data Contracts, Table Creation, Feature Engineering & Imputation

Fixing MRO table creation, indexing warehouse tables, eliminating target leakage in imputer features, data contract schema checks, and StrikeTracker Redis operations.

#### Fix 3.1: Resolve MRO Collision in User Repository
- **Files Touched**: `core/db/repositories/user_repo.py`
- **One-line Description**: Rename colliding `UserRepository.create_app_tables()` to `ensure_user_tables()` so `BaseRepository.create_app_tables()` executes `Base.metadata.create_all()` on startup to create all 8 tables.
- **Severity**: High
- **Verification Needed**: Verify that invoking `create_app_tables()` on startup creates warehouse tables, segments, and network metrics rather than only `app_users` and `user_purchases`.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_core.md` (N-1, H-1), `REVIEW_devops.md` (§4).

#### Fix 3.2: Add Index on Product ID in Cleaned Warehouse Model
- **Files Touched**: `core/db/models/warehouse.py`
- **One-line Description**: Add `index=True` to `product_id` in `black_friday_cleaned` ORM model to eliminate 550,000-row full table scans during category mode queries.
- **Severity**: Med
- **Verification Needed**: Verify generated DDL includes `CREATE INDEX` on `product_id` for `black_friday_cleaned`.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_core.md` (N-8).

#### Fix 3.3: Eliminate Target Leakage in MissForest Imputer Feature Definitions
- **Files Touched**: `ml/features/imputation.py`
- **One-line Description**: Remove `"purchase"` from `MissForestImputer.FEATURE_COLS`, safely handle missing inference columns in `transform()`, and clip imputed category values to `[1, 20]`.
- **Severity**: High
- **Verification Needed**: Verify `imputer.transform(df)` runs without `KeyError` when `"purchase"` is omitted from the input DataFrame.
- **Human Decision / Retraining**: **Retraining Required**: MissForest imputer weights and downstream regression models must be retrained after removing `purchase`.
- **Mentioned in**: `REVIEW_ml.md` (H-01, N-04, N-11), `REVIEW_backend.md` (H5).

#### Fix 3.4: Add Missing Column Validation in Cleaned Data Contract
- **Files Touched**: `ml/features/data_contract.py`
- **One-line Description**: Add `stay_in_current_city_years` validation column (`isin(["0", "1", "2", "3", "4+"])`) to `CleanedTransactionSchema`.
- **Severity**: Med
- **Verification Needed**: Verify `CleanedTransactionSchema.validate(df)` rejects records with invalid stay years while accepting valid strings (`"0"`, `"4+"`).
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_ml.md` (N-05).

#### Fix 3.5: Support Model Aliases in Training CLI
- **Files Touched**: `ml/pipelines/train.py`
- **One-line Description**: Resolve `--model` CLI arguments through `ModelRegistry.resolve_name()` so `--model lgbm` maps to `lightgbm` instead of raising a `ValueError`.
- **Severity**: High
- **Verification Needed**: Run `python -m ml.pipelines.train --model lgbm --help` or verify alias resolution converts `"lgbm"` to `"lightgbm"`.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_ml.md` (N-02).

#### Fix 3.6: Fix Column Dropping Bug in Serving ONNX Imputer
- **Files Touched**: `apps/api/serving/imputer.py`
- **One-line Description**: Retain imputed columns when they are absent from the input DataFrame, drop dummy `"purchase"` handling, and convert categories to integer safely.
- **Severity**: High
- **Verification Needed**: Pass a test DataFrame without `product_category_2` to `ONNXMissForestImputer.transform()` and verify the returned DataFrame contains imputed `product_category_2`.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_backend.md` (N1, H5).

#### Fix 3.7: Fix Broken Redis Calls in Security Strike Tracker
- **Files Touched**: `ai/guardrails/strike_tracker.py`
- **One-line Description**: Replace non-existent `_cache.get()` and `_cache.client` with `_cache._client.get()` and raw client commands under `is_available` guards to restore distributed strikes and 24-hour lockouts.
- **Severity**: High
- **Verification Needed**: Verify strikes increment in Redis under `strike:<id>` and bans write to `ban:<id>` with 24-hour TTL instead of falling back to in-memory dictionaries.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_ai.md` (F-01).

---

### Batch 4: Repository Sanitization, Serving Engine & Agent Tool Durability

Parameterizing search queries, deduplicating tree model preprocessors, optimizing ONNX serving, fixing cart Redis caching, and dynamic hero card binding.

#### Fix 4.1: Parameterize RRF Hybrid Search and Purge Inline DDL in Warehouse Repository
- **Files Touched**: `core/db/repositories/warehouse_repo.py`
- **One-line Description**: Bind `:top_k` as a query parameter in `_execute_rrf_query()` and `hybrid_search_products()` to eliminate SQL injection, and remove inline `ensure_user_carts_table()` and `create_all()` calls from read/write methods.
- **Severity**: High
- **Verification Needed**: Verify SQL query binds `:top_k` as parameter, and verify `get_curated_products()` and cart methods execute without issuing DDL.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_core.md` (N-3, N-5, N-6).

#### Fix 4.2: Deduplicate Preprocessors and Reclassify Occupation in Regression Models
- **Files Touched**: `ml/models/regression.py`
- **One-line Description**: Extract duplicate `ColumnTransformer` definitions into a reusable factory function, and reclassify nominal `occupation` from `NUMERIC_FEATS` to `CATEGORICAL_FEATS`.
- **Severity**: Med
- **Verification Needed**: Verify preprocessor encodes `occupation` using `OrdinalEncoder` and that transformed feature shapes match model requirements.
- **Human Decision / Retraining**: **Retraining Required / Human Decision**: Changing `occupation` encoding modifies feature representation and requires retraining regression models.
- **Mentioned in**: `REVIEW_ml.md` (N-06, N-07).

#### Fix 4.3: Enable ONNX Runtime Graph Optimizations in Inference Predictor
- **Files Touched**: `apps/api/serving/predictor.py`
- **One-line Description**: Set `opts.graph_optimization_level = ort.GraphOptimizationLevel.ORT_ENABLE_ALL` in `ONNXPredictor` to enable C++ kernel fusion and constant folding.
- **Severity**: Med
- **Verification Needed**: Verify model outputs match baseline and that prediction latency decreases.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_backend.md` (N6).

#### Fix 4.4: Implement Optional User Authentication Dependency
- **Files Touched**: `apps/api/auth.py`
- **One-line Description**: Implement `get_optional_user` with `HTTPBearer(auto_error=False)` to allow endpoints to accept both authenticated tokens and unauthenticated guest callers.
- **Severity**: High
- **Verification Needed**: Verify `get_optional_user` returns `None` without raising HTTP 403 when no authorization header is supplied.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_backend.md` (N4, H8).

#### Fix 4.5: Fix Broken Redis Cache Calls in Cart Management Tool
- **Files Touched**: `ai/tools/cart_tools.py`
- **One-line Description**: Replace non-existent `_cache.get()` and `_cache.set()` with `_cache.get_json()` and `_cache.set_json()` to restore Redis shopping cart persistence.
- **Severity**: High
- **Verification Needed**: Verify carts persist in Redis across process restarts and are retrieved via `get_json()`.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_ai.md` (F-02, H8).

#### Fix 4.6: Dynamically Bind Hero Card to Reactive State
- **Files Touched**: `apps/reflex_app/reflex_app/components/hero_card.py`
- **One-line Description**: Bind image, title, tagline, price, and click handlers in `hero_card()` to `ShoppingState.hero_product` instead of hardcoding product `P00025442`.
- **Severity**: High
- **Verification Needed**: Verify changing `ShoppingState.hero_product` dynamically updates the card display and adds the correct item to the cart.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_frontend.md` (F-02).

#### Fix 4.7: Decouple Frontend Container Dependencies and Copy Data
- **Files Touched**: `docker/Dockerfile.ui`
- **One-line Description**: Install lightweight `requirements-ui.txt` instead of full ML requirements to shrink image size by >1.5 GB, and copy `data/` for offline catalog fallbacks.
- **Severity**: Med
- **Verification Needed**: Verify UI Docker container builds quickly and offline catalog loads from `/app/data/curated_products.json`.
- **Human Decision / Retraining**: **Human Decision**: Confirm separate `requirements-ui.txt` file creation.
- **Mentioned in**: `REVIEW_devops.md` (§2), `REVIEW_frontend.md` (F-07).

---

### Batch 5: API Middleware, Rate Limiting, Monitoring & UI Components

FastAPI main configuration, CORS, rate limiter encapsulation and atomicity, ban middleware JWT inspection, drift monitoring target check, and sidebar filters.

#### Fix 5.1: Fix CORS Wildcard Conflict and Add Global Exception Handlers
- **Files Touched**: `apps/api/main.py`
- **One-line Description**: Replace wildcard origin `*` with explicit frontend origins when `allow_credentials=True`, register structured exception handlers for `ValueError` and unhandled exceptions, and move late imports to top of file.
- **Severity**: Med
- **Verification Needed**: Verify browser CORS preflight with credentials succeeds, and verify `ValueError` returns structured HTTP 400 JSON.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_backend.md` (N5, N13, N14, H7).

#### Fix 5.2: Decode JWT Bearer Tokens in Security Ban Middleware
- **Files Touched**: `apps/api/middleware/security_ban_middleware.py`
- **One-line Description**: Decode `Authorization: Bearer <token>` to extract user ID (`sub`) when `X-User-ID` is omitted, preventing banned users from bypassing lockout.
- **Severity**: Med
- **Verification Needed**: Send request with banned user's JWT token without `X-User-ID` header and verify middleware responds with HTTP 403 Forbidden.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_backend.md` (N7).

#### Fix 5.3: Relocate Rate Limiter, Consume Public Client, and Make Sliding Window Atomic
- **Files Touched**: `apps/api/core/rate_limiter.py`
- **One-line Description**: Relocate rate limiter to avoid namespace shadowing, access public `cache_manager.client`, and execute sliding-window check and increment atomically in Redis.
- **Severity**: Med
- **Verification Needed**: Run simulated burst requests and verify request counts strictly adhere to configured rate limits without race conditions.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_backend.md` (H3, H4, N12), `REVIEW_core.md` (H-2, H-6).

#### Fix 5.4: Remove Redundant RateLimiterService Indirection Layer
- **Files Touched**: `apps/api/services/rate_limiter_service.py`
- **One-line Description**: Delete or empty redundant 17-line `RateLimiterService` pass-through class in favor of direct `rate_limiter` dependency consumption.
- **Severity**: Low
- **Verification Needed**: Verify no remaining application code imports `RateLimiterService`.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_backend.md` (H1).

#### Fix 5.5: Fix Ground-Truth Target Verification in Drift Monitoring Pipeline
- **Files Touched**: `ml/pipelines/monitor.py`
- **One-line Description**: Check target column presence against normalized evaluation dataframe `curr_eval` or raw `current_batch_df["purchase"]` so incoming batches with labels trigger supervised retraining.
- **Severity**: High
- **Verification Needed**: Verify `has_target` evaluates to `True` when incoming batch contains `"purchase"`.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_ml.md` (N-03).

#### Fix 5.6: Harmonize UI Card Structure in Retrieval Aggregator Join
- **Files Touched**: `ai/nodes/aggregator_node.py`
- **One-line Description**: Align aggregated UI cards format with synthesis node expectations so specialist recommendations are preserved through LangGraph fanout join.
- **Severity**: High
- **Verification Needed**: Verify `aggregator_node` returns `ui_payload` formatted with both `cards` and `action_chips`.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_ai.md` (F-03).

#### Fix 5.7: Make Sidebar Filters Dynamically Data-Driven from Catalog State
- **Files Touched**: `apps/reflex_app/reflex_app/components/sidebar.py`
- **One-line Description**: Compute category, brand, style, and season filter chips dynamically from `ShoppingState.products` instead of hardcoding 40 static items.
- **Severity**: Med
- **Verification Needed**: Verify all 17 catalog categories are selectable in the sidebar filter.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_frontend.md` (F-09).

---

### Batch 6: Route Security, AI Nodes, Tool Bridges & Service Sanitization

Attaching route dependencies, eliminating DDL from auth services, unifying AI node logging, preserving synthesis UI cards, and eliminating evaluation target leakage.

#### Fix 6.1: Attach Rate Limiting and Optional User Auth to Shopper Routes
- **Files Touched**: `apps/api/routes/shopper.py`
- **One-line Description**: Attach `rate_limit_dependency` to `/shopper/predict-price` and wire `get_optional_user` to `/shopper/predict-price-batch`.
- **Severity**: High
- **Verification Needed**: Verify unauthenticated POST to `/shopper/predict-price-batch` succeeds with HTTP 200, and rapid repeated requests trigger HTTP 429.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_backend.md` (N3, N4).

#### Fix 6.2: Purge DDL Calls from Authentication Service
- **Files Touched**: `apps/api/services/auth_service.py`
- **One-line Description**: Remove `repo.create_app_tables()` from `register_user` and `authenticate_user` methods to eliminate database catalog lock contention on auth paths.
- **Severity**: High
- **Verification Needed**: Benchmark registration/login latency under load and verify zero DDL `CREATE TABLE` queries are executed.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_backend.md` (N2).

#### Fix 6.3: Unify Cart Node Logging to Core Logging
- **Files Touched**: `ai/nodes/cart_node.py`
- **One-line Description**: Replace `from loguru import logger` with `from core.logging import get_logger; logger = get_logger(__name__)`.
- **Severity**: Med
- **Verification Needed**: Verify log messages format consistently with centralized application logging.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_ai.md` (F-04, H1), `REVIEW_core.md` (H-3).

#### Fix 6.4: Preserve Aggregator UI Cards in Synthesis Service
- **Files Touched**: `ai/services/synthesis_service.py`
- **One-line Description**: Preserve and merge `aggregator_ui` cards into `final_ui_payload` rather than overwriting `ui_payload`, and extract deterministic templating logic.
- **Severity**: High
- **Verification Needed**: Verify synthesized responses retain aggregated specialist product cards in the returned payload.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_ai.md` (F-03, F-07).

#### Fix 6.5: Eliminate Target Leakage in Holdout Imputation Evaluation
- **Files Touched**: `ml/models/evaluate.py`
- **One-line Description**: Drop `"purchase"` column from `masked_test` during holdout imputation evaluation so metrics reflect true inference conditions.
- **Severity**: Med
- **Verification Needed**: Verify holdout evaluation runs successfully on test data where `purchase` is absent.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_ml.md` (N-10).

#### Fix 6.6: Read S3 Storage Endpoint from Settings in MLflow Tracker
- **Files Touched**: `ml/tracking/mlflow_tracker.py`
- **One-line Description**: Set `MLFLOW_S3_ENDPOINT_URL` from `settings.S3_ENDPOINT_URL` or environment variable instead of hardcoding `http://localhost:9000`.
- **Severity**: Med
- **Verification Needed**: Verify artifact uploads connect to container S3 endpoint in Docker environments.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_ml.md` (N-09).

#### Fix 6.7: Upgrade Assistant Drawer with Markdown, Interactivity & Deactivated Mock Voice
- **Files Touched**: `apps/reflex_app/reflex_app/components/bot_drawer.py`
- **One-line Description**: Render assistant messages using `rx.markdown()`, add click handlers to recommended product cards, and replace deceptive voice mode toggle with a disabled "Beta Soon" badge.
- **Severity**: Med
- **Verification Needed**: Verify Markdown lists and bold text render formatted, and clicking recommendation cards opens product details.
- **Human Decision / Retraining**: **Human Decision**: Confirm disabling voice toggle until real audio recording is implemented.
- **Mentioned in**: `REVIEW_frontend.md` (F-05, F-17, F-18).

---

### Batch 7: Bot Streaming, AI Observability, Frontend State & Performance

Bot SSE stream formatting & rate limiting, shopper service N+1 elimination, AI bundle logging, hybrid extractor sizing deduplication, seed curated decoupling, and frontend reactive state streaming.

#### Fix 7.1: Attach Rate Limiting and Preserve Markdown Boundaries in Bot SSE Route
- **Files Touched**: `apps/api/routes/bot.py`
- **One-line Description**: Attach `rate_limit_dependency` to `/bot/stream` and stream tokens using regex boundaries (`\S+|\n+`) to preserve Markdown indentation and newlines without artificial delays.
- **Severity**: High
- **Verification Needed**: Verify SSE tokens preserve double newlines and list formatting, and test that rapid repeated requests trigger HTTP 429.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_ai.md` (F-10), `REVIEW_backend.md` (N3).

#### Fix 7.2: Eliminate DDL, N+1 Queries, and Duplicated Pricing in Shopper Service
- **Files Touched**: `apps/api/services/shopper_service.py`
- **One-line Description**: Remove `create_app_tables()` from `process_purchase`, replace N+1 category queries with bulk category retrieval, and extract duplicated member discount calculation helper.
- **Severity**: High
- **Verification Needed**: Verify batch pricing performs a single bulk category query and checkout executes without DDL locks.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_backend.md` (N2, N8, N9).

#### Fix 7.3: Unify Bundle Node Logging to Core Logging
- **Files Touched**: `ai/nodes/bundle_node.py`
- **One-line Description**: Replace `from loguru import logger` with `from core.logging import get_logger; logger = get_logger(__name__)`.
- **Severity**: Med
- **Verification Needed**: Verify bundle node logs use centralized logger format.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_ai.md` (F-04, H1), `REVIEW_core.md` (H-3).

#### Fix 7.4: Remove Redundant Sizing Logic in Hybrid Entity Extractor
- **Files Touched**: `ai/extractor/hybrid_extractor.py`
- **One-line Description**: Remove redundant lines re-parsing shoe and waist sizes that were already extracted by `RegexEntityExtractor`.
- **Severity**: Low
- **Verification Needed**: Verify extracted entities for sizing queries match expected size outputs.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_ai.md` (F-08).

#### Fix 7.5: Decouple Bot Embedding Import from Seed Curated Pipeline
- **Files Touched**: `ml/pipelines/seed_curated.py`
- **One-line Description**: Remove direct import of `ai.services.embedding_service` to decouple offline ML data seeding from conversational assistant application layer.
- **Severity**: Med
- **Verification Needed**: Verify `seed_curated` executes without requiring `ai` package dependencies.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_ml.md` (N-08).

#### Fix 7.6: Asynchronous SSE Streaming, Member Pricing, Checkout & LocalStorage in Frontend State
- **Files Touched**: `apps/reflex_app/reflex_app/state.py`
- **One-line Description**: Convert `_execute_bot_query` to an `async` generator streaming tokens without freezing the event loop, check `cached_price_estimates` in direct cart additions, respect item quantity and error states in checkout, parse grounding citations, and wrap `auth_token` in `rx.LocalStorage`.
- **Severity**: High
- **Verification Needed**: Verify chat tokens stream progressively, authenticated users receive member pricing when adding from catalog grid, cart items with quantity > 1 checkout properly, and page refresh preserves authentication.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_frontend.md` (F-01, F-03, F-04, F-06, F-08, F-10, F-12, F-16).

#### Fix 7.7: Exercise Real Method in Frontend API Client Tests
- **Files Touched**: `tests/test_api_client.py`
- **One-line Description**: Invoke `state.load_dashboard()` in `test_analytics_fetch_in_reflex` to verify mocked response handling, and test `API_BASE_URL` with monkeypatching rather than asserting static localhost.
- **Severity**: Med
- **Verification Needed**: Run `pytest tests/test_api_client.py` and verify both tests pass with dynamic environment overrides.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_devops.md` (§2), `REVIEW_frontend.md` (F-19).

---

### Batch 8: Clean Architecture, Quality Gates & Architectural Decisions

Repository composition, support node logging, bundle tools cache reuse, ONNX exporter decoupling, test suite reliability, and Makefile quality gates.

#### Fix 8.1: Transition BlackFridayRepository from Multiple Inheritance to Composition
- **Files Touched**: `core/db/repository.py`
- **One-line Description**: Refactor `BlackFridayRepository` to aggregate domain repositories (`warehouse`, `analytics`, `segmentation`, `recommendation`, `users`) via composition with backward-compatible method delegation.
- **Severity**: High
- **Verification Needed**: Verify callers injecting `BlackFridayRepository` continue to access domain repository methods without breakage.
- **Human Decision / Retraining**: **Human Decision**: Decide whether to migrate route callers to inject individual domain repositories directly.
- **Mentioned in**: `REVIEW_core.md` (H-1), `REVIEW_backend.md` (§5).

#### Fix 8.2: Unify Support Node Logging to Core Logging
- **Files Touched**: `ai/nodes/support_node.py`
- **One-line Description**: Replace `from loguru import logger` with `from core.logging import get_logger; logger = get_logger(__name__)`.
- **Severity**: Med
- **Verification Needed**: Verify support node logs use centralized logger format.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_ai.md` (F-04, H1), `REVIEW_core.md` (H-3).

#### Fix 8.3: Reuse Singleton Cache Manager in Bundle Tools
- **Files Touched**: `ai/tools/bundle_tools.py`
- **One-line Description**: Import and reuse global `cache_manager` singleton rather than instantiating an unpooled `RedisCacheManager()`.
- **Severity**: Low
- **Verification Needed**: Verify bundle recommendations tool uses the shared connection pool.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_ai.md` (F-09).

#### Fix 8.4: Decouple Serving Imports from ONNX Exporter
- **Files Touched**: `ml/models/onnx_exporter.py`
- **One-line Description**: Remove imports from `apps.api.serving` in `onnx_exporter.py` to prevent circular dependencies between training and presentation layers.
- **Severity**: Med
- **Verification Needed**: Verify ONNX export executes in an environment without `apps/api` loaded.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_ml.md` (N-08).

#### Fix 8.5: Relax Latency Assertion in JEV Router Unit Tests
- **Files Touched**: `tests/test_phase2_jev_router.py`
- **One-line Description**: Relax sub-millisecond p95 latency assertion threshold or mark with `@pytest.mark.benchmark` so noisy CI virtual machine runners do not fail intermittently.
- **Severity**: Med
- **Verification Needed**: Run `pytest tests/test_phase2_jev_router.py` and verify deterministic test pass.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_devops.md` (§2).

#### Fix 8.6: Assert Specific SchemaError in Feature Contract Tests
- **Files Touched**: `tests/test_features.py`
- **One-line Description**: Replace `pytest.raises(Exception)` with `pytest.raises(SchemaError)` in contract tests to verify explicit Pandera schema rejection.
- **Severity**: Low
- **Verification Needed**: Run `pytest tests/test_features.py` and ensure expected validation failures are caught.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_devops.md` (§2).

#### Fix 8.7: Include `ai/` in Quality Gates and Implement Cross-Platform Clean
- **Files Touched**: `Makefile`
- **One-line Description**: Add `ai/` to `flake8` and `black` commands in `Makefile:77-78`, and implement cross-platform cache cleanup using Python one-liners.
- **Severity**: Low
- **Verification Needed**: Run `make lint` and verify `ai/` is inspected; run `make clean` on Windows and verify clean execution.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_devops.md` (§2), `REVIEW_ai.md` (F-05).

---

### Batch 9: Long-Term Tech Debt, Secondary Refactors & Human Decisions

Optimizing recommendation queries, Redis analytics caching, currency constants, modernizing GenAI SDK, hot-reloading policies, and auth modal UI deduplication.

#### Fix 9.1: Optimize Category Mode Query in Recommendation Repository
- **Files Touched**: `core/db/repositories/recommendation_repo.py`
- **One-line Description**: Optimize `get_product_categories` query to utilize index on `product_id` in `black_friday_cleaned` and leverage repository session.
- **Severity**: Med
- **Verification Needed**: Verify query execution uses index scan rather than sequential scan.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_core.md` (N-8).

#### Fix 9.2: Implement Distributed Redis Caching in Analytics Repository
- **Files Touched**: `core/db/repositories/analytics_repo.py`
- **One-line Description**: Replace discarded per-request `self._cache_eda_summary` with `cache_manager.get_json / set_json` Redis caching with 1-hour TTL.
- **Severity**: Low
- **Verification Needed**: Verify EDA summary query results persist across distinct repository instances.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_core.md` (N-13).

#### Fix 9.3: Clean Up Dead Code or Implement Unit of Work in DB Session
- **Files Touched**: `core/db/session.py`
- **One-line Description**: Clean up unused `get_db()` dependency or refactor repositories to support shared session injection for atomic multi-repository operations.
- **Severity**: Low
- **Verification Needed**: Verify no remaining application code imports `get_db()`.
- **Human Decision / Retraining**: **Human Decision**: Decide whether to adopt full Unit of Work pattern across all repositories.
- **Mentioned in**: `REVIEW_core.md` (N-7).

#### Fix 9.4: Centralize Currency Conversion Constant in Model Service
- **Files Touched**: `apps/api/services/model_service.py`
- **One-line Description**: Replace hardcoded `INR_TO_USD = 80.0` local variables with `settings.INR_TO_USD` configuration constant.
- **Severity**: Low
- **Verification Needed**: Verify price conversion uses centralized configuration value.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_backend.md` (N10).

#### Fix 9.5: Standardize Unified GenAI SDK in Embedding Service
- **Files Touched**: `ai/services/embedding_service.py`
- **One-line Description**: Migrate from legacy `google.generativeai` with suppressed warnings to the modern unified `google.genai.Client` SDK.
- **Severity**: Low
- **Verification Needed**: Verify vector embeddings generate successfully without deprecated API warnings.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_ai.md` (F-12).

#### Fix 9.6: Implement Dynamic File Mtime Check for Store Policies
- **Files Touched**: `ai/tools/policy_kb.py`
- **One-line Description**: Check file modification timestamp (`mtime`) on lookup in `PolicyKnowledgeBase` to allow hot reloading of policies without server restart.
- **Severity**: Low
- **Verification Needed**: Update `data/store_policies.json` and verify `get_policy()` returns updated content without restart.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_ai.md` (F-11).

#### Fix 9.7: Deduplicate Auth Error Banners and Add Registration Inputs
- **Files Touched**: `apps/reflex_app/reflex_app/components/auth_modal.py`
- **One-line Description**: Extract reusable `auth_error_banner()` component and add occupation select dropdown using `OCCUPATION_LABELS` to the signup form.
- **Severity**: Low
- **Verification Needed**: Verify login and signup panels display error banners cleanly and occupation dropdown populates in signup.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_frontend.md` (F-11, F-15).

---

### Batch 10: Structural Refactoring & Human Decision Workflows

Decoupling service exceptions, agent workflow state reducers, documentation alignment, cron scheduler task queues, and product card click event deduplication.

#### Fix 10.1: Decouple Service Exceptions from FastAPI HTTPException
- **Files Touched**: `apps/api/services/analytics_service.py`
- **One-line Description**: Raise domain exceptions (`NotFoundError`, `DataUnavailableError`) instead of `fastapi.HTTPException` to decouple business logic from the HTTP framework.
- **Severity**: Med
- **Verification Needed**: Verify route controllers catch domain exceptions and map to appropriate HTTP status codes.
- **Human Decision / Retraining**: **Human Decision**: Establish domain exception hierarchy across all services.
- **Mentioned in**: `REVIEW_backend.md` (N11).

#### Fix 10.2: Clarify State Reducer Channels in Agent Workflow
- **Files Touched**: `ai/workflow/state.py`
- **One-line Description**: Define explicit state channels or reducers for `ui_payload` to guarantee specialist UI cards merge cleanly into synthesis output.
- **Severity**: Med
- **Verification Needed**: Verify LangGraph state transitions preserve both card arrays and action chips through the graph run.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_ai.md` (F-03).

#### Fix 10.3: Align Feature Documentation in Model Card
- **Files Touched**: `ml/tracking/model_card.py`
- **One-line Description**: Update auto-generated Model Card template to document all 10 trained demographic and catalog input features rather than only 4.
- **Severity**: Low
- **Verification Needed**: Verify generated model card markdown lists all 10 features.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_ml.md` (N-14).

#### Fix 10.4: Clean Up Cron Scheduler Documentation and Plan Distributed Queue
- **Files Touched**: `ml/pipelines/cron_scheduler.py`
- **One-line Description**: Remove misleading docstring claiming 6-hour cache flushes, and document task queue transition path for high-availability deployments.
- **Severity**: Low
- **Verification Needed**: Verify docstrings accurately describe actual cron execution.
- **Human Decision / Retraining**: **Human Decision**: Decide whether to transition cron scheduling to Celery / ARQ / APScheduler.
- **Mentioned in**: `REVIEW_ml.md` (N-12, H-03).

#### Fix 10.5: Remove Redundant Click Handler Event Bubbling in Product Card
- **Files Touched**: `apps/reflex_app/reflex_app/components/product_card.py`
- **One-line Description**: Consolidate `on_click` handler to the root product card container to eliminate duplicate WebSocket event emissions on child clicks.
- **Severity**: Low
- **Verification Needed**: Click card sub-elements and verify only a single product detail open event is dispatched.
- **Human Decision / Retraining**: None.
- **Mentioned in**: `REVIEW_frontend.md` (F-14).

#### Fix 10.6: Update Settings Configuration in Benchmark Router
- **Files Touched**: `ml/eval/benchmark_router.py`
- **One-line Description**: Read S3 endpoint from settings and evaluate relocation of router evaluation benchmarks from `ml/eval/` to `ai/eval/`.
- **Severity**: Low
- **Verification Needed**: Verify benchmark evaluation connects to configured S3 storage.
- **Human Decision / Retraining**: **Human Decision**: Decide on relocating benchmark scripts to `ai/`.
- **Mentioned in**: `REVIEW_ml.md` (N-09, §5).

---

## 3. Human Decisions and Retraining Matrix

| Fix ID | File(s) Touched | Decision / Retraining Type | Details |
| :--- | :--- | :--- | :--- |
| **Fix 2.3** | `core/security.py` | **Human Decision** | Pre-hashing with SHA-256 standardizes passwords before bcrypt. If active user accounts exist in dev databases, they will need re-seeding or a password migration script. |
| **Fix 3.3** | `ml/features/imputation.py` | **Retraining Required** | Removing `"purchase"` from MissForest features eliminates target leakage and train/serving skew, requiring MissForest and downstream models to be retrained. |
| **Fix 4.2** | `ml/models/regression.py` | **Retraining Required & Decision** | Reclassifying `occupation` from numeric passthrough to categorical ordinal encoding alters feature matrices, requiring regression models to be retrained. |
| **Fix 4.7** | `docker/Dockerfile.ui` | **Human Decision** | Creating a split `requirements-ui.txt` slashes UI Docker image size by >1.5 GB; team must approve managing two requirements files. |
| **Fix 6.7** | `apps/reflex_app/reflex_app/components/bot_drawer.py` | **Human Decision** | Live voice mode is currently a non-functional mock that locks the UI. Team must decide whether to implement WebRTC/Web Audio or display a "Beta Soon" badge. |
| **Fix 8.1** | `core/db/repository.py` | **Human Decision** | Transitioning from multiple inheritance to composition replaces a God Object. Team must align on injecting individual repositories directly into FastAPI dependencies. |
| **Fix 9.3** | `core/db/session.py` | **Human Decision** | Transitioning from ad-hoc repository connections to an atomic Unit of Work pattern requires passing shared SQLAlchemy sessions into repository instances. |
| **Fix 10.1** | `apps/api/services/analytics_service.py` | **Human Decision** | Decoupling service layer from FastAPI HTTP exceptions requires establishing a shared domain exception hierarchy in `core/exceptions.py`. |
| **Fix 10.4** | `ml/pipelines/cron_scheduler.py` | **Human Decision** | Replacing blocking in-memory sleep loops with distributed task queues (e.g. Celery / ARQ / APScheduler) for multi-worker production deployments. |
| **Fix 10.6** | `ml/eval/benchmark_router.py` | **Human Decision** | Deciding whether router evaluation benchmarks belong under `ai/eval/` rather than `ml/eval/`. |

---

## 4. Verification & Execution Rules

When executing any fix in this plan:
1. **Never edit files outside the target batch** during that batch's execution cycle.
2. **Strictly verify metric directions** before modifying comparison logic (e.g., in `Fix 2.4`, R² is higher-is-better; candidate must exceed champion by positive delta).
3. **Verify parameterization** before modifying SQL queries (e.g., in `Fix 4.1`, bind `:top_k` as integer parameter).
4. **Run domain tests** after each batch to ensure zero regressions before advancing to the next batch.
