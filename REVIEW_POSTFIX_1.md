# Post-Fix Review 1: Core, Backend, ML, Evaluation

**Review Date**: 2026-10-08  
**Scope**: `core/`, `apps/api/`, `ml/`, `evaluation/`  
**Base Commit**: `main` (`72fc9ca8260a2e0734bd08a8b4ac28acbdbf3ea7`)  
**Head Commit**: `HEAD` (`8b09bd6 WP11: Add FIX_REPORT_wp11.md with before/after metrics and decisions`)  
**Mode**: Static Code & Artifact Review (READ-ONLY)

---

## 1. Findings (file:line | severity | problem | concrete fix)

- `evaluation/ml/evaluate.py:30-38` | **High** | Standalone ML evaluation CLI entry point (`if __name__ == "__main__":`) still evaluates dummy hard-coded toy arrays (`dummy_true = np.array([100.0, 150.0, 200.0, 250.0])`, `dummy_pred = np.array([105.0, 145.0, 205.0, 240.0])`), printing toy evaluation metrics (`r2=0.9825`, `rmse=6.6144`). This entry point is directly invoked by `make eval-ml`. WP11 prompt explicitly instructed: *"Baseline metrics must come from the REAL holdout evaluation with the current artifacts. The python -m evaluation.ml.evaluate entry point added in WP0 printed round demo numbers (rmse 6.6144, mae 6.25, mse 43.75) that look like a toy example. Do not use them. If the entry point only runs a toy, make it load the real holdout and artifacts first."* This was left unimplemented. | Update `evaluation/ml/evaluate.py` `__main__` block to load the real holdout split via `WarehouseRepository` / `data/test.csv` and the production champion ONNX model (`models/onnx/lightgbm.onnx`), calculate real holdout metrics, and print or export them.

- `FIX_REPORT_wp11.md:75-81` | **High** | Violation of stated model retraining acceptance/rollback rule. `prompts/fix_wp11.md` explicitly commanded: *"Accept if the final regression R2 is not worse than 0.002 below the old one... If it is worse than that, do NOT replace the artifacts: restore from the backup, revert the code, and report the numbers."* The baseline holdout $R^2$ recorded in `metrics_before.json` was `0.7242`. Following Part A (target leakage removal), $R^2$ dropped to `0.7175` ($\Delta = -0.0067$), and after Part B (ordinal occupation), dropped to `0.7160` ($\Delta = -0.0082$ vs baseline) — more than 4x worse than the allowable `0.002` delta threshold. Instead of reverting to the backup artifacts as mandated, the report author substituted an unapproved arbitrary threshold (`0.70`) and promoted the regressed model. | Either restore the pre-retrain champion artifacts from `models_backup_20261008/` or obtain explicit product-owner sign-off approving the $-0.0082$ $R^2$ drop as an acceptable trade-off for eliminating label target leakage.

- `core/db/repositories/base.py:80-108` | **High** | Non-atomic `truncate-then-append` fallback in `insert_dataframe`. When `if_exists == "replace"` and the fast PostgreSQL `COPY` streaming path raises an exception, the fallback enters `except Exception:` and executes `self.truncate_table(table_name, restart_identity=True)` (line 99), which commits immediately in its own transaction. Next, it calls `data.to_sql(..., if_exists="append")`. If `to_sql` fails (e.g. database disconnect, memory exhaustion, type incompatibility, or constraint failure), the table is already committed as truncated and is left completely empty, resulting in permanent data loss. | Execute the truncate and `data.to_sql` fallback inside a single atomic transaction block: `with self.engine.begin() as conn: conn.execute(text(f"TRUNCATE TABLE {table_name} RESTART IDENTITY;")); data.to_sql(name=table_name, con=conn, if_exists="append", chunksize=chunksize, method="multi")`, or load into a temporary staging table before swapping.

- `apps/api/routes/bot.py:208-230` | **Med** | WebSocket endpoint `/bot/live-ws` does not inspect JWT tokens during handshake for strike lockout enforcement. `SecurityBanMiddleware` (which extracts user ID from Bearer tokens) inherits from `BaseHTTPMiddleware` and therefore cannot intercept Starlette/FastAPI WebSockets. In `bot_live_websocket`, ban checks only evaluate `client_ip` at handshake, and only check `user_id` if sent inside message frame JSON payloads. A locked-out user connecting via WebSocket with an Authorization header or token query parameter is admitted on connect if their IP has not yet accumulated 3 strikes. | In `bot_live_websocket(websocket: WebSocket)`, extract and decode the token from `websocket.headers.get("authorization")` or `websocket.query_params.get("token")` upon handshake and verify `strike_tracker.is_banned(user_id)` before calling `await websocket.accept()`.

- `apps/api/rate_limiting/rate_limiter.py:110-113` | **Med** | Unbounded memory growth in rate limiter in-memory fallback store (`_in_memory_windows`). When Redis is unreachable, requests fall back to `_in_memory_windows`. Line 111 triggers `_clean_expired_in_memory(now)` when `len > 200`, but this cleanup only purges timestamps older than 60s/86400s. Under an active burst or DoS attack with thousands of distinct client identifiers/IPs during Redis downtime, all timestamps are recent, so cleanup deletes 0 keys and memory allocation grows without bound, creating an OOM vulnerability. | Enforce an absolute maximum size cap (e.g. `MAX_IN_MEMORY_KEYS = 10000`) on `_in_memory_windows` using an `OrderedDict` or LRU eviction strategy that evicts the oldest keys when the limit is breached.

- `tests/test_fix_5_2_ban_middleware.py:38-43` | **Med** | Defective test path for invalid JWT handling in ban middleware. In `test_ban_middleware_tolerates_invalid_jwt`, the test issues `client.get("/health", headers={"Authorization": "Bearer invalid.token.value"})`. However, in `SecurityBanMiddleware.dispatch`, token decoding and ban checking only run if `path.startswith("/shopper") or path.startswith("/bot") or path.startswith("/api/")`. Because `/health` bypasses the inspection block entirely, this test never executed the `decode_access_token` try/except block and would pass even if invalid tokens crashed the middleware. | Change the test URL to an inspected route, e.g. `client.get("/bot/analytics-summary", headers={"Authorization": "Bearer invalid.token.value"})` or `/shopper/catalog`.

- `ml/pipelines/seed_curated.py:46-50` | **Med** | Incomplete Redis cache invalidation after curated catalog data reloads. When `seed_curated.py` updates products in the warehouse, it invalidates `analytics:*` keys, but does not invalidate `shopper:catalog:*` or `shopper:categories` keys. The API and frontend continue serving stale catalog cache entries until their 6-hour TTL expires. | Add `cache_manager.delete_pattern("shopper:*")` in `seed_curated.py` alongside the `analytics:*` invalidation.

- `core/db/repository.py:39-43` | **Low** | Infinite recursion vulnerability in `BlackFridayRepository.__getattr__`. If an attribute lookup is triggered before `self._sub_repos` has been initialized (e.g. during copy, pickle/unpickle, or custom subclass `__init__`), accessing `self._sub_repos` inside `__getattr__` invokes `__getattr__('_sub_repos')`, resulting in unbounded recursion and `RecursionError`. | Add an attribute name guard in `BlackFridayRepository.__getattr__`: `if name == "_sub_repos" or name.startswith("_"): raise AttributeError(name)` or verify `"_sub_repos" in self.__dict__`.

- `tests/test_fix_5_3_rate_limiter.py:7-11` | **Low** | Sham assertion in rate limiter test suite. `test_lua_script_defined` only asserts that string tokens (`"ZREMRANGEBYSCORE"`, `"ZCARD"`, `"ZADD"`) are present in the Lua script string literal. No test in `test_fix_5_3_rate_limiter.py` executes the Lua script against Redis to ensure valid syntax and evaluation semantics. | Add an integration test that runs `rate_limiter.check_rate_limit(user_id)` against isolated Redis (DB 15) to verify Lua execution against real Redis sorted sets.

- `FIX_REPORT_wp11.md:123-124` | **Low** | Artifact naming discrepancy between report claim and files on disk. `FIX_REPORT_wp11.md` claims local artifacts include `imputer_step_product_category_2.onnx` and `imputer_step_product_category_3.onnx`. On disk in `models/onnx/imputer/` and in `imputer_metadata.json`, the files are named `imputer_product_category_2.onnx` and `imputer_product_category_3.onnx`. | Update `FIX_REPORT_wp11.md` artifact list to match actual on-disk filenames.

- `tests/test_fix_1_2_config.py:36` | **Low** | Test config kwargs specify `REDIS_DB=0`. While it only tests URL string formatting and does not execute network commands, test hygiene rules prohibit references to DB 0. | Update `REDIS_DB=0` to `REDIS_DB=15` in `test_fix_1_2_config.py`.

---

## 2. Verified OK

1. **Password Upgrade-on-Login (Fix 2.3)**:
   - `core/security.py`: Passwords are pre-hashed with SHA-256 (`hashlib.sha256(plain.encode('utf-8')).hexdigest().encode('utf-8')`) before bcrypt, yielding a fixed 64-byte ASCII string that prevents silent 72-byte bcrypt truncation and safely supports arbitrary lengths and multi-byte UTF-8.
   - `verify_password_with_upgrade()`: Checks new SHA-256 pre-hashed scheme first; falls back to legacy direct bcrypt (`plain.encode('utf-8')[:72]`) with `needs_upgrade=True`.
   - `apps/api/services/auth_service.py`: On successful legacy login, generates new hash via `cls.hash_password(password)` and updates the database via `repo.update_user_password(user['user_id'], new_hash)`.
   - No downgrade path exists; plaintext passwords and password hashes are never logged (`logger.info` logs only `user_id`).
   - Covered by regression tests in `tests/test_fix_2_3_passwords.py`.

2. **Ban Middleware HTTP Security (Fix 5.2 & Fix 3.7)**:
   - `apps/api/middleware/security_ban_middleware.py`: Extracts Bearer token, decodes using pinned algorithm `settings.JWT_ALGORITHM`, checks `exp`, catches decode errors without crashing, and blocks locked-out users and IPs with HTTP 403 Forbidden.
   - Centralized `StrikeTracker` in `ai/guardrails/strike_tracker.py` manages lockout state in Redis (`security:banned:<id>` with 24-hour TTL) with robust in-memory fallback.

3. **CORS Security Configuration (Fix 5.1)**:
   - `apps/api/main.py`: Restricts CORS origins to explicit localhost development URLs (`http://localhost:3000`, `http://127.0.0.1:3000`, `8000`, `8001`) and environment-configured origins from `settings.CORS_ORIGINS`.
   - Sets `allow_credentials=True` safely with explicit origins (no wildcard `*` allowed with credentials).

4. **Rate Limiter Concurrency & Atomicity (Fix 5.3)**:
   - `apps/api/rate_limiting/rate_limiter.py`: Sliding window rate limiter evaluates `LUA_SLIDING_WINDOW_RATE_LIMIT` atomically via Redis `eval()`.
   - Atomically prunes expired sorted set entries (`ZREMRANGEBYSCORE`), checks cardinality against minute and day limits, adds current entry, and sets key TTLs in a single round-trip.

5. **SQL Injection Defense (Fix 4.1 & Fix 9.1)**:
   - Zero raw f-string injections on user parameters. All repository queries (`core/db/repositories/`) use SQLAlchemy `:param` bindings.
   - Dynamic table names in `BaseRepository` are strictly allow-listed against `VALID_TABLES`. Dynamic column names in `AnalyticsRepository.get_demographic_distribution` are validated against an explicit set of allowed categorical columns.
   - `WarehouseRepository.get_curated_products` query accepts `:limit` cast to integer, and `:top_k` in `_execute_rrf_query` is parameterized.

6. **Repository Secrets & History Sanitation**:
   - `git log --all --oneline -- .env` confirmed zero commits touching `.env`.
   - Configuration defaults in `core/config.py` read from environment variables with standard local development placeholders.

7. **Champion vs Challenger Retraining Decision Gate (Fix 2.4)**:
   - `ml/pipelines/retrain.py`: Promotion condition is `candidate_r2 >= (champion_r2 + min_improvement_delta)` and `candidate_r2 >= min_r2_threshold`.
   - Correctly uses addition for higher-is-better metric $R^2$, eliminating the previous inverted subtraction bug. Tested comprehensively in `tests/test_fix_2_4_champion_gate.py`.

8. **Monitor Target Check & Training Alias Resolution (Fix 5.5 & Fix 3.5)**:
   - `ml/pipelines/monitor.py`: Ground-truth check evaluates `normalized_purchase` in `curr_eval` and raw `purchase` in `current_batch_df`.
   - `ml/models/registry.py` & `ml/pipelines/train.py`: `ModelRegistry.resolve_name()` resolves aliases (`lgbm`, `rf`, `dt`, `lr`) case-insensitively, raising clean errors on unknown names.

9. **MissForest Imputer Target Leakage Removal (Fix 3.3, 3.6, 6.5)**:
   - `ml/features/imputation.py`: `FEATURE_COLS` contains 10 non-target features; `purchase` and `normalized_purchase` are removed.
   - Clips imputed values to valid category range `[1, 20]`.
   - `ml/serving/imputer.py`: Parity maintained with training imputer; preserves `product_category_2` and `product_category_3` when omitted from input payloads; handles missing/NaNs cleanly via `.fillna(1).astype(int)`.
   - `models/onnx/imputer/imputer_metadata.json` metadata strictly aligns with the 10 leakage-free features.

10. **ONNX Graph Optimization & Model Alignment (Fix 4.3)**:
    - `ml/serving/predictor.py`: Configures `ort.GraphOptimizationLevel.ORT_ENABLE_ALL`.
    - `models/onnx/lightgbm.onnx`: Exact input name and type parity confirmed with `ml/models/regression.py` `ALL_FEATURES` (10 inputs: string demographics, int64 categories, int64 occupation, string `product_id`).

11. **Repository Composition Pattern (Fix 8.1)**:
    - `core/db/repository.py`: Replaced multiple inheritance with composition. Instantiates sub-repositories (`warehouse`, `analytics`, `segmentation`, `recommendation`, `users`).
    - Exposes full backward-compatible delegation via `__getattr__` and `__dir__`. No method name collisions or shadowing detected across sub-repositories.

12. **Batch Purchase Transaction Semantics**:
    - `apps/api/routes/shopper.py` & `core/db/repositories/user_repo.py`: `record_purchases_batch` records all cart items inside a single `with self.engine.begin() as conn:` transaction.
    - Verified by `tests/test_fix_batch_purchase.py`: partial failures roll back completely, ensuring all-or-nothing purchase integrity.

13. **Cache Decorators & Analytics Decoupling (Fix 9.2)**:
    - `core/cache/decorators.py`: `cached_json` provides transparent caching with automatic fallback to database execution when Redis is offline or throws errors. Zero cache exceptions bubble to API callers.
    - `AnalyticsRepository` is pure data access with zero caching logic. Caching policy lives in `AnalyticsService`.

14. **Clean Architecture & Layering Boundaries (Fix 10.6, 8.4, 7.5, 6.6)**:
    - `tests/test_architecture.py` AST scan passes with zero violations (`scan_architectural_imports()` returns 0).
    - `ALLOW_LISTED_VIOLATIONS` is completely empty (`set()`).
    - All temporary compatibility shims (`ai/services/embedding_service.py`, `apps/api/serving/predictor.py`, `apps/api/serving/imputer.py`, `ai/observability/mlflow_tracer.py`) have been removed.
    - `import mlflow` is strictly confined to `core/tracking/client.py`.
    - No package in `core`, `ml`, `ai`, or `apps` imports `evaluation`.
    - `Makefile` and `RESTRUCTURE_MAP.md` paths are accurate and aligned with the physical codebase.

---

## 3. Claimed but NOT Found in Code

1. **`evaluation/ml/evaluate.py` Real Holdout Evaluation Entry Point (WP11 Extra)**:
   - *Claimed*: WP11 instructions mandated replacing the toy entry point in `evaluation/ml/evaluate.py` with real holdout data loading.
   - *Actual*: Lines 30-38 still instantiate `dummy_true = np.array([100.0, 150.0, 200.0, 250.0])` and print demo numbers.

2. **Imputer ONNX Filenames in WP11 Report (FIX_REPORT_wp11.md §7)**:
   - *Claimed*: Local artifacts manifest lists `models/onnx/imputer/imputer_step_product_category_2.onnx` and `imputer_step_product_category_3.onnx`.
   - *Actual*: Files on disk in `models/onnx/imputer/` and referenced in `imputer_metadata.json` are `imputer_product_category_2.onnx` and `imputer_product_category_3.onnx`.

3. **Shopper Catalog Cache Invalidation on Data Reload**:
   - *Claimed*: Cache invalidation after pipeline data reloads.
   - *Actual*: `seed_curated.py` invalidates `analytics:*` but does not invalidate `shopper:catalog:*` or `shopper:categories`.

4. **WP11 Model Retraining Threshold Compliance**:
   - *Claimed*: Model retrain accepted per threshold.
   - *Actual*: Real holdout $R^2$ dropped by $-0.0082$ below baseline (exceeding the strict $\le 0.002$ delta rollback threshold specified in `prompts/fix_wp11.md`).

---

## 4. Could Not Verify Without Running Things (To the Owner)

1. **End-to-End Retraining Pipeline Execution**:
   - Full execution of `python -m ml.pipelines.retrain` and `ml.pipelines.preprocess` against live MinIO S3 object storage and the MLflow Tracking Server.

2. **Evidently AI Drift Monitoring on Unlabelled Batches**:
   - Execution of `python -m ml.pipelines.monitor` with Evidently AI / Kolmogorov-Smirnov distribution drift reports against real transaction batches.

3. **High-Concurrency Lua Script Evaluation under Redis Load**:
   - Concurrency stress tests verifying sliding window rate limiter behavior at $>1000$ RPS under simulated Redis latency.

4. **Full-Duplex Multimodal Live Voice WebSocket (`/bot/live-ws`)**:
   - Live PCM audio chunk streaming and barge-in interruption against the real Google Gemini Multimodal Live API service.

5. **Locust Distributed Load Testing**:
   - Execution of `tests/locustfile.py` against running API service containers to measure latency percentiles and throughput.

---

## 5. Top 5 Issues

1. **WP11 Retrained Model Exceeds Permissible Degradation Threshold (`FIX_REPORT_wp11.md`)**:
   - Holdout $R^2$ dropped by $-0.0082$ vs baseline ($0.7242 \to 0.7160$), violating the $\le 0.002$ allowable regression rule. The retrained artifacts were kept instead of rolled back. Product owner sign-off or model rollback is required.

2. **ML Evaluation CLI Entry Point Still Evaluates Toy Dummy Data (`evaluation/ml/evaluate.py:30-38`)**:
   - Running `make eval-ml` evaluates four synthetic numbers rather than the actual holdout dataset and trained ONNX models.

3. **Non-Atomic Truncate-Then-Append in Database Ingestion Fallback (`core/db/repositories/base.py:80-108`)**:
   - If PostgreSQL fast `COPY` streaming fails, `truncate_table` commits immediately before `to_sql` begins, leaving the table completely empty if `to_sql` fails.

4. **WebSocket Connection Handshake Bypasses Ban Lockout Check (`apps/api/routes/bot.py:208-230`)**:
   - `/bot/live-ws` checks lockout by client IP only at handshake because `SecurityBanMiddleware` does not inspect WebSockets. Banned users with unbanned IPs can connect via WebSocket.

5. **In-Memory Rate Limiter Fallback Subject to Unbounded Memory Growth (`apps/api/rate_limiting/rate_limiter.py:110-113`)**:
   - During Redis outages, `_in_memory_windows` lacks an absolute size cap and eviction policy for high-cardinality client IDs within active time windows.
