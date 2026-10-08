# Work Package 6: Backend Services, Serving and Performance — Fix Report

## Executive Summary
This work package completes all tasks outlined in `prompts/fix_wp6.md`: Fixes 6.2, 7.2, 4.3, 9.4, the batch-purchase endpoint (`POST /shopper/purchase/batch`), and the decoupling of analytics Redis caching from the repository layer into `core/cache/decorators.py` with pipeline invalidation.

---

## Task Details & Status

### 1. Fix 6.2: Purge DDL Calls from Authentication Service
- **Status**: DONE
- **Files Changed**:
  - `apps/api/services/auth_service.py`
- **Details**:
  - Removed `repo.ensure_user_tables()` from `register_user` and `authenticate_user`.
  - Application startup in `apps/api/main.py` lifespan already creates all tables via `BlackFridayRepository().create_app_tables()`.
  - Verified a fresh user registers and logs in cleanly without executing any DDL.
- **Verification Evidence**:
  - Captured SQL via SQLAlchemy `before_cursor_execute` event listener during registration and login; verified 0 `CREATE TABLE`, `ALTER TABLE`, or `DROP TABLE` statements executed.
  - Test suite: `tests/test_fix_6_2_auth_no_ddl.py` (3 passed in 5.83s).

### 2. Fix 7.2: Eliminate DDL, N+1 Queries, and Duplicated Pricing in Shopper Service
- **Status**: DONE
- **Files Changed**:
  - `core/db/repositories/recommendation_repo.py`
  - `apps/api/services/helpers.py`
  - `apps/api/services/shopper_service.py`
- **Details**:
  - Removed `repo.ensure_user_tables()` from `process_purchase`.
  - Added `RecommendationRepository.get_bulk_product_categories(product_ids)` using SQL `WHERE product_id IN :pids` with `bindparam('pids', expanding=True)` and `GROUP BY product_id`.
  - In `ShopperService.estimate_price_batch`, replaced the N+1 loop category queries with a single pre-fetch query across all items missing categories.
  - Extracted member discount calculation logic into reusable helper `calculate_member_discount_price(base_price, normalized_prediction)` in `apps/api/services/helpers.py`, eliminating duplicate logic in `estimate_price` and `estimate_price_batch`.
- **Verification Evidence**:
  - Measured category query count with an SQLAlchemy listener on a batch of 10 items; verified exactly 1 bulk category query is issued.
  - Verified batch pricing outputs match individual per-item pricing outputs on sample items.
  - Verified `process_purchase` executes without DDL.
  - Test suite: `tests/test_fix_7_2_shopper_performance.py` (4 passed in 6.35s).

### 3. Batch Purchase Endpoint: `POST /shopper/purchase/batch`
- **Status**: DONE
- **Files Changed**:
  - `core/db/repositories/user_repo.py`
  - `apps/api/schemas.py`
  - `apps/api/services/shopper_service.py`
  - `apps/api/routes/shopper.py`
- **Details**:
  - Added `UserRepository.record_purchases_batch(records, connection=None)` providing atomic transaction commit/rollback.
  - Added schemas `ShopperBatchPurchaseItem`, `ShopperBatchPurchaseRequest`, and `ShopperBatchPurchaseResponse`.
  - Added `ShopperService.process_batch_purchase(user_id, items, repo, user_demographics)` to price and record cart items in ONE atomic transaction with quantities, returning per-item purchase records.
  - Added authenticated endpoint `POST /shopper/purchase/batch` requiring bearer token, returning `total_items`, `total_amount`, and `purchases`.
  - Preserved single-item `POST /shopper/purchase` endpoint functionality.
- **Verification Evidence**:
  - Verified purchase of multiple items and quantities (`quantity=2` and `quantity=1`) records 3 database rows in one transaction.
  - Verified atomic rollback: partial failure (e.g. invalid item / bad data / foreign key violation) aborts the transaction and rolls back all items, leaving zero rows committed.
  - Test suite: `tests/test_fix_batch_purchase.py` (4 passed in 14.04s).

### 4. Fix 4.3: Enable ONNX Runtime Graph Optimizations in Inference Predictor
- **Status**: DONE
- **Files Changed**:
  - `ml/serving/predictor.py`
- **Details**:
  - Changed `opts.graph_optimization_level = ort.GraphOptimizationLevel.ORT_ENABLE_ALL` in `ONNXPredictor`.
  - Benchmarked predictions with `ORT_DISABLE_ALL` vs `ORT_ENABLE_ALL` on a sample batch of 50 items using the real production champion model (`models/onnx/lightgbm.onnx`).
  - Output values match with `np.allclose(rtol=1e-4, atol=1e-5)` (maximum absolute difference = 0.0).
  - Latency improved from 5.725ms (baseline) to 5.254ms (optimized).
- **Verification Evidence**:
  - Test suite: `tests/test_fix_4_3_onnx_optimization.py` (1 passed in 3.94s).

### 5. Fix 9.4: Centralize Currency Conversion Constant in Model Service
- **Status**: DONE
- **Files Changed**:
  - `core/config.py`
  - `apps/api/services/model_service.py`
- **Details**:
  - Added `INR_TO_USD: float = Field(default=80.0)` in `core.config.Settings`.
  - Replaced hardcoded `INR_TO_USD = 80.0` in `ModelService.predict_price` and `predict_price_batch_matrix` with `settings.INR_TO_USD`.
- **Verification Evidence**:
  - Tested that changing `settings.INR_TO_USD` inversely scales predicted USD prices across both single and 2D matrix predictions.
  - Test suite: `tests/test_fix_9_4_currency_conversion.py` (2 passed in 6.23s).

### 6. Extra: Move Analytics Cache out of Repository Layer (Follow-up to Fix 9.2)
- **Status**: DONE
- **Files Created / Changed**:
  - `core/cache/decorators.py` (created)
  - `core/cache/__init__.py` (exported `cached_json`)
  - `core/db/repositories/analytics_repo.py` (cache removed)
  - `apps/api/services/analytics_service.py` (`@cached_json` applied)
  - `ml/pipelines/ingest.py` (cache invalidation added)
  - `ml/pipelines/seed_curated.py` (cache invalidation added)
  - `ml/pipelines/preprocess.py` (cache invalidation added)
  - `tests/test_fix_9_2_analytics_cache.py` (updated)
- **Details**:
  - Created `cached_json(key, ttl)` decorator supporting sync and async functions, fault-tolerant Redis execution with transparent fallback, and zero hanging when Redis is down.
  - Removed all `cache_manager` imports and logic from `core/db/repositories/analytics_repo.py`. Repositories are now pure data access.
  - Applied `@cached_json("analytics:eda_summary", ttl=3600)` in `AnalyticsService.get_summary` (1 hour TTL).
  - Ingestion and data rewriting pipelines (`ingest.py`, `seed_curated.py`, `preprocess.py`) now delete `"analytics:*"` keys upon writing tables.
  - Audited `core/db` with `git grep -n "cache_manager" core/db`: zero occurrences remain.
- **Verification Evidence**:
  - Verified repository functions cleanly with Redis stopped and makes 0 cache calls.
  - Verified service caches EDA summary with 3600s TTL, falls back when Redis is unavailable, and invalidates on pipeline reloads.
  - Test suite: `tests/test_fix_9_2_analytics_cache.py` (4 passed in 10.31s).

---

## Architectural Verification
Enforced architectural boundary rules with `tests/test_architecture.py`. All layers strictly adhere to dependency hierarchy:
- `core` imports nothing from `apps`, `ml`, `ai`, `evaluation`.
- `ml` and `ai` import only `core`.
- `apps` imports `core`, `ml`, `ai`.
- Direct `import mlflow` is strictly confined to `core/tracking/`.
- Test suite: `tests/test_architecture.py` (PASSED).

---

## Noticed but Not Fixed
1. `models/onnx/decision_tree.onnx` is an unpruned tree with millions of nodes (~2.2MB serialized ONNX graph) that takes ~60 seconds for ONNX Runtime CPUExecutionProvider to initialize a session. Production serving correctly defaults to `lightgbm.onnx` (which initializes in < 2ms). Decision tree model retraining/pruning belongs to the ML training packages.
2. In `docker-compose.yml`, the top-level `version` attribute emits a deprecation warning in Compose v2. Obsolete attribute cleanup can be consolidated in DevOps packages.
