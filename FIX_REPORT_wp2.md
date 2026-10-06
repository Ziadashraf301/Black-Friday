# WP2: Database Foundation Fix Report

## Overview
- **Work Package**: WP2 (Database foundation)
- **Branch**: `review-fixes`
- **Status**: COMPLETE
- **Test Results**: 94 passed, 2 pre-existing failures (identical to `baseline_tests.txt`), 0 regressions.

---

## Fix Details & Verification

### Fix 3.1: MRO Table Creation Collision & Vector Extension
- **Status**: DONE
- **Finding**: `UserRepository.create_app_tables` collided with `BaseRepository.create_app_tables`.
- **Changes**:
  - Renamed `UserRepository.create_app_tables` to `ensure_user_tables`.
  - Updated all callers in `apps/api/services/auth_service.py` (lines 26, 78) and `apps/api/services/shopper_service.py` (line 350).
  - Hardened `BaseRepository.create_app_tables` to issue `CREATE EXTENSION IF NOT EXISTS vector;` before `Base.metadata.create_all(bind=self.engine)`.
- **Verification**:
  - Test: `tests/test_fix_3_1_mro_and_tables.py::test_base_repository_create_app_tables_on_scratch_db` (PASSED).
  - Scratch database verification created all 8 ORM tables (`raw_black_friday_features`, `black_friday_cleaned`, `curated_products`, `customer_segments`, `product_network_metrics`, `app_users`, `user_purchases`, `user_carts`).

### Fix 1.4: Container Database Initialization Schema
- **Status**: DONE
- **Finding**: `docker/postgres/init_schema.sql` had a hardcoded `\connect fridayblack;` breaking custom database names, and was missing application tables (`app_users`, `user_purchases`, `user_carts`, `semantic_query_cache`).
- **Changes**:
  - Removed `\connect fridayblack;` (container entrypoint handles database targeting safely).
  - Added DDL for `app_users`, `user_purchases`, `user_carts`, and `semantic_query_cache` (including HNSW index `idx_semantic_query_cache_embedding_hnsw` using `vector_cosine_ops`).
- **Verification**:
  - Executed `/docker-entrypoint-initdb.d/02-init_schema.sql` inside Postgres container on a temporary scratch database.
  - Verified creation of all 9 tables and 28 indexes, including `vector` extension and HNSW indexing.

### Fix 2.1: Dataframe Ingestion Dtype Preservation & Safe Truncate Fallback
- **Status**: DONE
- **Finding**: `BaseRepository.bulk_copy_df` had a destructive fallback `to_sql(if_exists="replace")` that dropped tables and destroyed custom indexes/HNSW vectors, plus an aggressive `float -> Int64` heuristic corrupting nullable floats like `product_category_2/3`.
- **Changes**:
  - Added `user_carts` and `semantic_query_cache` to `BaseRepository.VALID_TABLES`.
  - Replaced `to_sql(if_exists="replace")` fallback with `self.truncate_table(table_name, restart_identity=True)` + `data.to_sql(..., if_exists="append")`.
  - Removed `float -> Int64` heuristic so whole-number floats retain their `float64` dtype as expected by warehouse models.
- **Verification**:
  - Tests in `tests/test_fix_2_1_base_repo.py`:
    - `test_valid_tables_includes_user_carts_and_semantic_cache` (PASSED)
    - `test_dataframe_with_whole_number_floats_preserves_dtype` (PASSED)
    - `test_copy_fallback_preserves_table_and_indexes` (PASSED - HNSW and GIN indexes survive failed COPY fallback).

### Fix 3.2: Cleaned Warehouse Table Product ID Index
- **Status**: DONE
- **Finding**: `black_friday_cleaned` ORM model was missing an explicit index on `product_id`.
- **Changes**:
  - Added `index=True` to `product_id` column in `BlackFridayCleaned` (`core/db/models/warehouse.py`).
- **Verification**:
  - Verified generated SQLAlchemy index `ix_black_friday_cleaned_product_id` on model metadata and database DDL.

### Fix 4.1: SQL Parameterization & Removal of Inline DDL
- **Status**: DONE
- **Finding**: `_execute_rrf_query` and Tier 4 fallback formatted `top_k` as an f-string; read and cart methods called inline DDL (`create_all` / `ensure_user_carts_table`).
- **Changes**:
  - Bound `:top_k` safely as an integer parameter in SQL text queries (`WarehouseRepository._execute_rrf_query` and Tier 4 doorbuster query).
  - Removed inline `Base.metadata.create_all()` from `seed_curated_products()` and `get_curated_products()`.
  - Removed inline `ensure_user_carts_table()` from `save_user_cart_snapshot()` and `load_user_cart_snapshot()`.
  - Verified `apps/api/main.py` startup lifespan already invokes `create_app_tables()`.
- **Verification**:
  - Tests in `tests/test_fix_4_1_param_and_ddl.py`:
    - `test_malicious_top_k_is_safely_rejected` (PASSED - SQL injection payloads rejected).
    - `test_no_ddl_issued_during_get_curated_and_cart_calls` (PASSED - SQLAlchemy engine listener confirmed 0 DDL statements issued during read and cart operations).

### Fix 9.1: Recommendation Category Query Index Scan & Session Pass-through
- **Status**: DONE
- **Finding**: `RecommendationRepository.get_product_categories` did not support passing an active database session and executed a full table scan on 550k rows when `product_id` was unindexed.
- **Changes**:
  - Added optional `session: Optional[Any] = None` argument to `get_product_categories(product_id, session=None)`.
  - Utilizes `ix_black_friday_cleaned_product_id` index.
- **Verification**:
  - Ran `EXPLAIN` against populated table:
    `Bitmap Heap Scan on black_friday_cleaned ... Bitmap Index Scan on idx_cleaned_product_id (cost=0.00..27.69 rows=1530)` instead of sequential scan.

### Fix 9.2: Analytics Summary Redis Caching
- **Status**: DONE
- **Finding**: `AnalyticsRepository.get_eda_summary` recomputed heavy aggregations on raw warehouse tables without caching.
- **Changes**:
  - Integrated `cache_manager.get_json` and `cache_manager.set_json` with 1 hour TTL (3600 seconds) in `AnalyticsRepository.get_eda_summary()`.
  - Gracefully falls back to direct database execution if Redis is unavailable.
- **Verification**:
  - Tests in `tests/test_fix_9_2_analytics_cache.py`:
    - `test_two_repository_instances_share_cached_summary` (PASSED - verified 1 query executed, second instance retrieved from Redis).
    - `test_eda_summary_works_when_redis_is_down` (PASSED - verified clean fallback to database query).

### Fix 9.3: Remove Dead `get_db()` Session Helper
- **Status**: DONE
- **Finding**: Dead function `get_db()` in `core/db/session.py`.
- **Changes**:
  - Verified via repo-wide grep that `get_db` had 0 callers/importers across all modules.
  - Removed dead `get_db()` function from `core/db/session.py`.
- **Verification**:
  - Grepped repository: 0 occurrences outside git diff. All database sessions utilize `get_db_session()` context manager.

### Fix 8.1: Repository Composition Refactor
- **Status**: DONE
- **Finding**: `BlackFridayRepository` used complex multiple inheritance with diamond MRO issues.
- **Changes**:
  - Refactored `BlackFridayRepository` into composition with domain repository instances: `warehouse`, `analytics`, `segmentation`, `recommendation`, `users`.
  - Implemented dynamic `__getattr__` and `__dir__` delegation preserving exact method signatures, keyword arguments, and docstrings.
- **Verification**:
  - Tests in `tests/test_fix_8_1_composition.py`:
    - `test_black_friday_repository_composition_structure` (PASSED)
    - `test_every_public_method_of_old_facade_resolves` (PASSED - all public methods across domain repositories resolve and remain callable).

---

## Files Modified
- `apps/api/services/auth_service.py`
- `apps/api/services/shopper_service.py`
- `core/db/models/warehouse.py`
- `core/db/repositories/analytics_repo.py`
- `core/db/repositories/base.py`
- `core/db/repositories/recommendation_repo.py`
- `core/db/repositories/user_repo.py`
- `core/db/repositories/warehouse_repo.py`
- `core/db/repository.py`
- `core/db/session.py`
- `docker/postgres/init_schema.sql`

## Test Files Added
- `tests/test_fix_3_1_mro_and_tables.py`
- `tests/test_fix_2_1_base_repo.py`
- `tests/test_fix_4_1_param_and_ddl.py`
- `tests/test_fix_9_2_analytics_cache.py`
- `tests/test_fix_8_1_composition.py`

---

## Architectural & Layering Rules Check
- `tests/test_architecture.py` PASSED (100%).
- `core/db/` imports nothing from `apps/`, `ai/`, `ml/`, `evaluation/`.
- All callers preserve backward compatibility.

---

## Noticed But Not Fixed
1. In `tests/test_phase3_langgraph_agent.py`, `test_search_agent_node_hybrid_execution` and `test_details_agent_node_retrieval` fail because Tier 0 cache in router routes fast-path cached responses directly to `END` rather than `response_synthesis_node` (recorded in `baseline_tests.txt`). Scheduled for AI/Workflow package.
2. In `core/db/models/warehouse.py`, `CuratedProduct` stores `embedding` as `Vector(768)`; if Gemini embedding dimensions change, migration scripts will be required.
