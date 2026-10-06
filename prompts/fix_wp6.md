# WP6: Backend services, serving and performance

Read prompts/fix_common.md and follow it. Fixes from FIX_PLAN.md: 6.2, 7.2, 4.3, 9.4, plus the batch-purchase endpoint described below. (Fix 3.6, the serving imputer, is in WP11 because it needs the retrained artifacts.)

- 6.2 auth_service.py: remove create_app_tables() from register/login. Verify startup creates all tables (apps/api/main.py) and that a fresh database works end to end with register -> login. Test: capture SQL during login and assert no DDL.
- 7.2 shopper_service.py: remove create_app_tables() from process_purchase; replace the N+1 category lookups with one bulk query; extract the duplicated member-discount calculation into one helper. Test: batch price of N items issues a constant number of category queries (count statements with an SQLAlchemy listener); pricing results equal the old per-item results on a sample.
- Batch purchase endpoint: add POST /shopper/purchase/batch (authenticated) that records all cart items in ONE transaction with quantities, returning per-item results. Keep the single-item endpoint working. Test: partial failure rolls back everything. (The frontend will use it in WP8.)
- 4.3 predictor.py: enable ORT_ENABLE_ALL. Verify with a test that predictions with optimization on equal the previous setting (np.allclose on a sample batch of the real ONNX model, rtol 1e-4) and measure latency before/after; if outputs differ for any model, keep the old level for that model and explain.
- 9.4 model_service.py: replace hardcoded INR_TO_USD=80.0 with settings.INR_TO_USD (add to core/config.py if missing); test that changing the setting changes the price.

## Extra: move the analytics cache out of the repository (follow-up to Fix 9.2)
The Redis cache added to AnalyticsRepository.get_eda_summary in WP2 is in the wrong layer: repositories must be pure data access.
1. Create core/cache/decorators.py with a cached_json(key, ttl) decorator (sync and async if needed). On any Redis failure it must call the wrapped function and return its result, with no exception and no 2-second hang.
2. Remove all cache_manager imports and usage from core/db/repositories/analytics_repo.py (and any other repository except the semantic-cache methods in warehouse_repo).
3. Apply the decorator in apps/api/services/analytics_service.py for the EDA summary (1 hour TTL).
4. Invalidation: after ingest/seed pipelines load data (ml/pipelines/ingest.py, seed_curated.py, preprocess.py where tables are rewritten), delete the analytics:* keys.
5. Update tests/test_fix_9_2_analytics_cache.py: the repository test must pass with Redis stopped and prove no cache calls; the service test proves caching, TTL, fallback when Redis is down, and invalidation after a reload.
6. grep core/db for cache_manager: only the semantic-cache methods may remain, and say so in the report.
