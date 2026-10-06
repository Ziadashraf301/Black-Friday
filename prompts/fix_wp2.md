# WP2: Database foundation

Read prompts/fix_common.md and follow it. Fixes from FIX_PLAN.md: 1.4, 2.1, 3.1, 3.2, 4.1, 9.1, 9.2, 9.3, 8.1. Do them in this order (later ones depend on earlier ones).

Notes and required verification:
- 3.1 MRO collision: rename UserRepository.create_app_tables to ensure_user_tables; update every caller of the old override (grep all of apps/, ai/, ml/, tests/). Verify with a test on a SCRATCH database (create a temporary database, never use the real data): after BaseRepository.create_app_tables() ALL expected tables exist (list them from the ORM models). Check pgvector extension requirements for tables with Vector columns.
- 1.4 docker/postgres/init_schema.sql: add app_users, user_purchases, user_carts, semantic_query_cache (with HNSW index); remove the hardcoded \connect. FIRST read docker/postgres/init-multiple-dbs.sh and docker-compose to understand how the scripts run so the removal is safe. Verify by running the script against a scratch database in the postgres container and listing tables and indexes.
- 2.1 base.py: replace destructive if_exists="replace" fallback with TRUNCATE + append; add user_carts and semantic_query_cache to VALID_TABLES; remove the float->Int64 heuristic ONLY after checking what downstream code and schemas expect for columns like product_category_2/3 (they contain NaN). Test: force the COPY path to fail and assert an existing HNSW/GIN index survives; test a DataFrame with whole-number floats keeps its dtype.
- 3.2 warehouse.py: index=True on product_id of black_friday_cleaned; verify the generated DDL / pg_indexes.
- 4.1 warehouse_repo.py: bind :top_k as a parameter in every RRF/hybrid query; remove inline CREATE TABLE / create_all from read and cart methods (tables now come from startup, 3.1 + 1.4). Verify apps/api/main.py startup actually calls create_app_tables. Test: pass a malicious top_k string and assert it is rejected or bound safely; assert no DDL statements are issued during get_curated_products and cart calls (use SQLAlchemy event listener to capture statements).
- 9.1 recommendation_repo.py: use the new index and the repository session; verify with EXPLAIN (if the table is empty, verify the index exists and say so).
- 9.2 analytics_repo.py: Redis cache via cache_manager.get_json/set_json with 1h TTL. First confirm those methods exist in core/cache/redis_client.py. Test that two repository instances share the cached summary and that it still works when Redis is down.
- 9.3 session.py: remove get_db() only if nothing imports it (prove with grep).
- 8.1 repository.py: composition with delegation (warehouse, analytics, segmentation, recommendation, users). All existing callers and tests must pass unchanged. Add a test that every public method of the old facade still resolves.
