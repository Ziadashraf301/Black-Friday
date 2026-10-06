# WP10: Final verification, lint, docs

Read prompts/fix_common.md and follow it. This is the integration pass after WP0 to WP9 (WP11 may come after it).

1. Full test run: `pytest tests/ -q`. Compare with baseline_tests.txt. Every test that failed before and still fails must be listed with the reason. Fix regressions.
2. Lint and format: run flake8/black/isort over core/ ml/ ai/ evaluation/ apps/ tests/ (use `make lint`). Auto-format only files that earlier WPs changed; list pre-existing violations elsewhere without mass-reformatting.
3. Architecture: tests/test_architecture.py passes; `git grep -n "import mlflow"` shows only core/tracking; no stale references to moved paths (use RESTRUCTURE_MAP.md to grep every old path in code, docs, Dockerfiles, Makefile, CI, README).
4. Docker smoke: `docker compose config`; build the api and ui images; bring the stack up (postgres, redis, minio, mlflow, api); check /health, /docs, a /shopper/predict-price call (guest), register + login, and a /bot/stream request (it may need a Gemini key; if absent, verify the graceful no-key behaviour). Tear down afterwards.
5. Docs: update README.md, system.md, docs/ and the architecture section so paths, commands (make targets, `python -m evaluation...`) and the dependency rule match reality. Add a short CONTRIBUTING-style note describing the layering rule and where shared things live (core/tracking, core/embeddings, evaluation/).
6. Produce FINAL_REPORT.md: a table of ALL fix ids in FIX_PLAN.md with status (DONE/SKIPPED/DEFERRED), the WP report that covers it, and the evidence. Deferred items and anything needing a human decision are listed at the top.

## Extra items (added after WP2)
- Remove every TODO-remove shim after proving there are no importers (git grep). The architecture test allow-list must end up EMPTY.
- core/config.py: use urllib.parse.quote(password, safe='') instead of quote_plus; verify with sqlalchemy.engine.make_url(url).password and the redis URL parser, including a password with a space.
- Check pg_indexes for duplicate indexes between init_schema.sql and the ORM models (for example black_friday_cleaned.product_id); keep one source of truth.
- Run the ingest and preprocess steps against a SCRATCH database to confirm bulk_copy_df works with float columns (no 'invalid input syntax for type integer').
- Verify the model-api HEALTHCHECK reports healthy (curl must exist in the image), prove COPY data/ without the bind mount, and import the Reflex app inside the UI image built from requirements-ui.txt.
- Finish with ONE formatting-only commit (black + isort over the whole repo), then re-run all tests and make lint, and run the CI steps in a fresh venv from a clean clone.
