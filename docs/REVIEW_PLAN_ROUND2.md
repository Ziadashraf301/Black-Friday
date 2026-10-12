# Code Review Runbook — Round 2

> Instructions for the AI agent. The user starts each session with:
> **"Read docs/REVIEW_PLAN_ROUND2.md and do the next step."**

Round 2 works through the deferred findings and leftover notes from round 1 (see "Round-2 items" in [REVIEW_LOG.md](REVIEW_LOG.md)). The `review/round-2` branch is created by the first session (step 1), from an up-to-date `main`.

## Rules (every session)

1. Read this file and `docs/REVIEW_LOG.md` first. The next step is the first one below not marked `[x]`.
2. Do **one step per session**, then stop and tell the user to run `/clear`.
3. Work on branch `review/round-2`. Never push. Commit only after the user approves the diff.
4. Review level: `medium` (use `low` only where noted). Review only the listed paths.
5. Don't re-report findings already in the log as fixed / deferred / won't fix.
6. Fix only `high`/`medium` severity bugs unless the user says otherwise; log the rest as `deferred`. The deferred items named in a step are already approved for that step.
7. Every fix gets a test where practical. After fixing, run only the tests related to the touched files (plus `tests/architecture/test_architecture.py`), one file at a time with `--timeout=45`; the full suite hangs on Windows (see the Baseline in the log). Lint only the touched files (flake8, black --check, isort --check), except in step 1. If a test takes a long time, stop and find out why.
8. Follow all CLAUDE.md rules (layering, no `print()`, config via `settings`, mlflow only in `core/tracking/`).
9. Never replace a trained model, the ONNX exports in `models/`, or the stored customer personas without asking the user first (see step 6). Never print `.env` or secrets.

## Per-step procedure

1. Do the work listed for the step. Where the step is a review rather than a planned change, review its paths with the `code-review` skill at the given level.
2. Add any new findings to `docs/REVIEW_LOG.md` under a "Round 2" heading, with IDs continuing the round-1 prefixes (next free: `CORE-19`, `SEC-11`, `PIPE-15`, `ANL-12`, `INF-21`, `UI-8`), file:line, severity, status `open`.
3. Show the user a short table and ask which to fix.
4. Fix the approved findings, add tests, run tests and lint.
5. Show the diff summary. After the user approves, commit as `review(<area>): <summary> (<IDs>)`, one commit per finding.
6. Update the log statuses with the commit hash, mark the step `[x]` below, and tell the user to `/clear`.

## Steps

Order: 1, 3, 4, 6, 6.5, then the RUN LAST steps 2 and 5 (planned for 2026-10-13), then 7. The next step is the first one in this order not marked `[x]`. In any step, never rebuild Docker images or download anything larger than ~50 MB without asking the user first.


- [x] **1. Repo-wide lint pass:** create `review/round-2` from an up-to-date `main`. Run `black` and `isort` on `apps/`, `evaluation/`, `ml/` and `tests/`, then fix the remaining flake8 violations (F401, E402, W291/W293, E501). Formatting-only commits, kept separate from any behaviour change; run the tests of each touched directory afterwards to confirm nothing changed. Then widen `LINT_CLEAN_PATHS` in the `Makefile` to every directory that passes (the goal is `LINT_CLEAN_PATHS = LINT_PATHS`) and keep black pinned (`>=25.1,<26`). Baseline to beat: flake8 258 violations (97 outside `tests/`), black 93 files, isort 45 files.
- [ ] **2. (RUN LAST, after step 6.5 and before step 7: big downloads and Docker rebuilds; ask the user to confirm they have time and good internet before starting) Dependency lock file (INF-10, INF-17):** add a lock for the Python dependencies (for example `pip-compile` or `uv pip compile` outputs for `requirements.txt`, `requirements-ml.txt`, `requirements-ui.txt` and `requirements-dev.txt`), use it in the Dockerfiles and CI, and document how to refresh it. Confirm the API image really runs on the slim `requirements.txt` beyond the health check (login, predict-price, a bot call). Keep the MLflow client at `>=2.20.0,<3` to match the server image.
- [x] **3. Alembic migrations (CORE-16):** make the ORM the real schema. Add the `CustomerSegment` columns the segmentation pipeline writes (`age_group`, `popular_category`, `purchase_amount_variability`, `updated_at`), a `semantic_query_cache` model, `JSONB` instead of `JSON`, `Numeric` instead of `Float`, and gender/city column lengths that match the SQL (VARCHAR(8) / VARCHAR(4)). Introduce Alembic with a baseline migration that matches the live schema (`docker/postgres/init_schema.sql`) and replace `core/db/migrations/init_semantic_cache.py`. Check the live shape of `product_network_metrics`. Test the migration on a scratch database, never on the warehouse data. Paths: `core/db/models/`, `core/db/`, `docker/postgres/`.
- [x] **4. Trusted proxy (SEC-9):** add `X-Forwarded-For` handling to `client_ip` in `apps/api/client_identity.py`, honoured only when the peer is in a configured trusted-proxy list (a new `settings` field, empty by default so nothing changes). Use the rightmost untrusted hop. Make sure rate limits, `SecurityBanMiddleware` and the guest identity (SEC-8) all use the same function, and add tests for spoofed headers from untrusted peers. Run `security-review` on the diff. Paths: `apps/api/client_identity.py`, `apps/api/rate_limiting/`, `apps/api/middleware/`, `core/config.py`.
- [ ] **5. (RUN LAST, after step 6.5 and before step 7: big downloads and Docker rebuilds; ask the user to confirm they have time and good internet before starting) UI build-time installs and non-root containers (INF-12, INF-19, UI-7 leftovers):** the UI image installs bun/npm packages at every start; do it (and the Reflex frontend build) at image build time. Run the API and UI containers as a non-root `USER`; decide with the user how the bind-mounted `models/`, `data/` and `logs/` are owned on Linux hosts before changing anything, and say what could not be tested on Windows. Fix `ShoppingState.handle_bot_trigger_click` returning a `bool` (Reflex `TypeError`). Validate prices once at the catalog boundary and replace the other inline `$x.2f` sites. Paths: `docker/`, `docker-compose.yml`, `apps/reflex_app/`. Level `low` for the UI code.
- [x] **6. ML: ANL-9 and PIPE-13 (needs a retrain):**
  - ANL-9: Item2Vec uses the whole basket as the window instead of `window=5` over baskets in arbitrary order (`ml/market_basket/item2vec.py`, `ml/features/basket_encoder.py`).
  - PIPE-13: `frequency` counts distinct products, not purchases (`ml/features/customer_features.py`). Fixing it changes the clusters and personas.
  - Make the code changes and tests first. Then **stop and ask the user before** running `make basket`, `make segmentation` or `make train`, and before replacing any model, embedding, graph metric or persona in the database or in `models/`. Back up the current artifacts first (`models_backup_<date>/`), retrain into a scratch location, and show the user a before/after comparison (coverage, similarity, Apriori alignment, cluster sizes, persona names and counts, recommendation spot checks). Promote only on the user's explicit go-ahead. The README benchmark and persona tables must be updated with newly measured numbers, never estimated ones.
- [ ] **6.5. Finish every remaining item (no round 3):** the user wants everything done in round 2 except deployment (steps 2 and 5 run after this step). Fix, one commit each with tests:
  - CORE-15: the semantic-cache HNSW search filters by `user_key` after the vector search; filter inside the query (or partial index / iterative scan) so per-user results are not lost.
  - SRV-5 / ANL-11: decide and wire `LGBM_SUBSAMPLE` + `LGBM_SUBSAMPLE_FREQ` (keep the champion unchanged by default) and either use or remove the unused `SEGMENTATION_METRIC`.
  - INF-11: look up and pin the newest stable minio / mc release tags (compose file only; do not pull or rebuild images without asking).
  - Remove the duplicate `_ensure_semantic_cache_schema` in `warehouse_repo.py` now that Alembic owns the schema.
  - Reflex `do_login` / `do_signup` (and any other handler) set a loading flag without `yield`, so the spinner never shows; add `yield` after setting it.
  - The step-7 structural ideas from round 1 (validator on `RoutingDecision`, shared `is_cacheable`, `track_order` returning `Optional`, scoped cache view, shared product-id normaliser): do each that is a clear win, log the rest as won't fix with the reason.
  - Lazy MLflow tracer so importing `ai.observability.tracing` makes no network call (tests no longer depend on the unit network guard).
  - Re-run the e2e suite against the live `gemini-3-flash-preview` and fix what fails.
  - The 5 observations in section 6 of `docs/DATABASE_ERD.md`, each as an Alembic migration (0003+) with the ORM updated to match, tested on a scratch database first, and applied to the live database only after a `pg_dump` backup: (1) prices in `curated_products` and `user_purchases.predicted_usd` from `double precision` to `numeric`; (2) `user_carts.updated_at` from `varchar` to `timestamptz` (convert existing values safely); (3) align `app_users.gender` / `city_category` and `user_purchases.product_id` sizes with the warehouse; (4) add foreign keys where they are safe for the application tables (e.g. `user_purchases.product_id`), never on the bulk-loaded warehouse tables, and check existing rows first; (5) document the two user-id spaces (dataset customers vs app accounts) in `docs/DATABASE_ERD.md`, and rename only if it's low-risk. Then update `docs/DATABASE_ERD.md` (and regenerate the PDF if possible) to match.
  - Any other item marked open or deferred in `REVIEW_LOG.md` that is not deployment (steps 2 and 5 run separately).
- [ ] **7. Wrap-up:** rerun all tests (`make test-unit`, `make test-slow`, then integration and e2e one file at a time) and `make lint`, compare with the round-1 final numbers (530 collected; lint figures in the log), write a "Round 2" summary at the top of `REVIEW_LOG.md`, and update the README counts and `CLAUDE.md` if anything changed (lint paths, lock file, Alembic, new settings). Confirm that nothing is left open except deployment; there is no round 3.
