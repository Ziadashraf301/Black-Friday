# WP1: Config, Docker, CI, Makefile

Read prompts/fix_common.md and follow it. Fixes from FIX_PLAN.md: 1.1, 1.2, 1.3, 1.5, 1.6, 1.7, 2.6, 4.7, 8.7. (Fix 1.4 belongs to WP2.)

Notes and required verification:
- 1.2 core/config.py: GEMINI_API_KEY Optional; URL-encode DB/Redis passwords with urllib.parse.quote_plus; consolidate duplicated validators; S3/MINIO settings exist (WP0 may have added them). Verify with a test: Settings() builds with GEMINI_API_KEY unset; a password containing @ : / # ? produces a valid URL that parses back to the original password. Check every place that reads GEMINI_API_KEY handles None gracefully (grep and fix).
- 1.3 core/logging.py: no directory creation at import time; create lazily with error handling. Test: import with LOG_DIR pointing to an unwritable path does not raise.
- 1.5 add locust to requirements.txt; verify `python -c "import locust"` after install.
- 1.1 CI: add pgvector/pgvector:pg16 and redis:7-alpine service containers with health checks and matching env vars. Verify the YAML parses (`python -c "import yaml; yaml.safe_load(open('.github/workflows/ci-cd.yml'))"`) and the env names match core/config.py. Also make sure the CI installs what the tests need.
- 1.6 / 2.6: mount ./data in docker-compose for model-api; COPY data/ in Dockerfile.api; add a HEALTHCHECK probing /health (confirm that route exists). CHECK .dockerignore first: it must exclude huge files (train.csv, big CSVs, .web/, node_modules, .git, models caches) so the image does not bloat; create or fix it. Verify with `docker compose build model-api` and, if feasible, `docker compose run --rm model-api ls /app/data`.
- 1.7 apps/reflex_app/rxconfig.py: api_url from env. CAREFUL: Reflex's api_url is the URL of REFLEX's own backend, not the FastAPI service. Confirm which it is before changing; do not point it at the FastAPI service by mistake. State your finding in the report.
- 4.7 Dockerfile.ui: create requirements-ui.txt containing only what the Reflex UI imports (derive it by grepping imports in apps/reflex_app and its dependencies); copy data/. Verify: `pip install --dry-run -r requirements-ui.txt` resolves, and `python -c "import reflex_app.reflex_app"` works in a clean venv with only those requirements if feasible.
- 8.7 Makefile: add ai/ and evaluation/ to flake8/black/isort, make `clean` cross-platform with a python one-liner. Verify `make lint` runs and `make clean` works on Windows. If lint reports pre-existing violations in files you did not change, list them, do not mass-reformat.
