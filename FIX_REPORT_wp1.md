# FIX_REPORT_wp1.md: Work Package 1 (Config, Docker, CI, Makefile)

**Branch**: `review-fixes`  
**Status**: COMPLETED  
**Baseline Test Comparison**: 84 passed, 2 pre-existing failures (identical to `baseline_tests.txt` + 1 WP0 test + 5 WP1 regression tests). 0 regressions introduced.

---

## 1. Fix Status Summary

| Fix ID | Description | Status | Files Changed / Created |
|---|---|---|---|
| **1.1** | CI pgvector and redis service containers with health checks & env vars | **DONE** | `.github/workflows/ci-cd.yml` |
| **1.2** | `core/config.py`: Optional GEMINI_API_KEY, URL-encode passwords, consolidate validators | **DONE** | `core/config.py`, `tests/test_fix_1_2_config.py` |
| **1.3** | `core/logging.py`: Lazy log directory creation with error handling | **DONE** | `core/logging.py`, `tests/test_fix_1_3_logging.py` |
| **1.4** | `docker/postgres/init_schema.sql` schema updates | **SKIPPED** | Explicitly deferred to WP2 per `prompts/fix_wp1.md` |
| **1.5** | Add `locust` to `requirements.txt` | **DONE** | `requirements.txt` |
| **1.6** | Dockerfile.api COPY data/ and HEALTHCHECK probing /health | **DONE** | `docker/Dockerfile.api`, `docker-compose.yml`, `.dockerignore` |
| **1.7** | `apps/reflex_app/rxconfig.py`: Dynamic `api_url` from environment | **DONE** | `apps/reflex_app/rxconfig.py` |
| **2.6** | Mount `./data:/app/data` in `docker-compose.yml` for model-api & `.dockerignore` | **DONE** | `docker-compose.yml`, `.dockerignore` |
| **4.7** | Decouple UI container: create `requirements-ui.txt` and COPY data/ | **DONE** | `requirements-ui.txt`, `docker/Dockerfile.ui` |
| **8.7** | Makefile: Include `ai/` and `evaluation/` in lint targets; cross-platform clean | **DONE** | `Makefile` |

---

## 2. Detailed Verification and Evidence

### Fix 1.1: CI Service Containers & Environment Alignment
- **Implementation**: Added `pgvector/pgvector:pg16` (port 5432, healthcheck: `pg_isready -U postgres`) and `redis:7-alpine` (port 6379, healthcheck: `redis-cli ping`) service containers in `.github/workflows/ci-cd.yml`. Defined required environment variables (`POSTGRES_HOST: localhost`, `POSTGRES_PORT: 5432`, `POSTGRES_USER: postgres`, `POSTGRES_PASSWORD: postgres_password123`, `POSTGRES_DB: fridayblack`, `REDIS_HOST: localhost`, `REDIS_PORT: 6379`, `REDIS_DB: 0`) matching `core/config.py`.
- **Verification**: Verified YAML syntax parses with `python -c "import yaml; yaml.safe_load(open('.github/workflows/ci-cd.yml'))"` (exit code 0).

### Fix 1.2: Core Configuration Sanitization & Validation
- **Implementation**:
  - Made `GEMINI_API_KEY: Optional[str] = Field(default=None)` in `core/config.py`.
  - Added `urllib.parse.quote_plus` to encode passwords in `database_url`, `async_database_url`, and `redis_url`.
  - Consolidated duplicate integer field validators into a single `@field_validator(..., mode="before")` method `parse_optional_integer`.
  - Grepped all call sites for `GEMINI_API_KEY` (`synthesis_service.py`, `bot.py`, `core/embeddings/service.py`) and verified graceful handling of `None` without unhandled exceptions.
- **Verification**:
  - Regression tests in `tests/test_fix_1_2_config.py`:
    - `test_gemini_api_key_optional`: PASSED.
    - `test_special_characters_in_db_and_redis_passwords`: PASSED (passwords with `@ : / # ?` successfully encoded and roundtrip unquoted).
    - `test_consolidated_optional_integer_validator`: PASSED (empty string, none, null parsed to None; valid integers parsed to int).

### Fix 1.3: Lazy Logging Directory Creation
- **Implementation**: Removed module import-time `os.makedirs(settings.LOG_DIR)`. Implemented `_ensure_log_dir()` to create directory lazily on handler initialization with try/except protection and graceful fallback to console logging if the filesystem is unwritable or restricted.
- **Verification**:
  - Regression tests in `tests/test_fix_1_3_logging.py`:
    - `test_import_unwritable_log_dir_does_not_raise`: PASSED.
    - `test_get_logger_with_unwritable_log_dir`: PASSED (falls back to console `StreamHandler` when `PermissionError` is raised).

### Fix 1.5: Locust Dependency
- **Implementation**: Added `locust>=2.24.0` to `requirements.txt`.
- **Verification**: Installed package; executed `python -c "import locust; print(locust.__version__)"` returning `2.46.7` (exit code 0).

### Fix 1.6 & 2.6: Docker API Healthcheck, Data Mounting, and Dockerignore
- **Implementation**:
  - Created `.dockerignore` excluding heavy artifacts (`data/*.csv`, `*.csv`, `node_modules`, `.web`, `.git`, `.venv`, cache files, `mlruns`).
  - Added `COPY data/ /app/data/` to `docker/Dockerfile.api`.
  - Added `HEALTHCHECK` probing `curl -f http://localhost:8000/health || exit 1` in `docker/Dockerfile.api` (verified route `/health` exists in `apps/api/main.py`).
  - Added volume mount `- ./data:/app/data` to `model-api` in `docker-compose.yml`.
- **Verification**:
  - `docker compose config` validates cleanly without error.
  - `docker compose build model-api` completed successfully with code 0.
  - `docker compose run --rm model-api ls /app/data` executed with code 0 and confirmed files exist:
    `curated_products.json`, `extraction_benchmark_dataset.json`, `golden_benchmark_dataset.json`, `store_policies.json`, `test.csv`, `train.csv`.

### Fix 1.7: Reflex Dynamic API URL
- **Reflex Architecture Finding**:
  In Reflex, `api_url` in `rxconfig.py` specifies the URL where the compiled frontend client connects to Reflex's own event/websocket backend server (port 8001), NOT the FastAPI REST service (port 8000). The FastAPI endpoint is consumed via `API_BASE_URL` in `apps/reflex_app/reflex_app/state.py`.
- **Implementation**:
  Updated `apps/reflex_app/rxconfig.py` to read `api_url=os.getenv("REFLEX_API_URL", "http://localhost:8001")`.
- **Verification**: Verified via Python test that `rxconfig.config.api_url` honors `REFLEX_API_URL` environment overrides.

### Fix 4.7: Frontend Dependency Decoupling & Static Data Fallback
- **Implementation**:
  - Created lightweight `requirements-ui.txt` containing only Reflex UI dependencies (`reflex>=0.7.0`, `httpx>=0.27.0`, `pydantic>=2.6.4`).
  - Updated `docker/Dockerfile.ui` to install from `requirements-ui.txt` and `COPY data/ /app/data/` so `curated_products.json` is available for offline fallback.
- **Verification**: Verified dependency resolution via `python -m pip install --dry-run -r requirements-ui.txt` (exit code 0).

### Fix 8.7: Makefile Quality Gates & Cross-Platform Cleanup
- **Implementation**:
  - Updated `Makefile` `lint` target to run `flake8 ai/ apps/ core/ evaluation/ ml/ tests/ --max-line-length=127`, `black --check ai/ apps/ core/ evaluation/ ml/ tests/`, and `isort --check ai/ apps/ core/ evaluation/ ml/ tests/`.
  - Updated `clean` target to use a cross-platform Python one-liner removing `__pycache__`, `*.pyc`, `*.pyo`, `.pytest_cache`, `.coverage`, and `htmlcov`.
- **Verification**:
  - Ran `make clean` on Windows: passed with exit code 0.
  - Ran `flake8 core/config.py core/logging.py apps/reflex_app/rxconfig.py --max-line-length=127`: 0 errors.

---

## 3. Decisions Made
1. **Reflex `api_url` scope**: Kept `api_url` pointing to Reflex's backend (`REFLEX_API_URL`, default `http://localhost:8001`) rather than FastAPI (`API_BASE_URL`, port 8000), preventing frontend disconnection from Reflex websockets.
2. **.dockerignore granularity**: Explicitly excluded `data/*.csv` to avoid bloated multi-gigabyte Docker contexts while preserving `.json` files (`curated_products.json`, benchmarks, store policies) needed by application logic.
3. **Consolidated Validators**: Retained `mode="before"` for optional integer fields in `core/config.py` to handle empty strings `""` and string representations of None before core Pydantic integer parsing.

---

## 4. Noticed But Not Fixed (Out of Scope for WP1)
1. **Pre-existing test failures**:
   - `tests/test_phase3_langgraph_agent.py::test_search_agent_node_hybrid_execution` (cached response routes to END instead of synthesis node).
   - `tests/test_phase3_langgraph_agent.py::test_details_agent_node_retrieval` (cached response routes to END instead of synthesis node).
   *(Identified in `baseline_tests.txt`, deferred to AI work packages).*
2. **Pre-existing linting violations**: Untouched files across `apps/`, `ml/`, `tests/` contain pre-existing line-length, unused import, and formatting warnings; intentionally left untouched to prevent mass-reformat churn.
3. **Fix 1.4**: Database initialization SQL schema updates (`docker/postgres/init_schema.sql`) deferred to WP2 per prompt instructions.
