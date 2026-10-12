# CLAUDE.md — Black Friday Sales Analysis & Production ML Platform

> Codebase guide for AI assistants working on this project.

## Project Overview

Enterprise-grade, end-to-end **Data Engineering + Data Science + ML Platform** built on Black Friday retail transactions (~550k records). The platform features a **FastAPI** backend serving predictions via **ONNX Runtime**, an interactive **Reflex** web UI, a **PostgreSQL 16** analytical warehouse, **MLflow** experiment tracking with **MinIO S3** storage, **Redis 7** caching, and an automated ML pipeline with drift monitoring and champion/challenger model promotion.

**Author:** Ziad Ashraf  
**Python Version:** 3.11  
**Current API Version:** 3.0.0

---

## Repository Structure

```
Black Friday/
├── core/              # Shared infrastructure (config, logging, security, DB, cache, tracking, embeddings)
│   ├── config.py      # Pydantic Settings — central configuration from .env
│   ├── logging.py     # Structured logging
│   ├── exceptions.py  # Domain errors (DomainError, RateLimitExceededError, ...)
│   ├── db/            # SQLAlchemy engine, session, models, migrations (semantic cache init), and domain repositories
│   │   ├── repository.py         # BlackFridayRepository — unified facade via composition
│   │   └── repositories/         # Domain repos: warehouse, analytics, segmentation, recommendation, user
│   ├── cache/         # RedisCacheManager — 6-hour TTL transparent caching
│   ├── tracking/      # MLflow integration (ONLY place mlflow is imported)
│   ├── security.py    # JWT creation/validation, bcrypt password hashing
│   └── embeddings/    # Vector embedding utilities
│
├── ml/                # Machine Learning domain
│   ├── features/      # Feature engineering, data_contract.py (Pandera schemas)
│   ├── models/        # Model registry, regression models, ONNX exporter, metrics computation
│   ├── tracking/      # MLflowTracker, drift monitor, SHAP explainability, model cards (go through core.tracking)
│   ├── market_basket/ # Apriori association mining, Item2Vec embeddings, product network centrality
│   ├── segmentation/  # Gower dissimilarity clustering, 10 customer personas
│   ├── pipelines/     # Pipeline entrypoints: ingest, preprocess, train, monitor, retrain, segmentation, market_basket, seed_curated, cron_scheduler
│   └── serving/       # ONNX runtime inference engines
│
├── ai/                # AI / LLM domain (shopping assistant)
│   ├── classifier, extractor, guardrails, nodes, observability, prompts, router, schemas, services, tools, workflow
│   └── (System-1 guardrail + LangGraph workflow)
│
├── evaluation/        # Standalone evaluation suites (never imported by other modules)
│   ├── ml/            # Regression & imputer evaluation
│   └── ai/            # Router benchmark evaluation
│
├── apps/              # Application layer
│   ├── api/           # FastAPI backend
│   │   ├── main.py              # App entry point with lifespan hooks
│   │   ├── dependencies.py      # DI providers
│   │   ├── auth.py              # JWT auth dependencies (get_current_user, get_optional_user)
│   │   ├── client_identity.py   # Client IP and per-guest identity (HMAC of peer IP + session id)
│   │   ├── routes/              # Route modules: analytics, auth, shopper, bot
│   │   ├── services/            # Business logic: analytics_service, auth_service, model_service, shopper_service
│   │   ├── schemas.py           # Pydantic request/response models
│   │   ├── handlers.py          # Global exception handlers
│   │   ├── middleware/          # SecurityBanMiddleware
│   │   └── rate_limiting/       # Rate limiter dependency
│   └── reflex_app/    # Reflex interactive frontend (consumes FastAPI via HTTP only)
│
├── tests/             # Pytest suite, split by what each test needs (markers: unit, integration, e2e, slow)
│   ├── conftest.py    # Shared setup (forces Redis DB 15); integration/ and e2e/ add per-test Redis/DB cleanup
│   ├── unit/          # Fast, mocks only, no network: core/, ml/, ai/, api/, ui/
│   ├── integration/   # Needs live PostgreSQL / Redis / MLflow (some also need the running API container)
│   ├── e2e/           # Full API / LangGraph flows (real LLM calls, slow)
│   ├── architecture/  # AST layering enforcement (test_architecture.py, test_evaluation_layering.py)
│   └── load/          # Locust load test (locustfile.py) and its auth helper
├── docker/            # Dockerfiles & init scripts
│   ├── Dockerfile.api
│   ├── Dockerfile.ui
│   ├── mlflow/Dockerfile
│   └── postgres/      # DB init scripts (init-multiple-dbs.sh, init_schema.sql)
│
├── docs/images/       # Architecture diagrams, SHAP plots, UI screenshots
├── data/              # Raw CSV data (gitignored)
├── models/            # Trained model artifacts & ONNX exports (gitignored)
├── reports/           # Locust CSV reports (gitignored)
├── .github/workflows/ # CI/CD pipeline (ci-cd.yml)
├── docker-compose.yml # Full stack orchestration (6 services + a one-shot minio-init bucket job)
├── Makefile           # Developer & MLOps command suite
├── pyproject.toml     # black / isort / pytest config (markers, 60s timeout)
├── .flake8            # flake8 config (line length 127)
├── requirements.txt   # API runtime only (Docker image); requirements-ml.txt adds pipelines, requirements-ui.txt is the UI image, requirements-dev.txt adds tests/lint
└── .env               # Environment variables (gitignored)
```

---

## Strict Architectural Layering Rules

These rules are **enforced by AST analysis** in `tests/architecture/test_architecture.py`. All changes MUST comply:

| Layer | May Import | NEVER Imports |
|:------|:-----------|:--------------|
| `core/` | stdlib, third-party only | `ml/`, `ai/`, `apps/`, `evaluation/` |
| `ml/` | `core/` | `apps/`, `ai/`, `evaluation/` |
| `ai/` | `core/` | `apps/`, `ml/`, `evaluation/` |
| `apps/` | `core/`, `ml/`, `ai/` | `evaluation/` |
| `evaluation/` | `core/`, `ml/`, `ai/` | (nothing imports `evaluation/`) |

**MLflow Isolation:** `mlflow` is imported **strictly within `core/tracking/`**. All other modules interact with MLflow through `core.tracking` abstractions.

---

## Key Technologies & Dependencies

| Domain | Stack |
|:-------|:------|
| **Backend API** | FastAPI 0.110+, Uvicorn, Pydantic v2, python-jose (JWT), bcrypt |
| **ML/DS** | scikit-learn, LightGBM, ONNX Runtime, SHAP, Pandera, MissForest |
| **Market Basket** | mlxtend (Apriori), gensim (Item2Vec/Word2Vec), networkx (PageRank/HITS) |
| **Segmentation** | gower, scipy (hierarchical clustering) |
| **Drift Monitoring** | Evidently AI |
| **Experiment Tracking** | MLflow `>=2.20,<3` (server image v2.20.0; the API image uses `mlflow-skinny`) with MinIO S3 artifact store |
| **Database** | PostgreSQL 16 (pgvector), SQLAlchemy 2.0, asyncpg, psycopg2 |
| **Cache** | Redis 7 (6-hour TTL, AOF persistence) |
| **Frontend** | Reflex 0.10.0 (pinned; Python-only reactive UI) |
| **AI/LLM** | google-genai (Gemini), LangGraph, langchain-core |
| **Testing** | pytest, pytest-asyncio, Locust (load testing) |
| **Code Quality** | flake8, black (`>=25.1,<26`), isort; config in `pyproject.toml` and `.flake8` |
| **Containerization** | Docker Compose (postgres, minio + minio-init, mlflow, redis, model-api, reflex-ui); minio and mc are pinned to `RELEASE.*` tags |
| **CI/CD** | GitHub Actions |

---

## Essential Commands

### Development

```bash
# Start FastAPI backend server
python -m uvicorn apps.api.main:app --host 127.0.0.1 --port 8000 --reload

# Start Reflex frontend (from project root)
cd apps/reflex_app && reflex run

# Docker: bring up all services
make up                   # docker-compose up -d --build
make down                 # stop all containers
make ps                   # container status
make logs                 # stream logs
```

### ML Pipelines

```bash
make ingest               # Ingest raw train.csv → PostgreSQL (COPY streaming, ~7s for 550k rows)
make preprocess           # MissForest imputation + train/test split (zero-leakage)
make segmentation         # Gower clustering → 10 customer personas
make basket               # Apriori rules + Item2Vec embeddings + PageRank/HITS
make train                # Train regression benchmark + ONNX export
make train MODEL=lgbm     # Train specific model
make monitor DEMO=1       # Evidently AI drift monitoring
make retrain DEMO=1       # Champion vs Challenger retraining
make cron                 # Start 6-hour automated pipeline scheduler
```

### Evaluation & Testing

```bash
make eval                 # Run full evaluation (ML + AI)
make eval-ml              # Standalone ML regression/imputer evaluation
make eval-ai              # AI System-1 router benchmarks
make test                 # unit + architecture + integration + e2e
make test-unit            # fast, mocks only: tests/unit, tests/architecture, tests/load (skips @slow)
make test-slow            # the slow unit tests; CI runs them together with test-unit
make test-integration     # needs live PostgreSQL / Redis / MLflow
make test-e2e             # full API / graph flows
make lint                 # flake8 + black --check + isort --check
make clean                # Remove __pycache__, .pytest_cache, etc.
```

### Load Testing

```bash
# Interactive (web UI at localhost:8089)
locust -f tests/load/locustfile.py --host http://127.0.0.1:8000

# Headless (50 users, 10 spawn rate, 15s duration)
locust -f tests/load/locustfile.py --headless -u 50 -r 10 --run-time 15s --host http://127.0.0.1:8000 --csv=reports/locust_summary
```

---

## Configuration

All configuration is managed through **Pydantic Settings** in `core/config.py`, loaded from `.env` file with sane defaults. Key groups:

- **Database:** `POSTGRES_USER`, `POSTGRES_PASSWORD`, `POSTGRES_HOST`, `POSTGRES_PORT`, `APP_DB_NAME`
- **Redis:** `REDIS_HOST`, `REDIS_PORT`, `REDIS_DEFAULT_TTL` (21600s = 6 hours)
- **MLflow:** `MLFLOW_TRACKING_URI`, `MLFLOW_EXPERIMENT_NAME`, `MLFLOW_MODEL_NAME`
- **MinIO/S3:** `MINIO_ROOT_USER`, `MINIO_ROOT_PASSWORD`, `MINIO_PORT`
- **JWT Auth:** `SECRET_KEY`, `JWT_ALGORITHM` (HS256), `JWT_EXPIRY_HOURS` (24), `JWT_ISSUER`, `JWT_AUDIENCE`
- **ML Hyperparams:** `LGBM_*`, `RF_*`, `DT_*`, `IMPUTER_*`, `K_CLUSTERS`, `APRIORI_*`, `ITEM2VEC_*`
- **Drift Thresholds:** `DRIFT_DATASET_THRESHOLD` (0.30), `DRIFT_TARGET_PSI_THRESHOLD` (0.20)
- **LLM:** `GEMINI_API_KEY`, `LLM_MODEL` (default `gemini-3-flash-preview`)
- **Rate limits:** `RATE_LIMIT_MINUTE` (60), `RATE_LIMIT_DAY` (1000), `AUTH_RATE_LIMIT_MINUTE` (10), `AUTH_RATE_LIMIT_DAY` (200)
- **Safety:** when `ENVIRONMENT` is not a dev environment, startup fails if `SECRET_KEY`, `POSTGRES_PASSWORD` or `MINIO_ROOT_PASSWORD` still has its default

---

## API Endpoints

| Route Prefix | Module | Description |
|:-------------|:-------|:------------|
| `GET /health` | `main.py` | Health check with model status |
| `/auth/*` | `routes/auth.py` | JWT signup, login, bcrypt verification |
| `/shopper/*` | `routes/shopper.py` | Catalog, ONNX price predictions, basket, purchases, history |
| `/analytics/*` | `routes/analytics.py` | Executive KPI summaries, demographic distributions |
| `/bot/*` | `routes/bot.py` | AI shopping assistant (HTTP and the `/bot/live-ws` WebSocket) |

---

## Database Design

The `BlackFridayRepository` (in `core/db/repository.py`) is a **facade** composing 5 domain repositories:

1. **WarehouseRepository** — Raw & cleaned transaction data, bulk COPY loading
2. **AnalyticsRepository** — EDA summary & demographic distributions  
3. **SegmentationRepository** — Customer personas & cluster metrics
4. **RecommendationRepository** — PageRank, HITS, bundle rules, categories
5. **UserRepository** — User registration, authentication & purchase history

All share the same SQLAlchemy engine via dependency injection.

---

## ML Model Architecture

- **Champion Model:** LightGBM Regressor (Test R² = 72.84%, RMSE = 0.1207)
- **Serving:** ONNX Runtime C++ inference (<1.3ms latency, >750 req/s)
- **Features:** 10 domain features (gender, age, occupation, city_category, stay_in_current_city_years, marital_status, product_category_1..3, product_id)
- **Imputation:** MissForest with tree-constrained ONNX compression (~4.5 MB artifacts, 99.1% size reduction)
- **Explainability:** SHAP beeswarm plots logged to MLflow

---

## Coding Conventions

- **Max line length:** 127 characters (flake8)
- **Formatter:** black
- **Import sorting:** isort
- **Type hints:** Pydantic v2 models for all API schemas; typing annotations throughout
- **Logging:** Use `core.logging.get_logger(__name__)` — never use `print()`
- **Config access:** Always use `core.config.settings` singleton — never hardcode values
- **DB access:** Always go through `BlackFridayRepository` — never create raw SQLAlchemy sessions directly in routes
- **MLflow:** Only import in `core/tracking/` — use abstractions elsewhere

---

## Docker Services (docker-compose.yml)

| Service | Container | Port | Image |
|:--------|:----------|:-----|:------|
| PostgreSQL 16 | `blackfriday_postgres` | 5432 | `pgvector/pgvector:pg16` |
| MinIO S3 | `blackfriday_minio` | 9000, 9001 | `quay.io/minio/minio` pinned `RELEASE.*` tag |
| MLflow | `blackfriday_mlflow` | 5000 | Custom Dockerfile |
| Redis 7 | `blackfriday_redis` | 6379 | `redis:7-alpine` |
| FastAPI | `blackfriday_api` | 8000 | `docker/Dockerfile.api` |
| Reflex UI | `blackfriday_ui` | 3000 (-> container 8001), 8001 | `docker/Dockerfile.ui` |
| MinIO bucket init | (one-shot `minio-init`) | none | `quay.io/minio/mc` pinned `RELEASE.*` tag |

---

## CI/CD Pipeline

GitHub Actions workflow (`.github/workflows/ci-cd.yml`):
- Triggers on push/PR to `main`/`master`
- Installs Python 3.11 + `requirements-dev.txt`
- Runs `make lint-ci` (flake8, black, isort on `LINT_CLEAN_PATHS`, which equals `LINT_PATHS`: the whole repo is lint-clean), then the unit tests (`tests/unit`, `tests/load`, 60s timeout) and the architecture tests; integration and e2e run locally

---

## Common Pitfalls

1. **Architecture violations** — `tests/architecture/test_architecture.py` uses AST parsing to enforce layering. Any forbidden cross-layer import will fail CI.
2. **MLflow imports** — Only allowed in `core/tracking/`. Importing `mlflow` anywhere else breaks the architecture test.
3. **Data leakage** — Preprocessing splits train/test BEFORE fitting any transformers. Never fit on the full dataset.
4. **Redis cache invalidation** — The cron scheduler flushes `shopper:*`, `analytics:*`, `price:*` patterns every 6 hours. Manual cache clearing may be needed during development.
5. **CORS** — Explicit origins are configured (no wildcard `*`) for credentialed cookie/JWT access. Add new origins to `CORS_ORIGINS` env var or the defaults in `main.py`.

---

## Code Review Status

- Round 1 review is complete (merged from `review/round-1`). Summary, before/after metrics and round-2 items are in `docs/REVIEW_LOG.md`; the runbook is `docs/REVIEW_PLAN.md`.
- Round 2 runbook: `docs/REVIEW_PLAN_ROUND2.md` (branch `review/round-2`, not created yet; the first session creates it). Same loop: one area per session, one commit per finding, log every finding. Ask before replacing any model or personas (step 6).
