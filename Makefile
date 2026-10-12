.PHONY: lock help up down restart ps logs migrate ingest preprocess segmentation basket catalog train monitor retrain cron eval eval-ml eval-ai test test-unit test-slow test-integration test-e2e lint lint-ci clean

# Default target: display help
help:
	@echo "======================================================================"
	@echo "Black Friday v2 - Developer & MLOps Command Suite"
	@echo "======================================================================"
	@echo "  make up           - Start PostgreSQL, MinIO, MLflow, API & Reflex UI"
	@echo "  make down         - Stop all running Docker containers"
	@echo "  make restart      - Restart all services"
	@echo "  make ps           - View running container status"
	@echo "  make logs         - Stream container logs"
	@echo "  make migrate      - Apply the Alembic schema migrations (alembic upgrade head)"
	@echo "  make ingest       - Ingest raw train.csv into PostgreSQL warehouse"
	@echo "  make preprocess   - Run missForest imputation and outlier tagging"
	@echo "  make segmentation - Run Gower clustering and 10 persona profiling"
	@echo "  make lock         - Regenerate the hashed requirements*.lock files after editing requirements*.txt"
	@echo "  make basket       - Mine Apriori rules and compute PageRank/HITS"
	@echo "  make catalog      - Rebuild curated catalogue bundles/similars/order counts (after basket)"
	@echo "  make train        - Train regression benchmark models & export ONNX"
	@echo "                      Example: make train [MODEL=lgbm]"
	@echo "  make monitor      - Run Evidently AI drift monitor on incoming batch"
	@echo "                      Example: make monitor BATCH=path/to/batch.csv [MODEL=lgbm]"
	@echo "                      Example: make monitor DEMO=1 (test with warehouse split)"
	@echo "  make retrain      - Run Champion vs Challenger continuous retraining"
	@echo "                      Example: make retrain NEW_DATA=path/to/new.csv [MODEL=lgbm]"
	@echo "                      Example: make retrain DEMO=1 (test with warehouse split)"
	@echo "  make cron         - Start the 6-hour pipeline scheduler (ARGS=--once for one cycle)"
	@echo "  make eval         - Run regression and router evaluation suites"
	@echo "  make eval-ml      - Run standalone ML regression/imputation evaluation"
	@echo "  make eval-ai      - Run AI System-1 router and extractor benchmarks"
	@echo "  make test         - Run every test folder (unit, architecture, load helpers, integration, e2e)"
	@echo "  make test-unit    - Fast tests, mocks only (tests/unit, tests/architecture, tests/load; skips @slow)"
	@echo "  make test-slow    - The slow unit tests (subprocess imports, drift/retrain, ONNX imputer)"
	@echo "  make test-integration - Needs live PostgreSQL / Redis / MLflow (tests/integration)"
	@echo "  make test-e2e     - Full API / graph flows (tests/e2e)"
	@echo "  make lint         - Run flake8, black and isort on everything (all three always run)"
	@echo "  make lint-ci      - Same, limited to LINT_CLEAN_PATHS (what CI enforces)"
	@echo "  make clean        - Remove temporary cache files and __pycache__"
	@echo "======================================================================"

# Docker Container Orchestration
up:
	docker compose up -d --build

down:
	docker compose down

restart:
	docker compose restart

ps:
	docker compose ps

logs:
	docker compose logs -f

# Database schema (Alembic, core/db/migrations). New change: edit core/db/models, then
# alembic revision --autogenerate -m "..." and review the generated file before committing it.
# Exact, hashed dependency locks for Linux / Python 3.11 (the images and CI). Edit requirements*.txt, then run this.
# The dev lock is compiled first and constrains the API and ML locks, so tests run on the versions the API ships.
LOCK_FLAGS = --python-version 3.11 --python-platform x86_64-manylinux_2_28 --generate-hashes --no-header --quiet
lock:
	uv pip compile requirements-dev.txt $(LOCK_FLAGS) -o requirements-dev.lock
	uv pip compile requirements.txt -c requirements-dev.lock $(LOCK_FLAGS) -o requirements.lock
	uv pip compile requirements-ml.txt -c requirements-dev.lock $(LOCK_FLAGS) -o requirements-ml.lock
	uv pip compile requirements-ui.txt $(LOCK_FLAGS) -o requirements-ui.lock

migrate:
	alembic upgrade head

# Data & ML Pipelines (Local or Container Execution)
ingest:
	python -m ml.pipelines.ingest

preprocess:
	python -m ml.pipelines.preprocess

segmentation:
	python -m ml.pipelines.segmentation

basket:
	python -m ml.pipelines.market_basket

catalog:
	python -m ml.pipelines.build_curated_catalog $(ARGS)

train:
	python -m ml.pipelines.train $(if $(MODEL),--model $(MODEL),) $(ARGS)

monitor:
	python -m ml.pipelines.monitor $(if $(BATCH),--batch $(BATCH),) $(if $(REF),--reference $(REF),) $(if $(MODEL),--model $(MODEL),) $(if $(THRESHOLD),--threshold $(THRESHOLD),) $(if $(DEMO),--demo,) $(if $(NO_RETRAIN),--no-retrain,) $(ARGS)

retrain:
	python -m ml.pipelines.retrain $(if $(NEW_DATA),--new-data $(NEW_DATA),) $(if $(OLD_DATA),--old-data $(OLD_DATA),) $(if $(MODEL),--model $(MODEL),) $(if $(DEMO),--demo,) $(ARGS)

cron:
	python -m ml.pipelines.cron_scheduler $(ARGS)

# Model & System Evaluation Suites
eval-ml:
	python -m evaluation.ml.evaluate

eval-ai:
	python -m evaluation.ai.benchmark_router

eval: eval-ml eval-ai


# Quality Assurance & Code Standards
# Test layout: tests/unit (mocks only), tests/integration (live services), tests/e2e (full flows),
# tests/architecture (layering), tests/load (locust). Each test has a 60s timeout (pyproject.toml).
test: test-unit test-slow test-integration test-e2e

test-unit:
	pytest tests/unit tests/architecture tests/load -m "not slow" -v

test-slow:
	pytest tests/unit tests/load -m slow -v

test-integration:
	pytest tests/integration -v

test-e2e:
	pytest tests/e2e -v

# `make lint` checks LINT_PATHS and CI (`make lint-ci`) enforces LINT_CLEAN_PATHS. Every directory is
# lint-clean now, so the two are the same; keep it that way.
LINT_PATHS = ai/ apps/ core/ evaluation/ ml/ tests/
LINT_CLEAN_PATHS = $(LINT_PATHS)

lint:
	@rc=0; flake8 $(LINT_PATHS) || rc=1; black --check $(LINT_PATHS) || rc=1; isort --check $(LINT_PATHS) || rc=1; exit $$rc

lint-ci:
	@rc=0; flake8 $(LINT_CLEAN_PATHS) || rc=1; black --check $(LINT_CLEAN_PATHS) || rc=1; isort --check $(LINT_CLEAN_PATHS) || rc=1; exit $$rc

clean:
	python -c "import pathlib, shutil; [shutil.rmtree(p, ignore_errors=True) for p in pathlib.Path('.').rglob('__pycache__')]; [p.unlink() for p in pathlib.Path('.').rglob('*.pyc')]; [p.unlink() for p in pathlib.Path('.').rglob('*.pyo')]; shutil.rmtree('.pytest_cache', ignore_errors=True); shutil.rmtree('htmlcov', ignore_errors=True); pathlib.Path('.coverage').unlink(missing_ok=True)"
