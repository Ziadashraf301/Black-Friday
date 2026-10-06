.PHONY: help up down restart ps logs ingest preprocess segmentation basket train monitor retrain eval eval-ml eval-ai test lint clean

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
	@echo "  make ingest       - Ingest raw train.csv into PostgreSQL warehouse"
	@echo "  make preprocess   - Run missForest imputation and outlier tagging"
	@echo "  make segmentation - Run Gower clustering and 10 persona profiling"
	@echo "  make basket       - Mine Apriori rules and compute PageRank/HITS"
	@echo "  make train        - Train regression benchmark models & export ONNX"
	@echo "                      Example: make train [MODEL=lgbm]"
	@echo "  make monitor      - Run Evidently AI drift monitor on incoming batch"
	@echo "                      Example: make monitor BATCH=path/to/batch.csv [MODEL=lgbm]"
	@echo "                      Example: make monitor DEMO=1 (test with warehouse split)"
	@echo "  make retrain      - Run Champion vs Challenger continuous retraining"
	@echo "                      Example: make retrain NEW_DATA=path/to/new.csv [MODEL=lgbm]"
	@echo "                      Example: make retrain DEMO=1 (test with warehouse split)"
	@echo "  make eval         - Run regression and router evaluation suites"
	@echo "  make eval-ml      - Run standalone ML regression/imputation evaluation"
	@echo "  make eval-ai      - Run AI System-1 router and extractor benchmarks"
	@echo "  make test         - Run pytest suite with coverage"
	@echo "  make lint         - Run flake8 and black code quality checks"
	@echo "  make clean        - Remove temporary cache files and __pycache__"
	@echo "======================================================================"

# Docker Container Orchestration
up:
	docker-compose up -d --build

down:
	docker-compose down

restart:
	docker-compose restart

ps:
	docker-compose ps

logs:
	docker-compose logs -f

# Data & ML Pipelines (Local or Container Execution)
ingest:
	python -m ml.pipelines.ingest

preprocess:
	python -m ml.pipelines.preprocess

segmentation:
	python -m ml.pipelines.segmentation

basket:
	python -m ml.pipelines.market_basket

train:
	python -m ml.pipelines.train $(if $(MODEL),--model $(MODEL),) $(ARGS)

monitor:
	python -m ml.pipelines.monitor $(if $(BATCH),--batch $(BATCH),) $(if $(REF),--reference $(REF),) $(if $(MODEL),--model $(MODEL),) $(if $(THRESHOLD),--threshold $(THRESHOLD),) $(if $(DEMO),--demo,) $(if $(NO_RETRAIN),--no-retrain,) $(ARGS)

retrain:
	python -m ml.pipelines.retrain $(if $(NEW_DATA),--new-data $(NEW_DATA),) $(if $(OLD_DATA),--old-data $(OLD_DATA),) $(if $(MODEL),--model $(MODEL),) $(if $(DEMO),--demo,) $(ARGS)

cron:
	python -m ml.pipelines.cron_scheduler

# Model & System Evaluation Suites
eval-ml:
	python -m evaluation.ml.evaluate

eval-ai:
	python -m evaluation.ai.benchmark_router

eval: eval-ml eval-ai


# Quality Assurance & Code Standards
test:
	pytest tests/ -v

lint:
	flake8 apps/ ml/ core/ evaluation/ tests/ --max-line-length=127
	black --check apps/ ml/ core/ evaluation/ tests/

clean:
	find . -type d -name "__pycache__" -exec rm -rf {} +
	find . -type f -name "*.pyc" -delete
	rm -rf .pytest_cache .coverage
