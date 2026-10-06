# Code Review Report: Session 6 — Tests, Docker & CI/CD

> **Target Domain**: Tests (`tests/`), Docker Containerization (`docker/`, `docker-compose.yml`), CI/CD Pipelines (`.github/workflows/`), and Build Automation (`Makefile`).  
> **Audience**: Engineering Team & Lead Architect.  
> **Report Output**: `REVIEW_devops.md` in repository root.  
> **Security Notice**: Zero secrets, `.env` files, or production credentials were read, quoted, or included.

---

## 1. Executive Summary & Verification of Handoff Hypotheses

During this audit, every finding from [REVIEW_HANDOFF.md](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/REVIEW_HANDOFF.md) touching `tests/`, `docker/`, `.github/`, and `Makefile` was treated as an unverified hypothesis and verified against the actual repository files. 

### Hypothesis Verification Matrix

| Handoff Section | Hypothesized Finding | Status | Real Code Evidence & Notes |
| :--- | :--- | :--- | :--- |
| **Section 5.4 Item 2** | `requirements.txt` is missing `langgraph`, `langchain-core`, and `loguru`. | **REJECTED** | [requirements.txt:72-74](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/requirements.txt#L72-L74) contains `langgraph>=1.2.11`, `langchain-core>=1.6.2`, and `loguru>=0.7.3`. |
| **Section 5.4 Item 3 & Sec 7** | `docker-compose.yml` mounts non-existent `./src` & sets `PYTHONPATH: /app/src`; `Dockerfile.api` copies `./config` and omits `ai/`. | **REJECTED** | [docker-compose.yml:151-158](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/docker-compose.yml#L151-L158) sets `PYTHONPATH: /app` and mounts `./ai`, `./apps`, `./core`, `./ml`, `./models`. [docker/Dockerfile.api:19-23](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/docker/Dockerfile.api#L19-L23) copies `core/`, `apps/`, `ml/`, `models/`, and `ai/` (no `./config` line exists). |
| **Section 5.4 Item 4 & Sec 7** | `apps/reflex_app/reflex_app/state.py:13` hardcodes `API_BASE_URL = "http://127.0.0.1:8000"`, breaking Docker container networking. | **REJECTED** | [apps/reflex_app/reflex_app/state.py:15](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/apps/reflex_app/reflex_app/state.py#L15) implements `API_BASE_URL = os.getenv("API_BASE_URL", "http://127.0.0.1:8000")`. |
| **Section 7** | `Makefile:77` `make lint` omits `ai/` directory from flake8 and black. | **CONFIRMED** | [Makefile:77-78](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/Makefile#L77-L78) runs `flake8 apps/ ml/ core/ tests/` and `black --check apps/ ml/ core/ tests/`, completely ignoring `ai/`. |
| **Section 7** | `Makefile:81-83` `make clean` uses Unix `find` and `rm -rf`, failing on Windows PowerShell/cmd. | **CONFIRMED** | [Makefile:81-83](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/Makefile#L81-L83) uses GNU `find` and `rm -rf`, which fail without Bash/WSL. |

---

## 2. Detailed Findings

---

### [docker/Dockerfile.api:19-23](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/docker/Dockerfile.api#L19-L23) & [docker-compose.yml:153-157](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/docker-compose.yml#L153-L157)
- **Severity**: High
- **Status**: **NEW**
- **Problem**: The `data/` directory is completely omitted from both `Dockerfile.api` build copying and `docker-compose.yml` container volumes. Inside the running API container, `/app/data/` does not exist. However, [ai/tools/policy_kb.py:19](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/ai/tools/policy_kb.py#L19) relies on `data/store_policies.json` and [ai/extractor/catalog_index.py:26](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/ai/extractor/catalog_index.py#L26) resolves `data/curated_products.json`. In production container deployments, policy lookups silently degrade to fallbacks and catalog entity extraction fails.
- **Fix**: Copy `data/` in `Dockerfile.api` and mount it as a volume in `docker-compose.yml`.

```dockerfile
# docker/Dockerfile.api
COPY core/ /app/core/
COPY apps/ /app/apps/
COPY ml/ /app/ml/
COPY models/ /app/models/
COPY ai/ /app/ai/
COPY data/ /app/data/
```

```yaml
# docker-compose.yml
    volumes:
      - ./ai:/app/ai
      - ./apps:/app/apps
      - ./core:/app/core
      - ./ml:/app/ml
      - ./models:/app/models
      - ./data:/app/data
```

---

### [.github/workflows/ci-cd.yml:10-34](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/.github/workflows/ci-cd.yml#L10-L34)
- **Severity**: High
- **Status**: **NEW**
- **Problem**: The CI pipeline executes `python -m pytest tests/ -v` on bare `ubuntu-latest` without provisioning PostgreSQL or Redis service containers. Several test suites ([tests/test_phase1_infra_frontend.py:41](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/tests/test_phase1_infra_frontend.py#L41), [tests/test_phase4_bundles.py:28](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/tests/test_phase4_bundles.py#L28), [tests/test_phase4_cart_durability.py:26](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/tests/test_phase4_cart_durability.py#L26), [tests/test_phase4_e2e_production.py:45](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/tests/test_phase4_e2e_production.py#L45)) execute real database and cache queries (`repo.engine.connect()`, `BlackFridayRepository()`, `RedisCacheManager()`). In GitHub Actions, these integration tests fail due to connection refused errors.
- **Fix**: Define `postgres` (with `pgvector`) and `redis` service containers in the GitHub Actions workflow, or mark live-service tests with `@pytest.mark.integration` and exclude them with `-m "not integration"` in CI.

```yaml
# .github/workflows/ci-cd.yml
jobs:
  test:
    runs-on: ubuntu-latest
    services:
      postgres:
        image: pgvector/pgvector:pg16
        env:
          POSTGRES_USER: test_user
          POSTGRES_PASSWORD: test_password
          POSTGRES_DB: fridayblack
        ports:
          - 5432:5432
        options: >-
          --health-cmd pg_isready
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5
      redis:
        image: redis:7-alpine
        ports:
          - 6379:6379
        options: >-
          --health-cmd "redis-cli ping"
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-python@v5
        with:
          python-version: "3.11"
          cache: "pip"
      - run: |
          pip install -r requirements.txt
      - run: |
          python -m pytest tests/ -v
        env:
          POSTGRES_HOST: localhost
          POSTGRES_PORT: 5432
          REDIS_HOST: localhost
          REDIS_PORT: 6379
```

---

### [tests/test_api_client.py:10-25](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/tests/test_api_client.py#L10-L25)
- **Severity**: Medium
- **Status**: **NEW**
- **Problem**: Sham unit test with zero method execution. `test_analytics_fetch_in_reflex` applies `@patch("httpx.Client.get")` and mocks response JSON, but never calls `state.load_dashboard()`. It merely checks default initialized attributes (`is_authenticated is False`, `welcome_name == "Shopper"`). The mock is unused and the actual HTTP analytics fetch logic in Reflex state is completely unexercised.
- **Fix**: Invoke `state.load_dashboard()` and verify that `mock_get` was called with the expected endpoint and that `state.dashboard_summary` was populated.

```python
# tests/test_api_client.py
@patch("httpx.Client.get")
def test_analytics_fetch_in_reflex(mock_get):
    mock_resp = MagicMock()
    mock_resp.status_code = 200
    mock_resp.json.return_value = {
        "total_orders": 550068,
        "total_revenue": 5095812740.0,
        "avg_order_value": 9264.12,
        "user_count": 5891
    }
    mock_get.return_value = mock_resp

    state = ShoppingState()
    state.load_dashboard()

    assert mock_get.called
    assert state.dashboard_summary["total_orders"] == 550068
```

---

### [tests/test_api_client.py:6-7](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/tests/test_api_client.py#L6-L7)
- **Severity**: Medium
- **Status**: **NEW**
- **Problem**: Brittle test anti-pattern. `test_api_base_url` asserts `assert API_BASE_URL == "http://127.0.0.1:8000"`. If pytest runs inside a Docker container or in an environment where `API_BASE_URL` is set to `http://model-api:8000`, this test immediately fails. The test penalizes the exact dynamic configuration that `os.getenv` was added to support.
- **Fix**: Use `monkeypatch` to verify default behavior and environment override behavior dynamically.

```python
# tests/test_api_client.py
import importlib
import apps.reflex_app.reflex_app.state as state_module

def test_api_base_url_env_override(monkeypatch):
    monkeypatch.setenv("API_BASE_URL", "http://custom-api:8000")
    importlib.reload(state_module)
    assert state_module.API_BASE_URL == "http://custom-api:8000"
```

---

### [docker/postgres/init_schema.sql:1-133](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/docker/postgres/init_schema.sql#L1-L133)
- **Severity**: Medium
- **Status**: **NEW**
- **Problem**: Incomplete database schema initialization. `init_schema.sql` only creates warehouse and curated tables (`raw_black_friday`, `black_friday_cleaned`, `customer_segments`, `product_network_metrics`, `curated_products`). Application tables (`app_users`, `user_purchases`, `user_carts`, `semantic_query_cache`) are completely absent. Instead, they are created ad-hoc via SQL strings executed during API startup ([apps/api/main.py:26](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/apps/api/main.py#L26)), on-demand repository queries ([core/db/repositories/warehouse_repo.py:465](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/core/db/repositories/warehouse_repo.py#L465)), or manual scripts ([core/db/migrations/init_semantic_cache.py](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/core/db/migrations/init_semantic_cache.py)). This creates race conditions when multiple API workers boot concurrently.
- **Fix**: Consolidate DDL for `app_users`, `user_purchases`, `user_carts`, and `semantic_query_cache` into `docker/postgres/init_schema.sql`.

```sql
-- Append to docker/postgres/init_schema.sql:
CREATE TABLE IF NOT EXISTS app_users (
    user_id SERIAL PRIMARY KEY,
    name TEXT NOT NULL,
    email TEXT UNIQUE NOT NULL,
    password_hash TEXT NOT NULL,
    gender VARCHAR(1),
    age VARCHAR(10),
    city_category VARCHAR(1),
    marital_status INTEGER,
    occupation INTEGER,
    stay_in_current_city_years VARCHAR(5),
    cluster_id INTEGER,
    cluster_persona TEXT,
    recommended_action TEXT,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS user_purchases (
    id SERIAL PRIMARY KEY,
    user_id INTEGER REFERENCES app_users(user_id) ON DELETE CASCADE,
    product_id TEXT NOT NULL,
    product_category_1 INTEGER,
    product_category_2 INTEGER,
    product_category_3 INTEGER,
    predicted_usd FLOAT,
    model_used TEXT,
    purchased_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS user_carts (
    user_id VARCHAR(64) PRIMARY KEY,
    session_id VARCHAR(64),
    cart_data JSONB NOT NULL,
    item_count INTEGER DEFAULT 0,
    total_amount NUMERIC(10, 2) DEFAULT 0.0,
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS semantic_query_cache (
    id BIGSERIAL PRIMARY KEY,
    query_text TEXT NOT NULL,
    embedding vector(768) NOT NULL,
    response_text TEXT NOT NULL,
    ui_payload JSONB NOT NULL,
    intent VARCHAR(64) NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW()
);
CREATE INDEX IF NOT EXISTS idx_semantic_cache_hnsw ON semantic_query_cache USING hnsw (embedding vector_cosine_ops);
```

---

### [tests/test_phase2_jev_router.py:279-281](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/tests/test_phase2_jev_router.py#L279-L281)
- **Severity**: Medium
- **Status**: **NEW**
- **Problem**: Hardcoded sub-millisecond latency assertions in standard unit tests. `test_router_latency_under_10ms` asserts `assert p50 < 3.0` and `assert p95 < 10.0` over 35 loop iterations. On shared virtual machines (such as GitHub Actions runners or congested developer laptops), CPU frequency scaling, GC pauses, or process preemption will cause this test to fail intermittently, leading to non-deterministic pipeline failures.
- **Fix**: Annotate with `@pytest.mark.benchmark` so it can be skipped during normal CI runs, or provide a generous threshold suitable for virtualized CI runners.

```python
# tests/test_phase2_jev_router.py
@pytest.mark.benchmark
def test_router_latency_under_10ms():
    # Warm-up run
    router = FastRuleRouter()
    for _ in range(5):
        router.route("Warmup query")
    ...
    # Relaxed tolerance for noisy CI runners
    assert p95 < 25.0, f"p95 latency {p95:.2f}ms exceeds CI threshold"
```

---

### [docker/Dockerfile.ui:15](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/docker/Dockerfile.ui#L15)
- **Severity**: Medium
- **Status**: **NEW**
- **Problem**: Severe container image bloat and slow build times. `Dockerfile.ui` executes `uv pip install --system --no-cache -r requirements.txt`. The full `requirements.txt` installs massive machine learning frameworks (`lightgbm`, `scikit-learn`, `onnx`, `onnxruntime`, `gensim`, `shap`, `evidently`, `gower`, `mlxtend`, `networkx`, `pgvector`). The frontend container only needs `reflex`, `httpx`, `requests`, and `pydantic`. This adds over 2 GB of unnecessary disk bloat and minutes of installation time to the UI image.
- **Fix**: Extract a lightweight `requirements-ui.txt` and install only that in `Dockerfile.ui`.

```dockerfile
# docker/Dockerfile.ui
COPY requirements-ui.txt ./
RUN uv pip install --system --no-cache -r requirements-ui.txt
```

---

### [docker/postgres/init_schema.sql:6](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/docker/postgres/init_schema.sql#L6)
- **Severity**: Low
- **Status**: **NEW**
- **Problem**: Hardcoded database name. Line 6 executes `\connect fridayblack;`. In [docker-compose.yml:14](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/docker-compose.yml#L14), `POSTGRES_DB: ${APP_DB_NAME}` allows the application database name to be configured via environment variables. If a user customizes `APP_DB_NAME` to anything other than `fridayblack`, `init_schema.sql` fails with `FATAL: database "fridayblack" does not exist`.
- **Fix**: Remove `\connect fridayblack;` from `init_schema.sql`. The database connection is already determined by `init-multiple-dbs.sh` or the container connection target.

```sql
-- docker/postgres/init_schema.sql
-- Remove line 6: \connect fridayblack;
CREATE EXTENSION IF NOT EXISTS vector;
```

---

### [requirements.txt:63-74](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/requirements.txt#L63-L74)
- **Severity**: Low
- **Status**: **NEW**
- **Problem**: Missing `locust` dependency. [tests/locustfile.py:2](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/tests/locustfile.py#L2) imports `from locust import HttpUser, task, between`. However, `locust` is omitted from `requirements.txt`. Developers running load tests encounter an immediate `ModuleNotFoundError: No module named 'locust'`.
- **Fix**: Add `locust` to `requirements.txt`.

```text
# Testing & Quality
pytest>=8.1.0
pytest-asyncio>=0.23.5
pytest-cov>=4.1.0
locust>=2.24.0
```

---

### [tests/test_features.py:67-68](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/tests/test_features.py#L67-L68)
- **Severity**: Low
- **Status**: **NEW**
- **Problem**: Catching generic `Exception` anti-pattern. `test_cleaned_data_contract_with_split` uses `with pytest.raises(Exception): validate_cleaned_data(cleaned_invalid)`. If an unexpected bug occurs (e.g. `AttributeError` or `KeyError`), the test falsely passes instead of asserting Pandera schema invalidation.
- **Fix**: Assert the specific Pandera validation exception `pandera.errors.SchemaError`.

```python
# tests/test_features.py
from pandera.errors import SchemaError

def test_cleaned_data_contract_with_split(sample_transactions):
    ...
    with pytest.raises(SchemaError):
        validate_cleaned_data(cleaned_invalid)
```

---

### [Makefile:77-78](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/Makefile#L77-L78)
- **Severity**: Low
- **Status**: **CONFIRMED**
- **Problem**: Omission of `ai/` from code quality targets. `make lint` executes `flake8 apps/ ml/ core/ tests/` and `black --check apps/ ml/ core/ tests/`. The multi-agent assistant package `ai/` (~4,200 LOC) is excluded from automated lint checks.
- **Fix**: Include `ai/` in both commands.

```makefile
# Makefile
lint:
	flake8 apps/ ml/ core/ tests/ ai/ --max-line-length=127
	black --check apps/ ml/ core/ tests/ ai/
```

---

### [Makefile:81-83](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/Makefile#L81-L83)
- **Severity**: Low
- **Status**: **CONFIRMED**
- **Problem**: Platform-incompatible clean commands. `make clean` uses GNU `find` and `rm -rf`, which fail on native Windows cmd/PowerShell.
- **Fix**: Implement cross-platform cleanup using Python one-liners.

```makefile
# Makefile
clean:
	python -c "import pathlib, shutil; [shutil.rmtree(p) for p in pathlib.Path('.').rglob('__pycache__')]; [p.unlink() for p in pathlib.Path('.').rglob('*.pyc')]; [shutil.rmtree(p, ignore_errors=True) for p in ('.pytest_cache', '.coverage')]"
```

---

### [docker/Dockerfile.api:27](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/docker/Dockerfile.api#L27) & [docker/Dockerfile.ui:26](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/docker/Dockerfile.ui#L26)
- **Severity**: Low
- **Status**: **NEW**
- **Problem**: Container processes run as `root`. Neither Dockerfile creates or switches to an unprivileged non-root user. Running container workloads as root increases attack surface in container breakout scenarios.
- **Fix**: Add a dedicated system user before the entrypoint.

```dockerfile
# In both Dockerfile.api and Dockerfile.ui:
RUN useradd -m -u 1000 appuser && chown -R appuser:appuser /app
USER appuser
```

---

### [docker/Dockerfile.api:25-27](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/docker/Dockerfile.api#L25-L27)
- **Severity**: Low
- **Status**: **NEW**
- **Problem**: Missing Docker `HEALTHCHECK` directive in `Dockerfile.api`. In `docker-compose.yml:171`, `reflex-ui` depends on `model-api`, but without a container healthcheck, Docker only waits for container instantiation, not for Uvicorn readiness.
- **Fix**: Add a healthcheck probing `/health`.

```dockerfile
# docker/Dockerfile.api
HEALTHCHECK --interval=10s --timeout=5s --retries=3 \
    CMD curl --fail http://localhost:8000/health || exit 1
```

---

## 3. Top 5 Refactors Ranked by Impact vs. Effort

| Rank | Refactor Description | Domain | Impact | Effort | Rationale |
| :---: | :--- | :--- | :---: | :---: | :--- |
| **1** | **Add `COPY data/ /app/data/` to `Dockerfile.api` & `docker-compose.yml`** | Docker | **Critical** | **Very Low** | Directly resolves missing policy and catalog files in containerized bot execution. |
| **2** | **Add Postgres (`pgvector`) & Redis Services to `.github/workflows/ci-cd.yml`** | CI/CD | **High** | **Low** | Prevents integration test failures in GitHub Actions without modifying test suites. |
| **3** | **Consolidate Missing Application Tables into `docker/postgres/init_schema.sql`** | Docker / DB | **High** | **Low** | Eliminates scattered ad-hoc DDL queries and concurrency race conditions on API worker startup. |
| **4** | **Decouple UI Container Dependencies into `requirements-ui.txt`** | Docker | **High** | **Low** | Shrinks Reflex Docker image by ~2 GB and drastically speeds up CI image builds. |
| **5** | **Fix Bogus and Brittle Tests in `tests/test_api_client.py` and `test_features.py`** | Tests | **Medium** | **Low** | Exercises `load_dashboard()`, fixes `API_BASE_URL` env override testing, and catches specific Pandera schema errors. |

---

## 4. Cross-Domain Dependencies for Future Sessions

1. **Session 1 (Core Foundation)**:
   - [core/db/repositories/warehouse_repo.py:465](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/core/db/repositories/warehouse_repo.py#L465) and [core/db/repositories/user_repo.py:17](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/core/db/repositories/user_repo.py#L17) contain ad-hoc `CREATE TABLE IF NOT EXISTS` queries. Once `docker/postgres/init_schema.sql` (or Alembic) owns the complete DDL schema, these methods should be removed from Core repositories.
2. **Session 3 (Backend API & Serving)**:
   - [apps/api/main.py:26](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/apps/api/main.py#L26) calls `BlackFridayRepository().create_app_tables()` inside the FastAPI lifespan handler. Once schema initialization is managed through Docker/Alembic, this blocking startup call can be eliminated.
3. **Session 4 (AI Subsystem)**:
   - [ai/tools/policy_kb.py:19](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/ai/tools/policy_kb.py#L19) and [ai/extractor/catalog_index.py:26](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/ai/extractor/catalog_index.py#L26) access static JSON files via relative file paths. Should be refactored to use centralized path resolution via `core.config.settings.DATA_DIR`.
4. **Session 5 (Frontend UI)**:
   - [apps/reflex_app/rxconfig.py:6](file:///C:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/apps/reflex_app/rxconfig.py#L6) hardcodes `api_url="http://localhost:8001"`. For production Docker and remote browser access, this should be configurable via `os.getenv("REFLEX_API_URL")`.
