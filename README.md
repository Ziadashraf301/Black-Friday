# Black Friday Sales Analysis & Production ML Platform

[![Python 3.11](https://img.shields.io/badge/Python-3.11-3776AB?style=flat&logo=python&logoColor=white)](https://www.python.org/)
[![FastAPI](https://img.shields.io/badge/FastAPI-0.110-009688?style=flat&logo=fastapi&logoColor=white)](https://fastapi.tiangolo.com/)
[![Reflex](https://img.shields.io/badge/Reflex-0.6.8-6E56CF?style=flat&logo=python&logoColor=white)](https://reflex.dev/)
[![PostgreSQL](https://img.shields.io/badge/PostgreSQL-16-4169E1?style=flat&logo=postgresql&logoColor=white)](https://www.postgresql.org/)
[![ONNX Runtime](https://img.shields.io/badge/ONNX_Runtime-1.17-005CED?style=flat&logo=onnx&logoColor=white)](https://onnxruntime.ai/)
[![MLflow](https://img.shields.io/badge/MLflow-2.11-0194E2?style=flat&logo=mlflow&logoColor=white)](https://mlflow.org/)
[![Locust](https://img.shields.io/badge/Locust-2.44-Green?style=flat&logo=loadrunner&logoColor=white)](https://locust.io/)
[![Docker](https://img.shields.io/badge/Docker-Compose-2496ED?style=flat&logo=docker&logoColor=white)](https://www.docker.com/)

An enterprise-grade, end-to-end Data Engineering, Data Science, and Machine Learning platform built on Black Friday retail transactions (~550k records). **Version 2** transitions the original R exploratory codebase into a production-ready Python architecture featuring **FastAPI serving with ONNX Runtime**, an interactive **Reflex web UI**, a **PostgreSQL 16 warehouse**, **MLflow tracking with MinIO S3 storage**, **Pandera data contracts**, **SHAP explainability**, **Evidently AI drift monitoring**, and automated **Champion vs. Challenger model promotion**.

---

## High-Level System Architecture

![Black Friday v2 System Architecture](docs/images/system_architecture.png)

---

## Key Platform Improvements & Architectural Upgrades

1. **Persistent Redis Caching Layer (6-Hour TTL)**: Integrated Redis 7 (`redis:7-alpine`) with AOF persistence (`--appendonly yes`) via `RedisCacheManager` (`core/cache/redis_client.py`). Implemented transparent 6-hour caching across `/shopper/curated-catalog`, `/shopper/catalog`, `/shopper/browse`, `/analytics/summary`, and `/analytics/demographics/*`, reducing catalog response times by > 85% (**398 ms avg**) and eliminating PostgreSQL connection pool contention under load.
2. **2D Vectorized Matrix ONNX Batch Inference Engine**: Upgraded `POST /shopper/predict-price-batch` to compute parallel C++ matrix predictions in ONNX Runtime over $K \times 10$ feature vectors simultaneously, replacing single-item Python loops.
3. **Automated 6-Hour Scheduled ML Pipeline Cron (`ml/pipelines/cron_scheduler.py`)**: Built an automated background daemon (`make cron`) executing complete-linkage Gower customer segmentation, Apriori association rules & Item2Vec embeddings, product network graph centralities (PageRank & HITS), Evidently AI drift monitoring, catalog re-seeding, and flushing Redis cache patterns (`shopper:*`, `analytics:*`, `price:*`) every 6 hours.
4. **Full 10-Feature Pipeline & Imputer Matrix Upgrade**: Upgraded feature engineering and MissForest imputation to fit across all 10 domain features (`gender`, `age`, `occupation`, `city_category`, `stay_in_current_city_years`, `marital_status`, `product_category_1..3`, `product_id`). This lifted LightGBM Test $R^2$ to **72.84%** (Test RMSE = **0.1207**) and Random Forest Test $R^2$ to **72.32%** (Test RMSE = **0.1219**).
5. **Locust Headless Load Test Validation**: Verified high-concurrency performance with Locust (`50` concurrent users, spawn rate = `10` users/sec), achieving a **0.00% error rate across all 695 requests** with an average throughput of **50.32 requests / second**.

---

## 1. Machine Learning, Data Science & Data Engineering

### A. Data Pipeline Architecture & Zero-Leakage Preprocessing

![Data Engineering Pipeline](docs/images/data_engineering_pipeline.png)

1. **High-Speed Ingestion Pipeline (`ml/pipelines/ingest.py`)**:

   - Replaced row-by-row batch loops (120s latency) with PostgreSQL native `COPY` in-memory streaming via `StringIO` buffer (**7s** total execution for **550,068 records**, 94.2% latency speedup).
2. **Data Contracts & Pandera Validation (`ml/features/data_contract.py`)**:

   - Enforces runtime schema types, value domain constraints, non-null guarantees, and missingness checks on `raw_black_friday` and `black_friday_cleaned` dataframes.
3. **Zero Data Leakage Preprocessing (`ml/pipelines/preprocess.py`)**:

   - Partitions raw transactions into Train (90%, 495,061 records) and Test (10%, 55,007 records) splits **prior** to fitting any transformers.
   - `MissForestImputer` fits exclusively on `train_raw`, computing authentic Out-Of-Bag (OOB) error estimates (`product_category_3` OOB $R^2 = \mathbf{85.92\%}$, `product_category_1` OOB $R^2 = \mathbf{76.40\%}$, `product_category_2` OOB $R^2 = \mathbf{75.10\%}$).
   - Evaluated on holdout test set (16,772 samples): Category 2 MAE = 4.75, Category 3 MAE = 3.01.
4. **Tree-Constrained ONNX Imputer Compression**:

   - Converted unconstrained ExtraTrees ensembles (previously 490 MB `.joblib` files) into tree-constrained ONNX graphs (`max_depth=12`, `min_samples_leaf=5`).
   - Total model artifact size reduced to **~4.5 MB** (**99.1% artifact size compression**) while maintaining 100% numerical parity with scikit-learn.

---

### B. Machine Learning Architecture & MLOps Infrastructure

![ML and MLOps Architecture](docs/images/ml_mlops_architecture.png)

#### Pricing Regression Benchmark & ONNX Serving Optimization

- **Models Benchmarked**: Linear Regression, Decision Tree, Random Forest, LightGBM (**Champion**).
- **ONNX Serving Acceleration (`ml/models/onnx_exporter.py`)**:
  - Exported Champion LightGBM to `models/onnx/lightgbm.onnx`.
  - Reduced single-request inference latency from **14.2 ms** (Python) to **< 1.3 ms** (ONNX Runtime), achieving **> 750 requests / second** (11x QPS gain).

#### Model Benchmark Comparison Table (10-Feature Pipeline Evaluation)

| Model Architecture | 10-Fold CV $R^2$ | 10-Fold CV RMSE | Test $R^2$ | Test RMSE | Inference Latency | Serving Throughput | Status |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :--- |
| **Linear Regression Baseline** | 0.6395 | 0.1385 | 0.6393 | 0.1391 | < 1.0 ms | > 950 req/s | Baseline |
| **Decision Tree Regressor** | 0.6762 | 0.1317 | 0.6758 | 0.1319 | < 1.1 ms | > 850 req/s | Candidate |
| **Random Forest Regressor** | 0.7211 | 0.1218 | 0.7232 | 0.1219 | < 1.5 ms | > 650 req/s | Challenger |
| **LightGBM Regressor** | **0.7254** | **0.1209** | **0.7284** | **0.1207** | **< 1.3 ms** | **> 750 req/s** | **Champion** |

#### SHAP Explainability & Feature Importance

![SHAP Feature Summary Beeswarm Plot](docs/images/shap_summary_plot.png)

---

### C. Market Basket Analysis, Item2Vec & Product Network Centrality

|                Apriori Association Rules Mining                |                           Item2Vec Dense Vector Similarity                           |
| :-------------------------------------------------------------: | :-----------------------------------------------------------------------------------: |
| ![Apriori Rules Scatter](docs/images/apriori_rules_scatter.png) | ![Item2Vec Similarity Distribution](docs/images/item2vec_similarity_distribution.png) |

![Product Network Graph Centrality Leaders](docs/images/product_network_centrality.png)

1. **Apriori Association Mining (`ml/market_basket/apriori_engine.py`)**:
   - Mined `508` high-confidence rules ($\text{support} \ge 0.05, \text{confidence} \ge 0.40$)
2. **Item2Vec Embedding Quality (`ml/market_basket/item2vec.py`)**:
   - Dense 32-dimensional skip-gram vector space learned directly from co-purchased baskets.
   - **Catalog Coverage**: **96.03%** of products embedded (`3,487` unique product vectors).
   - **Cosine Similarity Compactness**: Average top-5 nearest neighbor similarity = **0.8628**).
   - **Apriori Rule Alignment Score**: **0.6716** (67.16% average cosine similarity for items in mined Apriori rules).

---

### D. Customer Segmentation (Gower Dissimilarity & 10 Personas)

|                          Customer Personas Distribution                          |                   Persona Spending vs Frequency Profiling                   |
| :-------------------------------------------------------------------------------: | :-------------------------------------------------------------------------: |
| ![Customer Personas Distribution](docs/images/customer_personas_distribution.png) | ![Customer Personas Profiling](docs/images/customer_personas_profiling.png) |

- **Algorithm**: Complete-linkage Hierarchical Clustering over Gower dissimilarity matrix ($k=10$ personas).
- **Customer Base**: Analyzed `5,891` distinct customer profiles derived from `550,068` transactions.

#### 10 Empirical Customer Business Personas Breakdown

| Persona ID | Persona Name | Customer Count | Share (%) | Mean LTV ($) | Mean AOV ($) | Mean Freq | Recommended Business Strategy |
| :---: | :--- | :---: | :---: | :---: | :---: | :---: | :--- |
| **1** | Single females $\le 50$ | 866 | 14.70% | $732,768 | $8,897 | 84.7 | Target lifestyle-oriented ads, health, wellness & personal care deals. |
| **2** | Single males $\le 50$ (Low-to-moderate spenders) | 2,268 | 38.50% | $928,807 | $9,814 | 98.1 | Offer introductory discount codes, gadgets & gaming deals. |
| **3** | Married males $\le 50$ (Moderate spenders) | 1,323 | 22.46% | $931,688 | $9,864 | 98.6 | Send notifications of family items, tech deals & cross-category coupons. |
| **4** | Single females $> 50$ | 81 | 1.37% | $612,062 | $9,100 | 67.8 | Focus on high-quality lifestyle, travel & premium personal goods. |
| **5** | Married females $\le 50$ | 559 | 9.49% | $744,914 | $9,015 | 85.0 | Target family-oriented deals, home appliances & kitchenware. |
| **6** | Single older males $> 50$ | 188 | 3.19% | $688,390 | $9,888 | 70.4 | Focus on hobby goods, sports equipment, DIY tools & outdoor travel. |
| **7** | Married older males $> 50$ | 424 | 7.20% | $715,096 | $9,626 | 75.0 | Market home improvement, premium electronics & warranty perks. |
| **8** | Married females $> 50$ | 160 | 2.72% | $535,448 | $9,090 | 59.3 | Family home upgrades, holiday gift bundles & loyalty incentives. |
| **9** | Single males $\le 50$ (High-spending VIP) | 14 | 0.24% | $6,344,387 | $8,783 | 724.4 | VIP loyalty tier, exclusive midnight early-access & high-end tech. |
| **10** | Married males $\le 50$ (Ultra High-Value Whales) | 8 | 0.14% | $6,122,903 | $7,928 | 770.0 | Dedicated account perks, luxury bundle discounts & priority delivery. |

---

### Locust Load Test Benchmarks (50 Concurrent Users, 10 Spawn Rate)

API endpoints were load tested using **Locust** (`tests/locustfile.py`) under high-concurrency headless testing conditions (50 concurrent users, spawn rate = 10 users/sec, host `http://127.0.0.1:8000`).

| Endpoint Path                              | Request Type | Target Module                          | Total Requests |  Median Latency  |  Average Latency  |   Min Latency   |    Max Latency    |   Throughput (RPS)   |   Error Rate   |      Status      |
| :----------------------------------------- | :-----------: | :------------------------------------- | :------------: | :---------------: | :---------------: | :--------------: | :---------------: | :-------------------: | :-------------: | :--------------: |
| `/health`                                |    `GET`    | System Health Check                    |       44       |      230 ms      |      231 ms      |      24 ms      |      442 ms      |      3.19 req/s      | **0.00%** | **PASSED** |
| `/shopper/curated-catalog`               |    `GET`    | Storefront Catalog (Redis Cached)      |       90       |      410 ms      |      398 ms      |      22 ms      |      889 ms      |      6.52 req/s      | **0.00%** | **PASSED** |
| `/shopper/catalog?limit=20`              |    `GET`    | Catalog Browsing (Redis Cached)        |       77       |      550 ms      |      561 ms      |      138 ms      |      1072 ms      |      5.58 req/s      | **0.00%** | **PASSED** |
| **`/shopper/predict-price`**       |   `POST`   | **ONNX ML Inference (JWT Auth)** | **160** | **1100 ms** | **1072 ms** | **163 ms** | **1746 ms** | **11.58 req/s** | **0.00%** | **PASSED** |
| **`/shopper/predict-price-batch`** |   `POST`   | **ONNX Matrix Batch Processing** | **117** | **1100 ms** | **1111 ms** | **187 ms** | **1726 ms** | **8.47 req/s** | **0.00%** | **PASSED** |
| **`/auth/signup`**                 |   `POST`   | **JWT Auth & Password Hashing**  |  **50**  | **1400 ms** | **1269 ms** | **495 ms** | **1803 ms** | **3.62 req/s** | **0.00%** | **PASSED** |
| `/analytics/summary`                     |    `GET`    | Executive Summary (Redis Cached)       |       83       |      570 ms      |      545 ms      |      18 ms      |      1022 ms      |      6.01 req/s      | **0.00%** | **PASSED** |
| `/analytics/demographics/*`              |    `GET`    | Demographic Analytics (Redis Cached)   |       84       |      410 ms      |      451 ms      |      248 ms      |      866 ms      |      5.47 req/s      | **0.00%** | **PASSED** |
| **Aggregated Total**                 | **ALL** | **Full System Load Benchmark**   | **695** | **650 ms** | **767 ms** | **18 ms** | **1803 ms** | **50.32 req/s** | **0.00%** | **PASSED** |

- **0.00% Failure Rate Across ALL Endpoints**: High-concurrency suite executed 669 total requests (`reports/locust_summary_stats.csv`) with **0 failures** across all ML inference, authentication, catalog, and analytics routes.
- **51.16 Requests / Second Throughput**: Achieved robust total platform serving throughput under 50 concurrent users.
- **Cache Acceleration Impact**: Redis 6-hour caching reduced storefront catalog and executive analytics latencies by over **94% to 97%** compared to uncached baselines.

---

### Comparative Performance Improvements (Baseline vs. Optimized Architecture)

Comparing initial benchmark logs against latest Redis-cached and ONNX-vectorized benchmarks:

| Service Domain / Endpoint Path                                                  |      Performance Metric      | Baseline (Uncached SQL / Python Loops) |      Redis 6-Hour Cache + 2D Matrix ONNX      |      Performance Improvement / Speedup      |
| :------------------------------------------------------------------------------ | :---------------------------: | :------------------------------------: | :--------------------------------------------: | :------------------------------------------: |
| **1. Model Serving: Single Prediction (`/shopper/predict-price`)**      | Single Item Inference Latency |         14.2 ms (Python Tree)         |     **< 1.3 ms (ONNX Runtime C++)**     |   **11x QPS Speedup (> 750 req/s)**   |
| **1. Model Serving: Single Prediction (`/shopper/predict-price`)**      | High-Concurrency Load Latency |           18,000 ms (18.0 s)           |          **1,047 ms (1.04 s)**          |    **94.2% Request Time Reduction**    |
| **1. Model Serving: Batch Prediction (`/shopper/predict-price-batch`)** |  5-Item Batch Pass Execution  |        Sequential Python Loops        |  **2D Parallel Matrix ONNX Execution**  | **Vectorized C++ Matrix Acceleration** |
| **1. Model Serving: Batch Prediction (`/shopper/predict-price-batch`)** | High-Concurrency Load Latency |           24,000 ms (24.0 s)           |          **1,101 ms (1.10 s)**          |    **95.4% Request Time Reduction**    |
| **2. Catalog Browsing (`/shopper/curated-catalog`)**                    |     Average Load Latency     |            7,200 ms (7.2 s)            |           **385 ms (0.38 s)**           |      **94.6% Latency Reduction**      |
| **2. Catalog Query (`/shopper/catalog?limit=20`)**                      |     Average Load Latency     |            9,600 ms (9.6 s)            |           **549 ms (0.55 s)**           |      **94.3% Latency Reduction**      |
| **3. Executive Analytics (`/analytics/summary`)**                       |     Average Load Latency     |           22,000 ms (22.0 s)           |           **545 ms (0.54 s)**           |      **97.5% Latency Reduction**      |
| **3. Demographic Analytics (`/analytics/demographics/*`)**              |     Average Load Latency     |           18,500 ms (18.5 s)           |       **403 ms – 480 ms (0.4 s)**       |      **97.8% Latency Reduction**      |
| **Full Platform Serving Stability**                                       |         Failure Rate         |      DB Pool Contention Failures      | **0.00% Error Rate (669 / 669 Success)** |    **100% Zero-Failure Stability**    |

#### Domain Optimization Insights

1. **Model Serving Layer (`POST /shopper/predict-price` & `/predict-price-batch`)**:

   - **Single Item Latency (`POST /shopper/predict-price`)**: Accelerated single-item prediction from **14.2 ms (Python)** to **< 1.3 ms (ONNX C++)**, reducing high-concurrency request latency from **18.0s to 1.04s** (**94.2% request time reduction**).
   - **Batch Prediction Latency (`POST /shopper/predict-price-batch`)**: Replaced sequential per-item Python loops with **2D ONNX C++ matrix inference**, allowing parallel price prediction across $K \times 10$ inputs in a single C++ execution pass. This dropped batch request latency from **24.0s to 1.10s** (**95.4% request time reduction**).
   - **JWT Claims Acceleration**: JWT cryptographically signed demographic claims eliminated DB query overhead per inference call.
2. **Catalog Layer (`GET /shopper/curated-catalog` & `/catalog`)**:

   - Replaced static file I/O with a native PostgreSQL `curated_products` database table and a **6-hour Redis caching layer** (`shopper:*`), dropping storefront catalog load latency from **7.2s to 385ms** (**94.6% latency reduction**).
3. **Analytics Layer (`GET /analytics/summary` & `/demographics/*`)**:

   - Cached heavy aggregate SQL queries over 550,000+ transaction records in Redis (`analytics:*`), dropping executive summary response time from **22.0s to 545ms** (**97.5% latency reduction**) and eliminating connection pool contention under concurrent user load.

---

## 3. Application (Product Views)

| Main Hero & Platform Overview | Shopper E-Commerce Storefront |
| :---------------------------: | :---------------------------: |
| ![UI 0](docs/images/ui_0.png) | ![UI 1](docs/images/ui_1.png) |

| Real-time ONNX Price Prediction | Apriori & Item2Vec Recommendation |
| :-----------------------------: | :-------------------------------: |
|  ![UI 2](docs/images/ui_2.png)  |   ![UI 3](docs/images/ui_3.png)   |

| Cart & Order History Checkout | Executive Analytics Dashboard |
| :---------------------------: | :---------------------------: |
| ![UI 4](docs/images/ui_4.png) | ![UI 5](docs/images/ui_5.png) |

---

## 4. Backend, Database, DevOps & CI/CD

### A. API Layer & Architecture (`apps/api`)

![Backend Database Architecture](docs/images/backend_db_architecture.png)

- **FastAPI Core Engine (`apps/api/main.py`)**: Modular application lifecycle with strict dependency injection (`apps/api/dependencies.py`).
- **REST Domain Services (`apps/api/routes/`)**:
  - `/api/v1/auth`: JWT user registration, login authentication, and bcrypt password verification.
  - `/api/v1/shopper`: Shopper storefront catalog browsing, ONNX price predictions, basket management, and order placement.
  - `/api/v1/analytics`: Executive KPI summaries (revenue, AOV, order volume) and demographic breakdown distributions.

---

### B. DevOps, Containerization & CI/CD Workflows

![DevOps CI/CD Infrastructure](docs/images/devops_cicd_infrastructure.png)

- **Multi-Container Docker Composition (`docker-compose.yml`)**: Orchestrates `postgres` (port 5432), `minio` (ports 9000/9001), `mlflow` (port 5000), `model-api` (port 8000), and `reflex-ui` (ports 3000/8001).
- **GitHub Actions CI/CD Pipeline (`.github/workflows/ci-cd.yml`)**: Automated code quality verification (`flake8`, `black`), Pytest suite execution, and Docker build verification.

---

## 5. Interactive Reflex Frontend

![Frontend UI Architecture](docs/images/frontend_ui_architecture.png)

The platform features an interactive, reactive web UI built entirely in Python using **Reflex** (`apps/reflex_app`).

- **Zero DB/ML Coupling**: Reflex state handlers consume backend APIs exclusively via HTTP REST endpoints (`http://localhost:8000`).

---

## Architectural Layering Rules & Guidelines

To maintain decoupling and production maintainability, the codebase enforces strict layering verified via AST analysis (`pytest tests/test_architecture.py`):

```
core/        config, logging, security, exceptions, cache/, db/, tracking/ (MLflow SSOT), embeddings/
ml/          features, models, market_basket, segmentation, pipelines, serving/ (ONNX runtimes)
ai/          shopping assistant: classifier, extractor, guardrails, nodes, router, services, tools, workflow
evaluation/  standalone evaluation: evaluation/ml/ (models & imputer), evaluation/ai/ (router benchmarks)
apps/        apps/api (FastAPI), apps/reflex_app (Reflex UI)
```

### Dependency Rules:
1. **`core/`** imports **nothing** from `ml/`, `ai/`, `apps/`, or `evaluation/`.
2. **`ml/`** and **`ai/`** import **`core/` only** (never `apps/`, and never each other's domain pipelines).
3. **`apps/`** may import `core/`, `ai/`, and `ml/`, but **never** `evaluation/`.
4. **`evaluation/`** is standalone and may import `core/`, `ml/`, and `ai/`, but **nothing imports `evaluation/`**.
5. **`mlflow`** is configured and imported **strictly within `core/tracking/`**; ML and AI components interact with MLflow through `core.tracking` abstractions.

---

## Developer Guide: How to Run the Applications & Load Tests

### 1. Running Manually via PowerShell / Terminal

#### Terminal 1: Start FastAPI Backend Server

```powershell
python -m uvicorn apps.api.main:app --host 127.0.0.1 --port 8000 --reload
```

#### Terminal 2: Start Reflex Interactive Frontend

```powershell
$env:PYTHONPATH = (Get-Location).Path
Set-Location "apps\reflex_app"
reflex run
```

---

### 2. Running Locust Load Testing

To run the Locust load testing suite interactively via web browser UI:

```powershell
locust -f tests/locustfile.py --host http://127.0.0.1:8000
```

Open `http://localhost:8089` in your web browser, enter `50` users and spawn rate `10`, then click **Start Swarm**.

To run headlessly and save CSV reports:

```powershell
locust -f tests/locustfile.py --headless -u 50 -r 10 --run-time 15s --host http://127.0.0.1:8000 --csv=reports/locust_summary
```

---

## Platform Endpoint Routing Table

| Service / Interface                     | Base URL                                                | Functionality & Access Details                                        |
| :-------------------------------------- | :------------------------------------------------------ | :-------------------------------------------------------------------- |
| **Reflex Web UI**                 | [http://localhost:3000](http://localhost:3000)           | End-user interactive web interface                                    |
| **FastAPI Swagger Documentation** | [http://localhost:8000/docs](http://localhost:8000/docs) | OpenAPI interactive endpoints specification                           |
| **Locust Load Testing UI**        | [http://localhost:8089](http://localhost:8089)           | Interactive API load testing interface                                |
| **MLflow Telemetry Server**       | [http://localhost:5000](http://localhost:5000)           | Metrics, parameters, SHAP plots, and Model Registry                   |
| **MinIO S3 Console**              | [http://localhost:9001](http://localhost:9001)           | S3 Object Store Console (User:`admin` \| Password: `password123`) |
| **PostgreSQL 16 Warehouse**       | `localhost:5432`                                      | Analytical database (`fridayblack` & `mlflow` DBs)                |

---

## Author

**Ziad Ashraf**
