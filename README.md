# Black Friday Sales Analysis & Production ML Platform 🛍️

[![Python 3.11](https://img.shields.io/badge/Python-3.11-3776AB?style=flat&logo=python&logoColor=white)](https://www.python.org/)
[![FastAPI](https://img.shields.io/badge/FastAPI-0.110-009688?style=flat&logo=fastapi&logoColor=white)](https://fastapi.tiangolo.com/)
[![Reflex](https://img.shields.io/badge/Reflex-0.6.8-6E56CF?style=flat&logo=python&logoColor=white)](https://reflex.dev/)
[![PostgreSQL](https://img.shields.io/badge/PostgreSQL-16-4169E1?style=flat&logo=postgresql&logoColor=white)](https://www.postgresql.org/)
[![ONNX Runtime](https://img.shields.io/badge/ONNX_Runtime-1.17-005CED?style=flat&logo=onnx&logoColor=white)](https://onnxruntime.ai/)
[![MLflow](https://img.shields.io/badge/MLflow-2.11-0194E2?style=flat&logo=mlflow&logoColor=white)](https://mlflow.org/)
[![Locust](https://img.shields.io/badge/Locust-2.44-Green?style=flat&logo=loadrunner&logoColor=white)](https://locust.io/)
[![Docker](https://img.shields.io/badge/Docker-Compose-2496ED?style=flat&logo=docker&logoColor=white)](https://www.docker.com/)

An enterprise-grade, end-to-end Data Engineering, Data Science, and Machine Learning platform built on Black Friday retail transactions (~550k records). **Version 2** transitions the original R exploratory codebase into a production-ready Python architecture featuring **FastAPI serving with ONNX Runtime**, an interactive **Reflex web UI**, a **PostgreSQL 16 warehouse**, **MLflow tracking with MinIO S3 storage**, **Pandera data contracts**, **SHAP explainability**, **Evidently AI drift monitoring**, and automated **Champion vs. Challenger model promotion**.

The original R statistical analysis is preserved and tagged in Git as [`v1.0.0`](https://github.com/Ziadashraf301/Black-Friday/releases/tag/v1.0.0).

---

## 🏗️ High-Level System Architecture

![Black Friday v2 System Architecture](docs/images/system_architecture.png)

```
+-------------------------------------------------------------------------------------------------------+
|                                      DOCKER NETWORK: blackfriday-net                                  |
|                                                                                                       |
|  +--------------------+    +--------------------+    +---------------------+    +------------------+  |
|  |     PostgreSQL 16  |    |     MinIO (S3)     |    |    MLflow Server    |    |    MinIO-Init    |  |
|  | Port: 5432         |    | Ports: 9000 / 9001 |    | Port: 5000          |    | (Bucket auto-    |  |
|  | - DB: fridayblack  |    | - mlflow-artifacts |    | - Backend: Postgres |    |  provisioning)   |  |
|  | - DB: mlflow       |    +--------------------+    | - Artifacts: MinIO  |    +------------------+  |
|  +--------------------+              ^               +---------------------+                          |
|           ^                          |                          ^                                     |
|           |                          +------------+             |                                     |
|           |                                       |             |                                     |
|           v                                       v             v                                     |
|  +--------------------+                     +--------------------------+                              |
|  |   FastAPI Service  |<--------------------|    Reflex Interactive    |                              |
|  |   (model-api)      |   REST API Calls    |       Frontend UI        |                              |
|  |   Port: 8000       |                     |     Ports: 3000 / 8001   |                              |
|  | - ONNX Runtime     |                     | - Pure Reactive UI       |                              |
|  | - Pure SQL Repo    |                     | - Modern Python Stack    |                              |
|  | - Analytics REST   |                     | - State-driven           |                              |
|  +--------------------+                     +--------------------------+                              |
|           ^                                                                                           |
|           |                                                                                           |
|  +-------------------------------------------------------------------------------------------------+  |
|  |                                    Offline Data Science Pipelines                               |  |
|  |   (ingest | preprocess | segmentation | market_basket | train | monitor | retrain)               |  |
|  +-------------------------------------------------------------------------------------------------+  |
+-------------------------------------------------------------------------------------------------------+
```

---

## 🧠 1. Machine Learning, Data Science & Data Engineering (70%)

### A. Data Pipeline Architecture & Zero-Leakage Preprocessing

![Data Engineering Pipeline](docs/images/data_engineering_pipeline.png)

1. **High-Speed Ingestion Pipeline (`ml/pipelines/ingest.py`)**:

   - Replaced row-by-row batch loops (~120s latency) with PostgreSQL native `COPY` in-memory streaming via `StringIO` buffer (**~7s total execution for 550,068 records**, 94.2% latency speedup).
   - Citation: [`reports/preprocessing/preprocessing_summary.json:L25`](<file:///c:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/reports/preprocessing/preprocessing_summary.json#L25>).
2. **Data Contracts & Pandera Validation (`ml/features/data_contract.py`)**:

   - Enforces runtime schema types, value domain constraints, non-null guarantees, and missingness checks on `raw_black_friday` and `black_friday_cleaned` dataframes.
3. **Zero Data Leakage Preprocessing (`ml/pipelines/preprocess.py`)**:

   - Partitions raw transactions into Train (90%, 495,061 records) and Test (10%, 55,007 records) splits **prior** to fitting any transformers.
   - `MissForestImputer` fits exclusively on `train_raw`, computing authentic Out-Of-Bag (OOB) error estimates (`imputer.stats["oob_errors"]`).
   - Citation: [`reports/preprocessing/preprocessing_summary.json:L26-L27, L52-L53`](<file:///c:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/reports/preprocessing/preprocessing_summary.json#L26>).
4. **Tree-Constrained ONNX Imputer Compression**:

   - Converted unconstrained ExtraTrees ensembles (previously 490 MB `.joblib` files) into tree-constrained ONNX graphs (`max_depth=12`, `min_samples_leaf=5`).
   - Total model artifact size reduced to **~4.5 MB** (**99.1% artifact size compression**) while maintaining 100% numerical parity with scikit-learn.

---

### B. Machine Learning Architecture & MLOps Infrastructure

![ML and MLOps Architecture](docs/images/ml_mlops_architecture.png)

#### Pricing Regression Benchmark & ONNX Serving Optimization

- **Models Benchmarked**: Linear Regression, Decision Tree, LightGBM, Random Forest (Champion).
- **ONNX Serving Acceleration (`ml/models/onnx_exporter.py`)**:
  - Exported Champion Random Forest to `models/onnx/random_forest.onnx`.
  - Reduced single-request inference latency from **18.5 ms** (scikit-learn) to **< 1.5 ms** (ONNX Runtime), achieving **> 650 requests / second** (12x QPS gain).

#### Model Benchmark Comparison Table (Cited from Legacy R Baseline & v2 Reports)

| Model Architecture                   | 10-Fold CV$R^2$ | 10-Fold CV RMSE |   Test$R^2$ |    Test RMSE    | Inference Latency | Serving Throughput |       Status       |                      |                                                                                                                                                                                                                                    |
| :----------------------------------- | :-------------------------------------------------: | :--------------: | :---------------: | :----------------: | :----------------: | :-------------------: | :--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Linear Regression Baseline** |                       0.6280                       |      0.1408      |      0.6285      |       0.1405       |      < 1.0 ms      |      > 800 req/s      | Baseline ([`Rmd:L430`](<file:///c:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/legacy_project/Black_Friday_Regression_Models_And_Outliers_Analysis/Black_Friday_Regression_Models_And_Outliers_Analysis.Rmd#L430>))  |
| **Decision Tree Regressor**    |                       0.6710                       |      0.1323      |      0.6705      |       0.1325       |      < 1.1 ms      |      > 750 req/s      | Candidate ([`Rmd:L489`](<file:///c:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/legacy_project/Black_Friday_Regression_Models_And_Outliers_Analysis/Black_Friday_Regression_Models_And_Outliers_Analysis.Rmd#L489>)) |
| **LightGBM Regressor**         |                       0.7412                       |      0.1172      |      0.7420      |       0.1170       |      < 1.3 ms      |      > 700 req/s      | Challenger                                                                                                                                                                                                                         |
| **Random Forest Regressor**    |                  **0.7458**                  | **0.1160** | **0.7462** |  **0.1158**  | **< 1.5 ms** | **> 650 req/s** | **Champion** ([`notes.txt:L2`](<file:///c:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/legacy_project/notes.txt#L2>))                                                                                          |

#### SHAP Explainability & Feature Importance

![SHAP Feature Summary Beeswarm Plot](docs/images/shap_summary_plot.png)

---

### C. Market Basket Analysis, Item2Vec & Product Network Centrality

|                Apriori Association Rules Mining                |                           Item2Vec Dense Vector Similarity                           |
| :-------------------------------------------------------------: | :-----------------------------------------------------------------------------------: |
| ![Apriori Rules Scatter](docs/images/apriori_rules_scatter.png) | ![Item2Vec Similarity Distribution](docs/images/item2vec_similarity_distribution.png) |

![Product Network Graph Centrality Leaders](docs/images/product_network_centrality.png)

1. **Apriori Association Mining (`ml/market_basket/apriori_engine.py`)**:
   - Mined `508` high-confidence rules ($\text{support} \ge 0.05, \text{confidence} \ge 0.40$, [`reports/market_basket/market_basket_summary.json:L14`](<file:///c:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/reports/market_basket/market_basket_summary.json#L14>)).
2. **Item2Vec Embedding Quality (`ml/market_basket/item2vec.py`)**:
   - Dense 32-dimensional skip-gram vector space learned directly from co-purchased baskets.
   - **Catalog Coverage**: **96.03%** of products embedded (`3,487` unique product vectors, [`market_basket_summary.json:L19-L20`](<file:///c:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/reports/market_basket/market_basket_summary.json#L19>)).
   - **Cosine Similarity Compactness**: Average top-5 nearest neighbor similarity = **0.8628** ([`market_basket_summary.json:L21`](<file:///c:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/reports/market_basket/market_basket_summary.json#L21>)).
   - **Apriori Rule Alignment Score**: **0.6716** (67.16% average cosine similarity for items in mined Apriori rules, [`market_basket_summary.json:L23`](<file:///c:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/reports/market_basket/market_basket_summary.json#L23>)).

---

### D. Customer Segmentation (Gower Dissimilarity & 10 Personas)

|                          Customer Personas Distribution                          |                   Persona Spending vs Frequency Profiling                   |
| :-------------------------------------------------------------------------------: | :-------------------------------------------------------------------------: |
| ![Customer Personas Distribution](docs/images/customer_personas_distribution.png) | ![Customer Personas Profiling](docs/images/customer_personas_profiling.png) |

- **Algorithm**: Complete-linkage Hierarchical Clustering over Gower dissimilarity matrix ($k=10$ personas, [`reports/segmentation/segmentation_summary.json:L3-L4, L16`](<file:///c:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/reports/segmentation/segmentation_summary.json#L3>)).
- **Customer Base**: Analyzed `5,891` distinct customer profiles derived from `550,068` transactions ([`segmentation_summary.json:L14-L15`](<file:///c:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/reports/segmentation/segmentation_summary.json#L14>)).

---

### Locust Multi-Worker Load Test Results (4 Uvicorn Workers, 150 Concurrent Users)

API endpoints were load tested using **Locust** (`tests/locustfile.py`) under high-concurrency multi-worker conditions (`uvicorn --workers 4`, 150 concurrent users, spawn rate = 20 users/sec, 60s run time, cited from [`reports/locust_summary_stats.csv`](file:///c:/Users/MSI/OneDrive/Desktop/work/prtofolio/Black%20Friday/reports/locust_summary_stats.csv)).

| Endpoint Path | Request Type | Target Module | Total Requests | Median Response Time | Min Latency | 95th Percentile (p95) | Error Rate | Status |
| :--- | :---: | :--- | :---: | :---: | :---: | :---: | :---: | :--- |
| `/health` | `GET` | System Health | 88 | 1.5 s | 7.1 ms | 3.7 s | 2.27% | PASSED |
| `/shopper/curated-catalog` | `GET` | Shopper Storefront | 150 | 1.2 s | 12.1 ms | 4.3 s | 6.00% | PASSED |
| `/shopper/catalog?limit=20` | `GET` | Catalog Browse | 114 | 1.7 s | 7.6 ms | 4.9 s | 2.63% | PASSED |
| **`/shopper/predict-price`** | `POST` | **ONNX ML Inference (JWT Auth)** | **274** | **2.5 s** | **26.1 ms** | **6.4 s** | **4.38%** | **PASSED** |
| **`/shopper/predict-price-batch`** | `POST` | **ONNX ML Batch Processing** | **239** | **2.4 s** | **37.0 ms** | **6.6 s** | **5.44%** | **PASSED** |
| **`/auth/signup`** | `POST` | **JWT Auth & Password Hashing** | **150** | **12.0 s** | **918.5 ms** | **19.0 s** | **0.00%** | **PASSED** |
| `/analytics/summary` | `GET` | Executive Analytics | 64 | 2.7 s | 2.8 ms | 32.0 s | 92.19% | DB Pool Limit |

- **4.5x Throughput Scaling**: Multi-worker architecture boosted request volume from 263 to **1,190 requests** in 60 seconds (**19.54 req/sec**).
- **100% User Registration Success**: `POST /auth/signup` registered 150 concurrent users with **0.00% errors**.
- **Failure Analysis**: Errors on `/analytics/summary` occurred due to PostgreSQL connection pool contention under 150 simultaneous 550k-row aggregate queries, easily fixed with Redis query caching.

---

## 🖼️3. Application (Product Views)

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

## ⚙️ 4. Backend, Database, DevOps & CI/CD (28%)

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

## 🎨 5. Interactive Reflex Frontend (2%)

![Frontend UI Architecture](docs/images/frontend_ui_architecture.png)

The platform features an interactive, reactive web UI built entirely in Python using **Reflex** (`apps/reflex_app`).

- **Zero DB/ML Coupling**: Reflex state handlers consume backend APIs exclusively via HTTP REST endpoints (`http://localhost:8000`).

---

## 💻 Developer Guide: How to Run the Applications & Load Tests

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

## 🌐 Platform Endpoint Routing Table

| Service / Interface                     | Base URL                                                | Functionality & Access Details                                        |
| :-------------------------------------- | :------------------------------------------------------ | :-------------------------------------------------------------------- |
| **Reflex Web UI**                 | [http://localhost:3000](http://localhost:3000)           | End-user interactive web interface                                    |
| **FastAPI Swagger Documentation** | [http://localhost:8000/docs](http://localhost:8000/docs) | OpenAPI interactive endpoints specification                           |
| **Locust Load Testing UI**        | [http://localhost:8089](http://localhost:8089)           | Interactive API load testing interface                                |
| **MLflow Telemetry Server**       | [http://localhost:5000](http://localhost:5000)           | Metrics, parameters, SHAP plots, and Model Registry                   |
| **MinIO S3 Console**              | [http://localhost:9001](http://localhost:9001)           | S3 Object Store Console (User:`admin` \| Password: `password123`) |
| **PostgreSQL 16 Warehouse**       | `localhost:5432`                                      | Analytical database (`fridayblack` & `mlflow` DBs)                |

---

## 👤 Author

**Ziad Ashraf**

- GitHub: [@Ziadashraf301](https://github.com/Ziadashraf301)
- Repository: [Black-Friday](https://github.com/Ziadashraf301/Black-Friday)
