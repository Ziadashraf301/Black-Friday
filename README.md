# Black Friday Sales Analysis & Production ML Platform 🛍️

[![Python 3.11](https://img.shields.io/badge/Python-3.11-3776AB?style=flat&logo=python&logoColor=white)](https://www.python.org/)
[![FastAPI](https://img.shields.io/badge/FastAPI-0.110-009688?style=flat&logo=fastapi&logoColor=white)](https://fastapi.tiangolo.com/)
[![Streamlit](https://img.shields.io/badge/Streamlit-1.32-FF4B4B?style=flat&logo=streamlit&logoColor=white)](https://streamlit.io/)
[![PostgreSQL](https://img.shields.io/badge/PostgreSQL-16-4169E1?style=flat&logo=postgresql&logoColor=white)](https://www.postgresql.org/)
[![ONNX](https://img.shields.io/badge/ONNX_Runtime-1.17-005CED?style=flat&logo=onnx&logoColor=white)](https://onnxruntime.ai/)
[![MLflow](https://img.shields.io/badge/MLflow-2.11-0194E2?style=flat&logo=mlflow&logoColor=white)](https://mlflow.org/)
[![DVC](https://img.shields.io/badge/DVC-3.48-945DD6?style=flat&logo=dvc&logoColor=white)](https://dvc.org/)
[![Docker](https://img.shields.io/badge/Docker-Compose-2496ED?style=flat&logo=docker&logoColor=white)](https://www.docker.com/)

An enterprise-grade, end-to-end Data Engineering, Data Science, and Machine Learning platform built on Black Friday retail transactions (~550k records). **Version 2** transitions the original R exploratory codebase into a production-ready Python architecture featuring **FastAPI serving with ONNX Runtime**, an interactive **Streamlit analytical dashboard**, a **PostgreSQL 16 warehouse**, **MLflow tracking with MinIO S3 storage**, **Pandera data contracts**, **SHAP explainability**, **Evidently AI drift monitoring**, and automated **Champion vs. Challenger model promotion**.

The original R statistical analysis has been preserved and tagged in Git as [`v1.0.0`](https://github.com/Ziadashraf301/Black-Friday/releases/tag/v1.0.0).

---

## 🎯 Architecture & Systems Overview

```
+---------------------------------------------------------------------------------------------------+
|                                      DOCKER NETWORK: blackfriday-net                              |
|                                                                                                   |
|  +--------------------+    +--------------------+    +---------------------+    +---------------+  |
|  |     PostgreSQL 16  |    |     MinIO (S3)     |    |    MLflow Server    |    |   MinIO-Init  |  |
|  | Port: 5432         |    | Ports: 9000 / 9001 |    | Port: 5000          |    | (Bucket auto- |  |
|  | - DB: fridayblack  |    | - mlflow-artifacts |    | - Backend: Postgres |    |  provision)   |  |
|  | - DB: mlflow       |    | - blackfriday-dvc  |    | - Artifacts: MinIO  |    +---------------+  |
|  +--------------------+    +--------------------+    +---------------------+                       |
|           ^                                                     ^                                  |
|           |                 +------------------+                |                                  |
|           +-----------------| Training/Retrain |----------------+                                  |
|           |                 |    Pipelines     |                                                   |
|           |                 +------------------+                                                   |
|           v                                                                                        |
|  +--------------------+                     +---------------------+                                |
|  |   FastAPI Service  |<--------------------|  Streamlit Product  |                                |
|  |   (model-api)      |   REST API Only     |  (Frontend Client)  |                                |
|  |   Port: 8000       |                     |  Port: 8501         |                                |
|  |   - ONNX Runtime   |                     |  - Strict Consumer  |                                |
|  |   - Pure SQL Repo  |                     |    of FastAPI       |                                |
|  |   - Analytics APIs |                     +---------------------+                                |
|  +--------------------+                                                                            |
+---------------------------------------------------------------------------------------------------+
```

---

## 🔬 Core Components & Statistical Parity

### 1. High-Performance Pricing Regression (ONNX Runtime)
- **Outlier Detection:** IQR filtering flags transactions exceeding **$21,400.50** (0.4% of data, isolating Category 10 orders).
- **Target Normalization:** Divides `Purchase` by $Purchase_{max} = \$21,399.00$ for numerical stability.
- **Model-Agnostic Registry:** Config-driven model swapping via `DEFAULT_REGRESSION_MODEL` supporting Linear Regression, Decision Tree, Random Forest, and LightGBM.
- **Benchmarks (10-Fold CV):**
  - **Linear Regression:** $R^2 = 62.80\%$, RMSE = 0.1408
  - **Decision Tree:** $R^2 = 67.10\%$, RMSE = 0.1323
  - **LightGBM Regressor:** $R^2 = 74.12\%$, RMSE = 0.1172
  - **Random Forest (Champion):** **$R^2 = 74.58\%$**, **RMSE = 0.1160**
- **ONNX Serving:** Exported to `models/onnx/random_forest.onnx` with verified $|y_{sk} - y_{onnx}| < 10^{-5}$ precision.

### 2. Customer Segmentation (Gower Dissimilarity & 10 Personas)
- **Behavioral Features:** Customer LTV, AOV, Purchase Frequency, Spending Volatility, and Preferred Category.
- **Hierarchical Clustering:** Complete-linkage clustering on Gower dissimilarity cut at $k=10$ personas (e.g. Young Tech Shoppers $\le 50$, Married Female Home Buyers, High-Value Veterans).
- **Persisted to Warehouse:** Saved to PostgreSQL `customer_segments` for instant API retrieval.

### 3. Market Basket, Product Centrality & Item2Vec Embeddings
- **Apriori Association Rules:** Mined rules with support $\ge 0.05$ and confidence $\ge 0.40$.
- **Graph Centrality:** Product network graph with **PageRank**, **Hub Scores**, and **Authority Scores** (`networkx`) stored in `product_network_metrics`.
- **Item2Vec Embeddings:** Dense 32-dimensional Word2Vec skip-gram embeddings learned directly from co-purchased customer baskets for real-time item similarity recommendation.

### 4. Interactive Statistical Hypothesis Testing
- **Welch's Two-Sample t-Test (`Purchase ~ Gender`):** $t = -46.36, p < 2.2 \times 10^{-16}$. Confirms statistically significant male spending dominance ($9,437.53 vs. $8,734.57).
- **One-Way ANOVA (`Purchase ~ Age Groups`):** $F = 40.58, p < 2 \times 10^{-16}$. Replicates exact R analysis proving age bracket significantly influences basket value.

---

## 🛠️ Developer & MLOps Suite

| Capability | Module / Tool | Purpose & Details |
| :--- | :--- | :--- |
| **Data Contracts** | `src/features/data_contract.py` | [Pandera](https://pandera.readthedocs.io/) schema contracts validating incoming and cleaned DataFrames with strict type, range, and categorical checks. |
| **Model Explainability** | `src/tracking/explainability.py` | [SHAP](https://shap.readthedocs.io/) `TreeExplainer` generating beeswarm and bar summary plots logged as artifacts directly into MLflow runs. |
| **Drift Detection** | `src/tracking/drift_monitor.py` | [Evidently AI](https://www.evidentlyai.com/) automated data and target drift tests comparing holdout test batches to the baseline reference distribution. |
| **Model Governance** | `src/tracking/model_card.py` & [`MODEL_CARD.md`](MODEL_CARD.md) | Standardized Model Card generation including demographic subgroup fairness checks across Gender and Age slices. |
| **Champion vs Challenger** | `src/pipelines/retrain.py` | Automated model gatekeeper comparing newly trained models against the active MLflow Champion ($R^2$ threshold $+0.5\%$) before promoting to `Production`. |
| **Pipeline Reproducibility**| `dvc.yaml` | DVC multi-stage pipeline definition tracking dependencies, outputs, and model weights across 5 automated stages. |
| **Developer Automation** | `Makefile` | One-liner command shortcuts for Docker orchestration, pipeline execution, linting, testing, and formatting. |

---

## 📁 Repository Structure

```
Black-Friday/
├── .github/
│   └── workflows/
│       └── ci-cd.yml                # Automated Linting, Pytest & Docker Verification
├── docker/
│   ├── Dockerfile.api               # FastAPI container (Python 3.11 + ONNX Runtime)
│   ├── Dockerfile.ui                # Streamlit analytical product container
│   ├── mlflow/
│   │   └── Dockerfile               # MLflow server with Postgres & S3 MinIO drivers
│   └── postgres/
│       ├── init-multiple-dbs.sh     # Dual DB initialization (fridayblack + mlflow)
│       └── init_schema.sql          # Analytical warehouse schema, tables & indexes
├── docker-compose.yml               # Multi-container service composition
├── experiments/                     # Self-contained Jupyter research notebooks
│   ├── 01_data_preprocessing_imputation.ipynb
│   ├── 02_eda_and_hypothesis_testing.ipynb
│   ├── 03_regression_and_outliers.ipynb
│   ├── 04_customer_segmentation_gower.ipynb
│   └── 05_market_basket_and_network_analysis.ipynb
├── models/
│   ├── onnx/                        # High-throughput ONNX model artifacts
│   └── metadata.json                # Model metrics, scalers, and hyperparameters
├── src/
│   ├── core/                        # Configuration (Pydantic Settings) & Structured Logging
│   │   ├── config.py
│   │   └── logging.py
│   ├── data/                        # Pure SQL repository & SQLAlchemy connection pool
│   │   ├── db_connection.py
│   │   └── repository.py
│   ├── features/                    # Preprocessing, missForest imputer, encoders, Pandera
│   │   ├── data_contract.py
│   │   ├── imputation.py
│   │   ├── preprocessor.py
│   │   ├── customer_features.py
│   │   └── basket_encoder.py
│   ├── models/                      # Agnostic registry, Sklearn + LightGBM, ONNX exporter
│   │   ├── base.py
│   │   ├── regression.py
│   │   ├── lightgbm_model.py
│   │   ├── registry.py
│   │   ├── evaluate.py
│   │   └── onnx_exporter.py
│   ├── segmentation/                # Gower hierarchical clustering engine
│   │   └── clustering.py
│   ├── market_basket/               # Apriori, NetworkX graph & Item2Vec embeddings
│   │   ├── apriori_engine.py
│   │   ├── network_graph.py
│   │   └── item2vec.py
│   ├── tracking/                    # MLflow, SHAP, Evidently drift & Model Cards
│   │   ├── mlflow_tracker.py
│   │   ├── explainability.py
│   │   ├── drift_monitor.py
│   │   └── model_card.py
│   ├── pipelines/                   # CLI execution stages & Champion promotion
│   │   ├── ingest.py
│   │   ├── preprocess.py
│   │   ├── segmentation.py
│   │   ├── market_basket.py
│   │   ├── train.py
│   │   └── retrain.py
│   ├── api/                         # FastAPI application & endpoints
│   │   ├── main.py
│   │   ├── schemas.py
│   │   └── routes/
│   │       ├── regression.py
│   │       ├── segmentation.py
│   │       ├── market_basket.py
│   │       └── analytics.py
│   └── ui/                          # Streamlit analytical product
│       └── app.py
├── tests/                           # Pytest unit & integration test suite
│   ├── test_features.py
│   ├── test_models.py
│   └── test_api.py
├── dvc.yaml                         # DVC pipeline stages
├── Makefile                         # Unified CLI task runner
├── MODEL_CARD.md                    # ML Governance & demographic fairness card
├── requirements.txt                 # Project dependencies
└── README.md
```

---

## 🚀 Quick Start Guide

### 1. Launch All Services with Docker
```bash
# Using Makefile shortcut:
make up

# Or directly with Docker Compose:
docker-compose up -d --build
```

### 2. Run Data & Training Pipelines
```bash
# Ingest raw CSV data into PostgreSQL warehouse:
make ingest

# Run missing value imputation, outlier filtering & feature engineering:
make preprocess

# Compute customer personas (Gower clustering):
make segmentation

# Run Apriori mining, network centrality & Item2Vec:
make basket

# Train model, run 10-fold CV, export to ONNX & log to MLflow:
make train

# Run automated Champion vs Challenger promotion:
make retrain
```

### 3. Run Test Suite & Linters
```bash
# Run Pytest suite:
make test

# Format and lint code:
make lint
make format
```

---

## 🌐 Endpoints & Dashboards

| Service | URL | Credentials / Notes |
| :--- | :--- | :--- |
| **Streamlit Analytical Dashboard** | [http://localhost:8501](http://localhost:8501) | End-user analytical interface consuming FastAPI |
| **FastAPI Swagger UI** | [http://localhost:8000/docs](http://localhost:8000/docs) | Interactive API documentation |
| **MLflow Experiment Tracking** | [http://localhost:5000](http://localhost:5000) | Metrics, parameters, SHAP plots, and Model Registry |
| **MinIO S3 Console** | [http://localhost:9001](http://localhost:9001) | User: `admin` \| Password: `password123` |
| **PostgreSQL 16 Warehouse** | `localhost:5432` | DB: `fridayblack` / `mlflow` \| User: `postgres` |

---

## 👤 Author

**Ziad Ashraf**
- GitHub: [@Ziadashraf301](https://github.com/Ziadashraf301)
- Project: [Black-Friday](https://github.com/Ziadashraf301/Black-Friday)