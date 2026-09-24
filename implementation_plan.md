# Black Friday v2 (Python Architecture) - Implementation Plan

## 1. Executive Summary & Vision
The goal of this initiative is to re-architect the Black Friday statistical analysis and machine learning project from R into a production-grade Python Data & ML Engineering ecosystem. The new architecture achieves strict algorithmic and statistical parity with the original R findings while introducing modern industry best practices:
- **Clean Architecture & SOLID Principles** across all data, features, and model modules.
- **Unified Infrastructure:** Single PostgreSQL 16 instance hosting both the analytical warehouse (`fridayblack`) and MLflow tracking store (`mlflow`), combined with MinIO for S3-compatible artifact storage.
- **Reproducible ML Pipeline:** Tracked with **DVC** (data/pipeline versioning) and **MLflow** (experiment tracking & model registry).
- **High-Performance Serving:** High-throughput **ONNX Runtime** inference embedded in a RESTful **FastAPI** service.
- **Rich Analytical Product:** An interactive **Streamlit Dashboard** visualizing real-time EDA, live hypothesis testing, ONNX purchase price estimation, customer segmentation profiling (10 personas), and product network graph analysis (PageRank & HITS).
- **DevOps & QA:** Complete containerization via **Docker Compose**, automated unit/integration testing with **Pytest**, and automated linting/testing in **GitHub Actions CI/CD**.

---

## 2. Infrastructure Architecture & Service Topology

```
+---------------------------------------------------------------------------------------------------+
|                                      DOCKER NETWORK: blackfriday-net                              |
|                                                                                                   |
|  +--------------------+    +--------------------+    +---------------------+    +---------------+  |
|  |     PostgreSQL 16  |    |     MinIO (S3)     |    |    MLflow Server    |    |   MinIO-Init  |  |
|  | Port: 5432         |    | Ports: 9000 / 9001 |    | Port: 5000          |    | (Bucket auto- |  |
|  | - DB: fridayblack  |    | - mlflow-artifacts |    | - Backend: Postgres |    |  provision)   |  |
|  | - DB: mlflow       |    | - dvc-storage      |    | - Artifacts: MinIO  |    +---------------+  |
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

## 3. Implementation Roadmap: Phases, Sprints & Milestones

```mermaid
gantt
    title Black Friday v2 Migration Roadmap
    dateFormat  YYYY-MM-DD
    section Phase 1: Foundation & Data Engineering
    Sprint 1.1 Docker & Postgres Dual DB Setup       :done, s11, 2026-09-25, 2d
    Sprint 1.2 DVC & Ingestion Engine                :active, s12, after s11, 2d
    Sprint 1.3 Preprocessing & Imputation Pipeline   :s13, after s12, 3d
    section Phase 2: Feature Engineering & Segmentation
    Sprint 2.1 Customer RFM & Behavioral Features   :s21, after s13, 2d
    Sprint 2.2 Gower Distance & Hierarchical Cluster:s22, after s21, 3d
    Sprint 2.3 Market Basket & Network Graph Engine  :s23, after s22, 3d
    section Phase 3: ML Modeling, MLflow & ONNX
    Sprint 3.1 Outlier IQR, Normalization & Split    :s31, after s23, 2d
    Sprint 3.2 Regression Models & 10-Fold CV        :s32, after s31, 3d
    Sprint 3.3 MLflow Tracking & ONNX Export         :s33, after s32, 2d
    Sprint 3.4 Automated Retraining Engine           :s34, after s33, 2d
    section Phase 4: Jupyter Notebooks (Parity)
    Sprint 4.1 Port Notebooks 01 to 05               :s41, after s34, 4d
    section Phase 5: FastAPI Backend & Serving
    Sprint 5.1 REST Endpoints & Pydantic Schemas     :s51, after s41, 3d
    Sprint 5.2 ONNX Runtime Serving & SQL Repo       :s52, after s51, 2d
    section Phase 6: Streamlit Analytical Dashboard
    Sprint 6.1 Multi-Page UI & Executive EDA         :s61, after s52, 3d
    Sprint 6.2 ONNX Live Predictor & Comparison      :s62, after s61, 2d
    Sprint 6.3 Customer Persona Hub & Network Graph  :s63, after s62, 3d
    section Phase 7: Quality Assurance & CI/CD
    Sprint 7.1 Pytest Suite & Coverage               :s71, after s63, 2d
    Sprint 7.2 GitHub Actions CI/CD Pipeline         :s72, after s71, 2d
```

---

## 4. Detailed Sprints & Deliverables

### Phase 1: Infrastructure & Data Engineering
#### **Sprint 1.1: Docker & Dual PostgreSQL Infrastructure**
- **Milestone 1:** *Core storage and tracking services fully operational and interconnected.*
- Configure `docker-compose.yml` with:
  - `postgres:16` initializing two distinct databases (`fridayblack` and `mlflow`) using an entrypoint script `docker/postgres/init-multiple-dbs.sh`.
  - `minio` and `minio-init` provisioning buckets `mlflow-artifacts` and `blackfriday-dvc`.
  - `mlflow` server wired to Postgres (`postgresql://mlflow:mlflow123@postgres:5432/mlflow`) and MinIO (`s3://mlflow-artifacts/`).
- Create environment templates (`.env.example`) and configuration loaders (`src/core/config.py`).

#### **Sprint 1.2: Database Schema & Ingestion Pipeline**
- Design PostgreSQL analytical tables:
  1. `raw_black_friday` (raw staging).
  2. `black_friday_cleaned` (imputed transactions).
  3. `customer_segments` (customer-level aggregated metrics + cluster assignments).
  4. `product_network_metrics` (PageRank, Hubs, Authority, top associates).
- Build `src/data/db_connection.py` using SQLAlchemy connection engine pooling.
- Build `src/data/repository.py` utilizing pure parameterized SQL for batch loading and high-speed streaming.
- Implement `src/pipelines/ingest.py` to ingest raw `train.csv` and `test.csv` into Postgres with binary streaming.

#### **Sprint 1.3: Data Preprocessing & MissForest Imputation**
- Implement `src/features/imputation.py` using `sklearn.impute.IterativeImputer` with `ExtraTreesRegressor` (100 estimators, parallel execution) to mirror R's `missForest` behavior.
- Validate missing value imputation parity (~69% accuracy on `Product_Category_2` and `Product_Category_3`).
- Write imputed transactions into `black_friday_cleaned`.
- Set up DVC tracking (`dvc.yaml`) for raw and processed datasets.

---

### Phase 2: Feature Engineering, Customer Segmentation & Market Basket
#### **Sprint 2.1: Customer RFM & Behavioral Feature Engineering**
- Implement `src/features/customer_features.py`:
  - `Lifetime_Value`: $\sum \text{Purchase}$
  - `Average_Order_Value`: $\text{mean}(\text{Purchase})$
  - `Frequency`: distinct `Product_ID`
  - `Purchase_Amount_Variability`: $\max(\text{Purchase}) - \min(\text{Purchase})$
  - `Popular_Category`: mode of `Product_Category_1`
  - Demographic features: `Gender`, `Marital_Status`, binned `Age` (`<=50` vs `>51`).

#### **Sprint 2.2: Gower Distance & Hierarchical Clustering Engine**
- Implement `src/segmentation/clustering.py`:
  - Vectorized Gower distance matrix computation using the `gower` library.
  - Complete-linkage hierarchical clustering (`scipy.cluster.hierarchy`).
  - Cut dendrogram at $k=10$ distinct customer clusters.
  - Profile the 10 customer personas (demographics, spending tiers, marketing actions).
  - Persist segmentation results to PostgreSQL table `customer_segments`.

#### **Sprint 2.3: Market Basket Analysis & Product Network Graph**
- Implement `src/features/basket_encoder.py`:
  - User-basket transaction matrix generator (5,891 user baskets $\times$ 3,631 items).
- Implement `src/market_basket/apriori_engine.py`:
  - Association rule mining using `mlxtend.frequent_patterns.apriori` ($\text{supp} \ge 0.05, \text{conf} \ge 0.40, \text{min\_len} = 2$).
  - Stratified lift categorization (1-2, 2-3, 3-4, 4-5, 5-6, 6+).
- Implement `src/market_basket/network_graph.py`:
  - Directed graph construction using `networkx`.
  - Calculate **PageRank**, **HITS Hubs**, and **HITS Authorities** for each product node.
  - Persist network rankings to PostgreSQL table `product_network_metrics`.

---

### Phase 3: ML Modeling, MLflow Tracking & ONNX Deployment
#### **Sprint 3.1: Outlier Detection, Target Scaling & Splitting**
- Implement `src/features/preprocessor.py`:
  - Outlier detection: IQR filter ($Q1 - 1.5 \times \text{IQR}$ to $Q3 + 1.5 \times \text{IQR}$, threshold $\$21,400.50$, isolating Category 10).
  - Target normalization: $y = \text{Purchase} / \text{Purchase\_max}$ ($\text{Purchase\_max} = 21,399$).
  - Reproducible 90/10 train-test split (Seed 1234).

#### **Sprint 3.2: Regression Models & 10-Fold Cross-Validation**
- Implement `src/models/` with Strategy & Factory patterns:
  - `LinearRegression`: baseline (`Purchase ~ Product_Category_1`).
  - `DecisionTreeRegressor`: (`Purchase ~ Product_Category_1..3 + Product_ID`).
  - `RandomForestRegressor`: (`n_estimators=100`, `max_features=4`, parallelized).
- Implement 10-fold cross-validation engine tracking RMSE and $R^2$. Target parity:
  - Linear Model: CV RMSE $\approx 0.141$, $R^2 \approx 62.8\%$
  - Decision Tree: CV RMSE $\approx 0.132$, $R^2 \approx 67.1\%$
  - Random Forest: Test RMSE $\approx 0.116$, $R^2 \approx 74.6\%$

#### **Sprint 3.3: MLflow Experiment Tracking & ONNX Model Export**
- Implement `src/tracking/mlflow_tracker.py`:
  - Log parameters, cross-validation metrics, feature importance plots, and artifacts to MLflow.
- Implement `src/models/onnx_exporter.py`:
  - Convert trained scikit-learn models to ONNX (`skl2onnx`).
  - Save ONNX models to `models/onnx/` and log them into the MLflow Model Registry.
  - Perform automated parity verification: validate $|y_{\text{sklearn}} - y_{\text{onnx}}| < 10^{-5}$.

#### **Sprint 3.4: Automated Retraining Pipeline**
- Implement `src/pipelines/retrain.py`:
  - Ingests newly appended data from Postgres, validates schema drift, retrains the Random Forest, evaluates against champion metric thresholds, exports new ONNX artifacts, and registers updated models in MLflow.

---

### Phase 4: Jupyter Notebooks (Parity with Rmd)
#### **Sprint 4.1: Interactive Research & EDA Notebooks**
- Create 5 clean, self-contained, reproducible notebooks in `experiments/`:
  1. `01_data_preprocessing_imputation.ipynb`: Data inspection, missForest imputation parity, database ingestion.
  2. `02_eda_and_hypothesis_testing.ipynb`: 10 core questions, interactive visualizations, Welch's t-test, ANOVA test.
  3. `03_regression_and_outliers.ipynb`: Outlier analysis, linear regression, decision tree, random forest, 10-fold CV, MLflow run.
  4. `04_customer_segmentation_gower.ipynb`: RFM KPIs, Gower distance, dendrogram, $k=10$ cluster personas.
  5. `05_market_basket_and_network_analysis.ipynb`: Apriori association rules, NetworkX graph, PageRank, Hubs, and Authorities.

---

### Phase 5: FastAPI Production Serving
#### **Sprint 5.1: High-Performance RESTful API Architecture**
- Implement `src/api/main.py` with CORS, structured JSON logging, and lifecycle events.
- Implement strict Pydantic schemas in `src/api/schemas/` for input validation and output serialization.
- Implement dependency-injected PostgreSQL connection sessions and ONNX runtime inference sessions.

#### **Sprint 5.2: Endpoints Implementation**
- `/health`: System health, DB connection, and loaded ONNX models status.
- `/predict/purchase`: Single & batch purchase price prediction in USD using ONNX runtime.
- `/predict/cluster`: Customer segment assignment based on user behavioral features.
- `/analytics/eda`: Distribution statistics, top users, top products.
- `/analytics/hypothesis-test/gender`: Live Welch's t-test with custom $\alpha$.
- `/analytics/hypothesis-test/age`: Live One-Way ANOVA test.
- `/segmentation/personas`: Summary statistics and marketing strategies for all 10 clusters.
- `/market-basket/recommendations`: Cross-sell suggestions and bundle rules based on product IDs.
- `/market-basket/network`: Graph centrality metrics (PageRank, Hubs, Authorities).

---

### Phase 6: Streamlit Analytical Dashboard
#### **Sprint 6.1: Multi-Page UI & Executive EDA**
- Implement `src/ui/app.py` with modern, premium dark/light responsive layout.
- **Page 1: Executive Overview & EDA:**
  - Metric summary cards (Total Revenue, Transactions, Avg Basket Size).
  - Interactive Plotly visualizations for demographics, city categories, and occupations.
  - Interactive hypothesis testing cards with dynamic p-value interpretations.

#### **Sprint 6.2: Regression Explorer & ONNX Live Predictor**
- **Page 2: Pricing & Regression Hub:**
  - Model comparison tab: Scatter plot of Observed vs. Predicted with 45° reference line for $N \in [1, 3000]$ samples.
  - Interactive Live Estimator: Dropdowns for Category 1..3 and Product ID returning immediate USD predictions and confidence bounds.
  - Feature importance breakdown.

#### **Sprint 6.3: Customer Segmentation & Product Affinity Hub**
- **Page 3: Customer Segmentation Hub:**
  - Visual exploration of the 10 customer persona clusters.
  - Customer lookup tool by `User_ID` showing lifetime spend, frequency, and custom retention actions.
- **Page 4: Market Basket & Network Analysis:**
  - Association rule explorer with dynamic sliders for Support, Confidence, and Lift.
  - Product Network Graph visualization showing PageRank influence and hub-authority clusters.
  - Automated "Frequently Bought Together" bundle generator.

---

### Phase 7: Quality Assurance, DevOps & CI/CD
#### **Sprint 7.1: Pytest Test Suite**
- Write comprehensive tests in `tests/`:
  - `test_data_pipeline.py`: Database queries, repository methods, schema validation.
  - `test_features.py`: Imputer accuracy, scaler limits, RFM feature calculations.
  - `test_models.py`: Regression metric checks, 10-fold CV stability.
  - `test_onnx_inference.py`: Parity between scikit-learn and ONNX runtime.
  - `test_api.py`: FastAPI test client verifying all endpoints, status codes, and edge cases.

#### **Sprint 7.2: Containerization & CI/CD**
- Create production Dockerfiles:
  - `docker/Dockerfile.api`: Python 3.11-slim with `uv` fast package manager.
  - `docker/Dockerfile.ui`: Streamlit frontend.
- Configure `.github/workflows/ci-cd.yml`:
  - Step 1: Python environment setup with caching.
  - Step 2: Code quality and linting (`flake8`, `black`, `isort`).
  - Step 3: Run full `pytest` suite with coverage reporting.
  - Step 4: Docker Compose build and smoke testing.
