# Project Memory Snapshot & State Consolidation

**Project**: Black Friday Conversational AI Assistant  
**Current Milestone**: Phase 1 Completed & Verified (100%) | Phase 2 Planned & Ready  
**Date**: October 2, 2026  

---

## 1. Phase 1 Architecture & Delivered Components

### A. Database & Hybrid Vector Schema
* **Engine**: PostgreSQL 16 + `pgvector` v0.8.6 running in Docker container `blackfriday_postgres`.
* **Hybrid Storage on `curated_products`**:
  * `embedding vector(768)`: Dense semantic vectors indexed via HNSW (`idx_curated_embedding_hnsw`).
  * `search_vector tsvector`: Sparse English keyword index with token positions for BM25 ranking, indexed via GIN (`idx_curated_search_vector`).
* **Automated Verification**: Handled in `core/db/repositories/warehouse_repo.py` via `enable_pgvector_extension()`.

### B. Catalog Scope (50 Products)
* **File**: `data/curated_products.json` contains exactly **50 verified products**:
  * Products 1–20: Audited baseline curated products.
  * Products 21–23: Corrected against warehouse network metrics (`P00278642`, `P00242742`, `P00034742`).
  * Products 24–30: Selected diverse items (`P00148642`, `P00080342`, `P00031042`, `P00028842`, `P00251242`, `P00114942`, `P00270942`).
  * Products 31–50: Expanded items (`P00000142`, `P00112542`, `P00044442`, `P00334242`, `P00111142`, `P00277642`, `P00052842`, `P00116842`, `P00005042`, `P00086442`, `P00258742`, `P00085942`, `P00216342`, `P00073842`, `P00128942`, `P00113242`, `P00112442`, `P00105142`, `P0097242`, `P00147942`).
* **Attributes**: Pricing (original, discounted), sizes, real Apriori association bundles (lift, confidence), and Item2Vec similar products populated from warehouse metrics.
* **Hero Flag**: Exclusively set to `is_hero: true` for the flagship product `P00025442` (Artisan Paisley Silk Kimono Shirt); all other 49 products have `is_hero: false`.

### C. Embedding & Ingestion Strategy
* **Decoupled Architecture (`core/ai/embedding_service.py`)**:
  * `BaseEmbeddingProvider` (Abstract strategy interface)
  * `GeminiEmbeddingProvider` (`models/gemini-embedding-2`, 768-dim with Matryoshka Representation Learning dimension reduction and L2 normalization)
  * `DeterministicSemanticProvider` (Zero-failure offline fallback)
* **Incremental Seeding Pipeline (`ml/pipelines/seed_curated.py`)**:
  * Seeds all 50 items into PostgreSQL.
  * Skips items with existing non-null embeddings (`WHERE embedding IS NULL`).
  * Database state: 50 total products, 50 non-null 768-dim vector embeddings.

### D. Image Asset Coverage
* All 50 products have valid high-resolution image assets in both:
  * `apps/reflex_app/assets/products/`
  * `apps/reflex_app/.web/public/products/`
* Zero broken images / 404s in the web UI.

### E. Security & Rate Limiting
* **Redis Sliding-Window Rate Limiter**:
  * Enforces **5 requests/minute** and **20 requests/day** per authenticated `user_id`.
  * Returns HTTP 429 when quota exceeded.
  * Handled via `apps/api/rate_limiting/rate_limiter.py` and `apps/api/services/rate_limiter_service.py`.

### F. Frontend Reflex Application
* **Bot Drawer Component (`apps/reflex_app/reflex_app/components/bot_drawer.py`)**:
  * Floating trigger button (`bot_trigger_button`).
  * JWT auth gate (`ShoppingState.is_authenticated`).
  * Preset action chips (*"Top Deals Today"*, *"Sale Products"*, *"Under $50"*, *"Style Advisor"*).
* **Grid Display**: `filtered_products` in `state.py` dynamically displays all **49 non-hero products in the center 2x2 grid**, while the **1 active hero product** is showcased in the featured Hero Card on the right.

### G. Test Suite
* `tests/test_phase1_infra_frontend.py`: **7/7 tests passed (100%)**.

---

## 2. Phase 2 Specifications: Golden Benchmark & System-1 Guardrail Router

* **Excel Roadmap**: `Phase2_Implementation_Plan.xlsx`
* **Implementation Document**: `implementation_plan.md`

### Sequential Tasks:
1. **P2-01**: Benchmark Taxonomy & 40 Golden Test Cases (`data/golden_benchmark_dataset.json`).
2. **P2-02**: Adversarial Safety & Injection Guardrail Engine (`core/ai/guardrails/safety_engine.py`).
3. **P2-03**: Entity & Constraint Extractor (`core/ai/router/entity_extractor.py`).
4. **P2-04**: System-1 Router Core Architecture Strategy Pattern (`core/ai/router/intent_router.py`).
5. **P2-05**: Fast Intent Classifier & Domain Steering (`core/ai/router/intent_classifier.py`).
6. **P2-06**: Guardrail Service Layer & Router Orchestrator (`apps/api/services/guardrail_service.py`).
7. **P2-07**: Sub-10ms Latency SLA Optimization & Benchmark Harness (`evaluation/ai/benchmark_router.py`).
8. **P2-08**: Phase 2 Automated Pytest Suite (`tests/test_phase2_jev_router.py`).

---

## 3. Active Docker & Infrastructure State

| Service | Container Name | Host Port | Status | Role |
| :--- | :--- | :--- | :--- | :--- |
| **PostgreSQL 16** | `blackfriday_postgres` | `5432` | Healthy | Analytical warehouse, pgvector hybrid catalog |
| **Redis 7** | `blackfriday_redis` | `6379` | Healthy | Rate limiting & session cache |
| **MinIO** | `blackfriday_minio` | `9000`, `9001` | Running | S3-compatible artifact store |
| **MLflow** | `blackfriday_mlflow` | `5000` | Running | Experiment tracking server |
