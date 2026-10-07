# Project Memory Snapshot & State Consolidation

**Project**: Black Friday Conversational AI Assistant  
**Milestone**: Phase 2 Completed & Verified (100%) | Phase 3 Planned & Ready  
**Date**: October 5, 2026  

---

## 1. Phase 2 Architecture & Delivered Components

Phase 2 established the ground truth evaluation suite, the Tier-0 instant regex defense, and the System-1 Guardrail & Intent Router powered by TypeSafe AI Jev API (`typesafe-sdk`) and local deterministic fallback.

```
                           [ Incoming User Query ]
                                      │
                                      ▼
             ┌──────────────────────────────────────────────────┐
             │       Tier 0: Pre-Flight Lexical Engine          │
             │   - Rapid regex sanitization (<0.1ms)            │
             │   - Entity Extractor (Budgets, Sizes, P00... IDs)│
             └────────────────────────┬─────────────────────────┘
                                      │
                                      ▼
             ┌──────────────────────────────────────────────────┐
             │   Tier 1: System-1 Guardrail & Intent Router     │
             │             (Jev API Strategy)                   │
             │                                                  │
             │  TypeSafeClient.system_one(                      │
             │    questions={                                   │
             │      "is_adversarial": Noul(...),                │
             │      "intent": Choice(criteria=8_CANONICAL_MAP)  │
             │    }                                             │
             │  )                                               │
             └──────────┬───────────────────────────┬───────────┘
                        │                           │
          is_adversarial >= 0.80                    │ Safe & Classified
                        │                           │
                        ▼                           ▼
             ┌─────────────────────┐     ┌──────────────────────┐
             │ Adversarial Refusal │     │ Typed RoutingDecision│
             │   & Audit Security  │     │ - Target Intent      │
             │       Logging       │     │ - Extracted Entities │
             │  (Safe=False, P=0.99│     │ - Calibrated Conf.   │
             └─────────────────────┘     │ - Domain Steering    │
                                         └──────────────────────┘
```

### A. Ground-Truth Golden Benchmark Dataset (40 Cases)
* **File**: `data/golden_benchmark_dataset.json`
* **Coverage**: Exactly 40 evaluation cases across 8 core categories (5 each), strictly grounded in the 50 catalog products from `data/curated_products.json`:
  1. `CAT-01` (`PRODUCT_SEARCH`): Multi-attribute discovery (e.g., *"Looking for a warm winter jacket under $100 in size L"*).
  2. `CAT-02` (`PRODUCT_DETAILS`): Materials, specs, fabrics (e.g., *"Is the Artisan Paisley Silk Kimono Shirt P00025442 made of 100% pure silk?"*).
  3. `CAT-03` (`DEALS_PROMOTIONS`): Discounts, sales, clearance (e.g., *"Show me the biggest Black Friday discounts on sale right now"*).
  4. `CAT-04` (`BUNDLE_RECOMMENDATIONS`): Apriori association bundles & Item2Vec pairings (e.g., *"What jackets and boots pair well with the Artisan Silk Kimono Shirt P00025442?"*).
  5. `CAT-05` (`CART_ACTIONS`): Cart additions, size changes, checkout (e.g., *"Add the Artisan Paisley Silk Kimono in size L to my cart"*).
  6. `CAT-06` (`ORDER_SUPPORT`): Returns, shipping, tracking (e.g., *"What is the return policy for Black Friday promotional sale items?"*).
  7. `CAT-07` (`OUT_OF_DOMAIN`): Off-topic trivia, weather, programming, sports (e.g., *"What is the capital city of Australia?"*).
  8. `CAT-08` (`ADVERSARIAL_BLOCKED`): Jailbreaks, DAN triggers, prompt leakage, SQL injections, script attacks.

### B. Adversarial Safety & Prompt Injection Guardrail Engine
* **File**: `core/ai/guardrails/safety_engine.py`
* **Architecture**: Chain of Responsibility & Strategy Design Patterns:
  * **Tier 0 (`RegexPreFilter`)**: Intercepts known attacks (DAN, system overrides, SQLi tautologies/DDL, XSS) in **< 0.1ms** on local CPU.
  * **Tier 1 (`JevSafetyFilter`)**: Evaluates deep semantic prompt injections via TypeSafe AI `Noul` with RLCD-calibrated probability.
  * **Threshold Refusal**: Hard blocks queries with `adversarial_probability >= 0.80` returning standard security refusal message without calling expensive generative models.

### C. Entity & Shopping Constraint Extractor
* **File**: `core/ai/router/entity_extractor.py`
* **Strategy Implementations**:
  * `RegexEntityExtractor`: Sub-millisecond parsing of price limits (`<$100`, `under 50`), apparel sizes (`XS`–`XXL`), waist sizes (`30`–`38`), shoe sizes (`7`–`12`), catalog product IDs (`P00025442`), and 17 catalog category keywords.
  * `JevEntityExtractor`: Semantic classification of target apparel departments and price sensitivity.
  * `HybridEntityExtractor`: Production default combining regex precision with semantic fallback.

### D. LLMOps Prompt Versioning & Question Registry
* **File**: `core/ai/router/prompt_registry.py`
* **Features**: Semantic prompt versioning (`v1.0.0`), reproducible schema definitions for Jev questions, criteria maps, and author metadata for A/B testing and audit logging.

### E. System-1 Intent Classifier & Domain Steering
* **File**: `core/ai/router/intent_classifier.py`
* **Features**: Canonical enum mapping (`IntentType`), fast rule classification, and high-touch conversational steering responses for out-of-domain queries (politely redirecting weather, coding, or trivia requests back to Black Friday fashion deals).

### F. System-1 Router Core Architecture
* **File**: `core/ai/router/intent_router.py`
* **Design Patterns**: Strategy & Factory Patterns:
  * `BaseRouterStrategy`: Abstract Strategy interface returning typed `RoutingDecision`.
  * `JevApiRouter`: Primary production strategy using TypeSafe AI Jev System-1 API.
  * `FastRuleRouter`: Deterministic local CPU strategy for offline reproducibility and ultra-low latency (<1ms).
  * `RouterFactory`: Injects dependencies and API keys dynamically.

### G. Service Layer Facade
* **File**: `apps/api/services/guardrail_service.py`
* **Features**: Exposes `evaluate_query(query, user_id, session_id)` facade coordinating safety checking, System-1 routing, entity extraction, and audit trails.

---

## 2. MLflow Experiment & Benchmark Results

* **Benchmark Harness**: `evaluation/ai/benchmark_router.py`
* **MLflow Tracking URI**: `http://localhost:5000`
* **Experiment Name**: `black-friday-system1-router-benchmark`

### Benchmark Metric Scorecard:
| Metric | JevApiRouter (Production) | FastRuleRouter (Local Fallback) | Target SLA / Standard | Status |
| :--- | :--- | :--- | :--- | :--- |
| **Intent Classification Accuracy** | **100.0%** (40/40) | 97.5% | >= 90.0% | **PASSED** |
| **Intent Macro-F1** | **1.000** | N/A | >= 0.850 | **PASSED** |
| **Adversarial Detection Recall** | **100.0%** (5/5) | 100.0% | 100.0% | **PASSED** |
| **Adversarial Precision** | **100.0%** | 100.0% | >= 95.0% | **PASSED** |
| **Brier Score (Calibration)** | **0.0001** | N/A | < 0.050 | **PASSED** |
| **Latency p50 (Median)** | 888.2 ms | **0.13 ms** | < 10ms (Local) / < 1s (API) | **PASSED** |
| **Latency p95** | 1427.5 ms | **0.60 ms** | < 10ms SLA (CPU) | **PASSED** |

### Entity Extractor Benchmark:
| Extractor Strategy | Accuracy Score | Latency Profile | Architectural Role |
| :--- | :--- | :--- | :--- |
| **RegexEntityExtractor (Tier 0)** | **91.0%** (0.9095) | < 0.5 ms | Fast deterministic parsing of explicit numbers, sizes, and catalog codes |
| **JevEntityExtractor (Tier 1)** | **85.7%** (0.8571) | ~500 ms | Standalone semantic department, budget tier, and size classification |
| **HybridEntityExtractor (Combined)**| **96.2%** (0.9619) | < 1 ms (with fallback) | Production ensemble merging exact lexical precision with semantic grounding |

### Generated & Logged Artifacts:
1. `evaluation/ai/artifacts/confusion_matrix.png`: Heatmap of the 8 canonical intents.
2. `evaluation/ai/artifacts/latency_distribution.png`: Boxplot and histogram of execution latencies.
3. `evaluation/ai/artifacts/calibration_curve.png`: Reliability curve demonstrating RLCD calibration.
4. `evaluation/ai/artifacts/extractor_comparison.png`: Accuracy comparison of Regex, Jev, and Hybrid extractors.
5. `evaluation/ai/artifacts/benchmark_results.csv`: Case-by-case evaluation ledger.
6. `evaluation/ai/artifacts/benchmark_report.md`: Markdown summary.

---

## 3. Test Suite Verification

### Phase 2 Test Suite (`tests/test_phase2_jev_router.py`):
```text
tests/test_phase2_jev_router.py::test_golden_dataset_structure PASSED    [ 12%]
tests/test_phase2_jev_router.py::test_adversarial_guardrail_blocking PASSED [ 25%]
tests/test_phase2_jev_router.py::test_entity_extraction PASSED           [ 37%]
tests/test_phase2_jev_router.py::test_router_strategy_pattern PASSED     [ 50%]
tests/test_phase2_jev_router.py::test_intent_classification_accuracy PASSED [ 62%]
tests/test_phase2_jev_router.py::test_guardrail_service_orchestration PASSED [ 75%]
tests/test_phase2_jev_router.py::test_router_latency_under_10ms PASSED   [ 87%]
tests/test_phase2_jev_router.py::test_full_40_case_golden_benchmark PASSED [100%]
============================== 8 passed in 6.17s ==============================
```

### Phase 1 Regression Suite (`tests/test_phase1_infra_frontend.py`):
```text
tests/test_phase1_infra_frontend.py::test_pgvector_schema PASSED         [ 14%]
tests/test_phase1_infra_frontend.py::test_existing_20_products_audited PASSED [ 28%]
tests/test_phase1_infra_frontend.py::test_curated_catalog_30_items PASSED [ 42%]
tests/test_phase1_infra_frontend.py::test_product_image_paths_exist PASSED [ 57%]
tests/test_phase1_infra_frontend.py::test_redis_rate_limit_5_min_20_day PASSED [ 71%]
tests/test_phase1_infra_frontend.py::test_frontend_bot_auth_check PASSED [ 85%]
tests/test_phase1_infra_frontend.py::test_catalog_embeddings_incremental_skip PASSED [100%]
============================== 7 passed in 4.75s ==============================
```

---

## 4. Phase 3 Specifications: LangGraph Agent, Granular Tools, Memory & Tracing

* **Next Milestone**: Phase 3
* **Core Objectives**:
  1. **Granular Tools**:
     - `search_products(category, max_price, size, query)` (Hybrid BM25 + pgvector HNSW)
     - `get_product_details(product_id)`
     - `get_bundle_recommendations(product_id)` (Apriori & Item2Vec)
     - `modify_cart(user_id, action, product_id, size, quantity)`
     - `get_order_status(order_id)`
  2. **LangGraph StateGraph Workflow**:
     - State: `messages`, `intent`, `entities`, `user_profile`, `cart`, `retrieved_docs`.
     - Primary Generator: `gemini-3.1-flash-lite`.
  3. **Multi-Tier Memory Architecture**:
     - Short-term: Redis sliding conversation checkpointer.
     - Long-term: User profile preference vector store in PostgreSQL.
  4. **MLflow Tracing**:
     - Distributed span tracing across router, tool calls, and LLM generation.
