# Work Package 7 Fix Report: AI Pipeline, UI Schema, Logging, and Guardrails

## Summary of Findings and Fix Status

| Fix ID | Topic | Status | Files Changed | Evidence |
|---|---|---|---|---|
| **5.6, 6.4, 10.2** | UI Payload contract & markdown template separation | **DONE** | `ai/workflow/state.py`, `ai/services/template_service.py`, `ai/services/synthesis_service.py`, `ai/nodes/aggregator_node.py`, `ai/nodes/bundle_node.py`, `ai/nodes/cart_node.py`, `ai/nodes/details_node.py`, `ai/nodes/search_node.py`, `ai/nodes/support_node.py` | `tests/test_fix_wp7_ui_payload.py` (4 passed), `tests/test_phase3_langgraph_agent.py` (9 passed) |
| **6.3, 7.3, 8.2** | Standardized `core.logging` (eliminated loguru) | **DONE** | `ai/nodes/bundle_node.py`, `ai/nodes/cart_node.py`, `ai/nodes/support_node.py`, `requirements.txt` | `tests/test_fix_wp7_logging.py` (3 passed), codebase grep confirmed 0 loguru references in `ai/` |
| **7.4** | Hybrid entity extraction sizing deduplication | **DONE** | `ai/extractor/hybrid_extractor.py`, `ai/schemas/router.py` | `tests/test_fix_wp7_extractor.py` (3 passed), duplicate regex parsing removed |
| **9.5** | Modern Google GenAI SDK migration | **DONE** | `core/embeddings/service.py`, `requirements.txt` | `tests/test_fix_wp7_embeddings.py` (3 passed), `google.genai.Client` with fallback verified |
| **9.6** | Dynamic Policy KB hot-reload on mtime change | **DONE** | `ai/tools/policy_kb.py` | `tests/test_fix_wp7_policy_kb.py` (1 passed) |
| **8.5** | Guardrail & benchmark test segregation | **DONE** | `pytest.ini`, `tests/test_phase2_jev_router.py` | `tests/test_fix_wp7_jev_router_benchmark.py` (2 passed), `tests/test_phase2_jev_router.py` (10 passed) |

---

## Detailed Findings and Verification

### 1. Fix 5.6, 6.4, 10.2: Unified UI Payload & Template Separation
- **Finding**: Specialist nodes constructed unstructured ad-hoc dictionaries or plain strings without typed contract (`UICard`, `GroundingCitation`, `UIPayload`). Markdown templating in `SynthesisService` mixed rendering presentation directly with synthesis logic. Specialist cards were not merged properly when parallel specialists returned results.
- **Solution**:
  - Defined typed `UICard`, `GroundingCitation`, and `UIPayload` in `ai/workflow/state.py`.
  - Added `ui_payload_reducer` to merge cards, action chips, and citations across specialist nodes.
  - Extracted deterministic markdown formatting into `ai/services/template_service.py` (`ResponseTemplateService`).
  - Updated `SynthesisService` to preserve specialist cards and action chips in `UIPayload` and inject template rendering.
  - Updated specialist nodes (`search_node`, `details_node`, `bundle_node`, `cart_node`, `support_node`, `aggregator_node`) to emit typed cards and chips in `state.ui_payload`.
- **Verification**:
  - `pytest tests/test_fix_wp7_ui_payload.py` -> 4 passed.
  - `pytest tests/test_phase3_langgraph_agent.py` -> 9 passed (resolving prior baseline failures).

### 2. Fix 6.3, 7.3, 8.2: Logging Standardization
- **Finding**: Modules in `ai/nodes/` (`bundle_node.py`, `cart_node.py`, `support_node.py`) were directly importing `from loguru import logger`, violating logging consistency and introducing unnecessary dependencies.
- **Solution**:
  - Replaced all `from loguru import logger` with `from core.logging import get_logger; logger = get_logger(__name__)`.
  - Removed `loguru>=0.7.3` from `requirements.txt`.
- **Verification**:
  - `pytest tests/test_fix_wp7_logging.py` -> 3 passed.
  - Confirmed 0 occurrences of `loguru` in `ai/`.

### 3. Fix 7.4: Entity Extractor Size Parsing Deduplication
- **Finding**: `HybridEntityExtractor` in `ai/extractor/hybrid_extractor.py` duplicated shoe, waist, and word size regexes that `RegexEntityExtractor` had already parsed.
- **Solution**:
  - Removed lines 33-60 redundant sizing regex block from `HybridEntityExtractor`.
  - Forwarded `reg_res.sizes` directly to `ExtractedEntities`.
  - Added missing `min_price: Optional[float]` and `styles: List[str]` fields to `ai/schemas/router.py`.
- **Verification**:
  - `pytest tests/test_fix_wp7_extractor.py` -> 3 passed.

### 4. Fix 9.5: Google GenAI SDK Migration
- **Finding**: `core/embeddings/service.py` utilized deprecated `google.generativeai` instead of `google.genai` SDK.
- **Solution**:
  - Migrated `GeminiEmbeddingProvider` to use `from google.genai import Client` and `client.models.embed_content`.
  - Supported dependency injection of client for zero-network testing and automatic fallback to `DeterministicSemanticProvider`.
  - Replaced `google-generativeai>=0.8.0` with `google-genai>=2.0.0` in `requirements.txt`.
- **Verification**:
  - `pytest tests/test_fix_wp7_embeddings.py` -> 3 passed.

### 5. Fix 9.6: Policy KB Dynamic Reload
- **Finding**: `PolicyKnowledgeBase` in `ai/tools/policy_kb.py` cached policy json in memory upon initialization and required process restart to reflect policy edits.
- **Solution**:
  - Tracked file `os.path.getmtime` on policy store.
  - Implemented `_check_reload()` before policy queries, reloading policies on file modification without process restart.
- **Verification**:
  - `pytest tests/test_fix_wp7_policy_kb.py` -> 1 passed.

### 6. Fix 8.5: Guardrail & Benchmark Test Separation
- **Finding**: Router timing tests coupled latency benchmarking with routing correctness checks, causing flaky test failures under CI or high load.
- **Solution**:
  - Registered `benchmark` custom marker in `pytest.ini`.
  - Split `test_phase2_jev_router.py` into pure routing correctness vs latency benchmark tests.
  - Decorated timing assertions with `@pytest.mark.benchmark` using generous 50ms/100ms thresholds.
- **Verification**:
  - `pytest tests/test_phase2_jev_router.py` -> 10 passed.
  - `pytest tests/test_fix_wp7_jev_router_benchmark.py` -> 2 passed.

---

## Test Suite Execution Summary
- `tests/test_fix_wp7_*.py`: 16 passed
- `tests/test_phase2_jev_router.py`: 10 passed
- `tests/test_phase3_langgraph_agent.py`: 9 passed
- `tests/test_phase4_*.py`: 16 passed (bundles, cart durability, search relaxation, security strikes, tracing)
- `tests/test_architecture.py`: 1 passed (zero layering violations)

## Noticed but Not Fixed
- `evaluation/ai/` contains legacy scripts that reference older router interfaces, scheduled for clean-up in evaluation overhaul.
