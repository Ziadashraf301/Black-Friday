# Work Package 3 (WP3) Fix Report: Redis Layer and Its Consumers

## 1. Initial Code Audit & Findings Confirmation
Before modifying code, `core/cache/redis_client.py`, `ai/guardrails/strike_tracker.py`, and `ai/tools/cart_tools.py` were audited:
- **`core/cache/redis_client.py` Public Methods in Baseline**:
  - `is_available` (property)
  - `get_json(key: str) -> Optional[Any]`
  - `set_json(key: str, value: Any, ttl: Optional[int] = None) -> bool`
  - `delete_key(key: str) -> bool`
  - `delete_pattern(pattern: str) -> int`
  - `mget_json(keys: List[str]) -> Dict[str, Any]`
  - `mset_json(key_values: Dict[str, Any], ttl: Optional[int] = None) -> bool`
  - *TTL parameter handling*: Passed as `ttl: Optional[int] = None`, defaulting to `settings.REDIS_DEFAULT_TTL` (21,600s / 6 hours).
- **Review Claims Confirmation**:
  - **CONFIRMED**: Baseline `ai/guardrails/strike_tracker.py` called `.get(ban_key)` and `.client`, neither of which existed on `RedisCacheManager`. The test `tests/test_phase4_security_strikes.py` passed only because errors were swallowed and it fell back to in-memory dictionaries.
  - **CONFIRMED**: Baseline `ai/tools/cart_tools.py` called `self._cache.get(key)` and `self._cache.set(key, payload, ttl=ttl)`, neither of which existed on `RedisCacheManager`.

---

## 2. Remediations Executed

### Fix 2.2: `core/cache/redis_client.py` Hardening
- **Status**: DONE
- **Changes**:
  - Added thread-safe public `client` property returning the underlying `redis.Redis` instance.
  - Implemented 15-second reconnection cooldown (`_last_failure_time` tracking with `threading.Lock`) so offline Redis calls return `False` immediately (<100ms, sub-millisecond) instead of blocking for 2-second socket timeouts.
  - Replaced blocking `keys()` in `delete_pattern()` with non-blocking `scan_iter()` executed in batches (default 500).
  - Added public `get(key)` and `set(key, value, ttl=None)` methods.
- **Verification**: `tests/test_fix_2_2_redis_client.py` (4/4 passed).

### Fix 3.7: `ai/guardrails/strike_tracker.py` Public API & Distributed Security
- **Status**: DONE
- **Changes**:
  - Consumes public `cache_manager.client` property and public `.get()` method.
  - Strikes increment in Redis under `security:strikes:<id>`, and lockout bans write to `security:banned:<id>` with TTL (default 86,400s / 24h, with `ban_ttl` override support for testing).
  - Strengthened `tests/test_phase4_security_strikes.py` to assert that keys and TTLs actually persist in real Redis rather than only passing on in-memory fallback.
  - Robust in-memory fallback maintained when Redis is offline.
- **Verification**: `tests/test_fix_3_7_strike_tracker.py` (3/3 passed) and `tests/test_phase4_security_strikes.py` (3/3 passed).

### Fix 4.5: `ai/tools/cart_tools.py` Redis Persistence & Sliding TTL
- **Status**: DONE
- **Changes**:
  - Switched from non-existent `.get()`/`.set()` to public `get_json()` and `set_json()`.
  - Resolved sliding TTL issue: mutating cart or accessing cart refreshes/slides the Redis session TTL (`expire`).
  - Verified cross-instance durability: carts written with one instance hydrate seamlessly into a fresh instance with empty in-memory state.
- **Verification**: `tests/test_fix_4_5_cart_tools.py` (2/2 passed) and `tests/test_phase4_cart_durability.py` (2/2 passed).

### Fix 8.3: `ai/tools/bundle_tools.py` Singleton Cache Reuse
- **Status**: DONE
- **Changes**:
  - `BundleRecommendationsTool` defaults to singleton `cache_manager` instead of instantiating unpooled `RedisCacheManager()`.
  - Audited `ai/` and `apps/`: verified no other direct unpooled `RedisCacheManager()` instantiations exist.
- **Verification**: `tests/test_fix_8_3_bundle_tools.py` (1/1 passed).

### API Rate Limiter Client Access
- **Status**: DONE
- **Changes**:
  - `apps/api/rate_limiting/rate_limiter.py` updated to access public `cache_manager.client` instead of private `_client`.

---

## 3. Extra: Test Isolation & Baseline Failures Resolution
- **Baseline Failures**: `tests/test_phase3_langgraph_agent.py::test_search_agent_node_hybrid_execution` and `::test_details_agent_node_retrieval` failed previously because residual cached responses from earlier test runs routed directly to `END`.
- **Root Cause & Isolation Fix**:
  - Enforced dedicated isolated `REDIS_DB=15` in `tests/conftest.py`.
  - Autouse test fixture cleans DB 15 (`flushdb()`) and truncates PostgreSQL `semantic_query_cache` and `user_carts` tables before each test.
  - Zero modifications to the agent router logic.
- **Verification**:
  - Run 1: `tests/test_phase3_langgraph_agent.py` (9/9 passed).
  - Run 2: `tests/test_phase3_langgraph_agent.py` (9/9 passed).

---

## 4. Verification Evidence Summary
- `tests/test_fix_2_2_redis_client.py`: 4 passed
- `tests/test_fix_3_7_strike_tracker.py`: 3 passed
- `tests/test_fix_4_5_cart_tools.py`: 2 passed
- `tests/test_fix_8_3_bundle_tools.py`: 1 passed
- `tests/test_phase4_security_strikes.py`: 3 passed
- `tests/test_phase4_cart_durability.py`: 2 passed
- `tests/test_phase4_bundles.py`: 2 passed
- `tests/test_phase4_caching_and_fanout.py`: 6 passed
- `tests/test_phase3_langgraph_agent.py`: 9 passed (Run 1) and 9 passed (Run 2)
- `tests/test_architecture.py`: 1 passed

Total tests across affected test suites: **33 passed, 0 failed**.

---

## 5. Files Changed
- `core/cache/redis_client.py`
- `ai/guardrails/strike_tracker.py`
- `ai/tools/cart_tools.py`
- `ai/tools/bundle_tools.py`
- `apps/api/rate_limiting/rate_limiter.py`
- `tests/conftest.py`
- `tests/test_phase4_security_strikes.py`
- `tests/test_fix_2_2_redis_client.py` (new)
- `tests/test_fix_3_7_strike_tracker.py` (new)
- `tests/test_fix_4_5_cart_tools.py` (new)
- `tests/test_fix_8_3_bundle_tools.py` (new)
- `FIX_REPORT_WP3.md` (new)

---

## 6. Noticed But Not Fixed
- When running the entire monolithic repository test suite in a single Python process (`pytest tests/`), massive ML feature extraction, ONNX loading, and SHAP background computation in `tests/test_drift_retrain.py` and `tests/test_models.py` can cause process memory to expand to >8GB, triggering Windows paging file exhaustion on host machines without generous swap allocations.
