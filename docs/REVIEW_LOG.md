# Code Review Log — Round 1

Branch: `review/round-1` · Plan: [REVIEW_PLAN.md](REVIEW_PLAN.md)

Severity: high / medium / low · Status: open / fixed / deferred / won't fix

## Summary

_(filled in at step 10)_

## Baseline (step 0)

Recorded 2026-10-11 on `review/round-1` (no fixes applied). Local Postgres, Redis and MLflow are reachable, and the Docker API container is running.

### Tests

A single `pytest tests/` run is unusable on Windows. `test_api.py` hangs, and pytest-timeout's thread method kills the whole process. The baseline below comes from running each file separately with `--timeout=45`. 53 of 56 files pass.

| Test / file | Result | Cause |
|---|---|---|
| `tests/test_api.py::test_shopper_predict_and_purchase_flow` | **HANG** (killed at the timeout) | `MagicMock` repo returns a `MagicMock` from `get_curated_products()`. `ShopperService.get_curated_catalog` (`shopper_service.py:48`) passes it to `cache_manager.set_json`. `_json_serializer` (`core/cache/redis_client.py:20`) calls `.item()` on it. Each `.item()` returns another mock, so the `json.dumps` default hook never ends. The other 4 tests in the file pass. |
| `tests/test_fix_wp7_logging.py` | collection ERROR | Circular import: `ai.nodes.router_node` → `ai.workflow.state` → `ai/workflow/__init__` → `ai.workflow.graph` → `ai.nodes.router_node` (partially initialised). |
| `tests/test_phase1_infra_frontend.py::test_redis_rate_limit_5_min_20_day` | FAIL | `RateLimitExceededError: Max 5 requests per minute`. The test uses the live Redis (DB 15), so state left over from earlier runs likely matters. Not confirmed. |

Passing but noisy: `test_phase3`, `test_phase4_*` and `test_phase2` log MLflow warnings ("Failed to send trace… interpreter shutdown"). `test_phase3_langgraph_agent` takes about 50s. Other slow files are `test_drift_retrain` (30s) and `test_phase4_caching_and_fanout` (28s).

### Lint (`make lint`)

| Tool | Result |
|---|---|
| flake8 | 494 violations (337 outside `tests/`). Mostly F401 unused imports, E402, W291/W293 and E501. |
| black | 205 of 228 files would be reformatted. |
| isort | 203 files have incorrectly sorted imports. |

The lint baseline is effectively "never enforced". Treat lint as a per-file check on touched files for the rest of the review. A repo-wide `black`/`isort` pass would be a separate round-2 item.

## Findings

| ID | Area | Finding | File:line | Severity | Status | Commit |
|----|------|---------|-----------|----------|--------|--------|
| CORE-1 | Core | `_json_serializer` calls `.item()` on any object that has an `.item` attribute. For a `MagicMock` (as in `tests/test_api.py::test_shopper_predict_and_purchase_flow`) every `.item()` returns another mock, so `json.dumps` never ends and the test hangs. The serializer should only unwrap numpy scalars and fall back to `str()` otherwise. The test fixture also needs a real return value for `get_curated_products()`. | `core/cache/redis_client.py:20` | medium | open | |
| AI-1 | AI | Circular import: `ai.nodes.router_node` → `ai.workflow.state` → `ai/workflow/__init__` → `ai.workflow.graph` → `ai.nodes.router_node` (partially initialised). It breaks collection of `tests/test_fix_wp7_logging.py` when it imports `ai.nodes` first. | `ai/workflow/__init__.py:6`, `ai/workflow/graph.py:9`, `ai/nodes/router_node.py:8` | medium | open | |
