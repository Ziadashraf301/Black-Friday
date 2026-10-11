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
| SEC-1 | Security | `SECRET_KEY` has a hardcoded public default. If `.env` omits it, the app starts silently and anyone can forge valid JWTs for any `sub`. There is no startup check when `ENVIRONMENT` is not dev. | `core/config.py:180` | high | fixed | 07b8b95 |
| SEC-2 | Security | Rate-limit identity trusts caller-controlled `X-User-ID` header and `user_id` query param ahead of the client IP. Rotating the header gives a fresh bucket per request, so the per-minute/day limits on `/bot/*` are bypassable. | `apps/api/rate_limiting/rate_limiter.py:280` | medium | fixed | 6fcc1b7 |
| SEC-3 | Security | `SecurityBanMiddleware` prefers unverified `X-User-ID`/`user_id` over the JWT `sub`. A locked-out authenticated user sends `X-User-ID: <anything>` to skip the user-level ban check; only the IP check remains. | `apps/api/middleware/security_ban_middleware.py:26` | medium | fixed | 6fcc1b7 |
| SEC-4 | Security | `/auth/login` and `/auth/signup` have no rate limiting, so there is no brute-force or credential-stuffing protection. | `apps/api/routes/auth.py:17,24` | medium | fixed | 656fa4a |
| SEC-5 | Security | `/bot/*` endpoints are unauthenticated and take `user_id` from query/body, so any caller can act as any shopper in the assistant (history, personalisation). | `apps/api/routes/bot.py:184,213` | medium | fixed | 149639d |
| SEC-6 | Security | `get_current_user` echoes the raw JWT exception text to the client (`Invalid or expired token: {e}`). Information leak. | `apps/api/auth.py:44` | low | fixed | 8193f96 |
| SEC-7 | Security | JWT has no `iss`/`aud` claims and no revocation. CORS uses `allow_methods=["*"]` and `allow_headers=["*"]` with credentials. | `core/security.py:99`, `apps/api/main.py:64` | low | fixed | a92508c |
| SEC-8 | Security | All unauthenticated bot users share the single `guest_user` identity (`guest_voice_shopper` on the WebSocket), so history, cart or personalisation state could be shared between strangers. Fixed: guests get an HMAC(SECRET_KEY) id from peer IP + session_id (`guest_<24 hex>`); `session_id` is validated (`[A-Za-z0-9_.:-]{1,64}`). Residual: guests behind one NAT sharing a `session_id` (the Reflex client sends `session_guest_user`) still share state; the client should send a random per-browser id (UI step). | `apps/api/client_identity.py` (`GUEST_USER_ID`), `apps/api/routes/bot.py:298` | fixed | 159a174 | |
| SEC-9 | Security | No `X-Forwarded-For` / trusted-proxy handling. The client IP is the socket peer, so behind a reverse proxy every caller shares the proxy IP for rate limits and bans. | `apps/api/client_identity.py` (`client_ip`) | low | deferred | |
| API-1 | API | `ShopperBatchPurchaseItem.quantity` and `items` lists have no upper bound. `process_batch_purchase` builds one record per unit, so an authenticated user can send `quantity=10**9` and exhaust memory / the DB. `predict-price-batch` (unauthenticated, no rate limit) is also uncapped. | `apps/api/schemas.py:147,214,224`, `apps/api/services/shopper_service.py:437` | fixed | 6a819fa | |
| API-2 | API | `GET /shopper/catalog?limit=` is unbounded and each value gets its own Redis key (`shopper:catalog:{limit}`). Negative or huge values hit the DB, and unique values pollute the cache. | `apps/api/routes/shopper.py:31`, `apps/api/services/shopper_service.py:54` | fixed | 5724579 | |
| API-3 | API | `/shopper/predict-price-batch` is open to guests with no rate limit, unlike `/predict-price`. Each call runs ONNX inference and DB lookups. Fixed: now `rate_limit_dependency` (auth + rate limit). Reflex only calls it with a token. | `apps/api/routes/shopper.py:50` | fixed | 5724579 | |
| API-4 | API | `process_batch_purchase` keys quotes by `product_id` only. Two cart lines for the same product with different demographic overrides get the same (last) quote. | `apps/api/services/shopper_service.py:420` | fixed | b4f6dfb | |
| API-5 | API | Purchase price depends on demographics taken from the request body (`gender`, `age`, ...), so a shopper can pick the demographic that gives the lowest member price. Design question rather than a bug. Product decision: whether quotes should use only the account's stored demographics (breaking for clients that send overrides) or keep accepting overrides. Needs an owner call. | `apps/api/routes/shopper.py:112`, `apps/api/schemas.py:133` | deferred |  | |
| API-6 | API | Signup does not normalise the email (case) and has a check-then-insert race, so a duplicate surfaces as a 500 (`IntegrityError`) and `A@x.com` / `a@x.com` can both register. `email` is a plain `str`, not validated. Fixed: emails are trimmed/lower-cased; duplicate-insert race maps to 409; login falls back to the submitted spelling for legacy mixed-case accounts. | `apps/api/services/auth_service.py:32`, `apps/api/schemas.py:93` | fixed | 8791fa0 | |
| API-7 | API | The WebSocket error path sends `str(e)` to the client, which can leak internals. A non-object JSON frame (list/number) raises `AttributeError` on `data.get` and ends the session. | `apps/api/routes/bot.py:359,288` | fixed | 304eb6d | |
| API-8 | API | The global `ValueError` handler returns `str(exc)` for any uncaught `ValueError` in the app, which can expose internals. Product decision: `test_value_error_exception_handler` asserts the handler exposes the ValueError message, and `model_service` raises user-facing ValueErrors. Needs a call on a generic message vs. typed domain errors. | `apps/api/handlers.py:20` | deferred |  | |
| AI-1 | AI | Circular import: `ai.nodes.router_node` → `ai.workflow.state` → `ai/workflow/__init__` → `ai.workflow.graph` → `ai.nodes.router_node` (partially initialised). It breaks collection of `tests/test_fix_wp7_logging.py` when it imports `ai.nodes` first. | `ai/workflow/__init__.py:6`, `ai/workflow/graph.py:9`, `ai/nodes/router_node.py:8` | medium | open | |

## Refactors (step 2, behaviour-preserving)

- 4cd6c22 unused imports and dead local removed
- 770ab4d auth service token/response building shared
- 7bf6440 bot SSE frame emission and StreamingResponse deduplicated
- a16db80 black/isort applied to apps/api, flake8 clean
