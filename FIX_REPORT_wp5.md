# Work Package 5: Backend Security and Routes — Fix Report

## Executive Summary
This work package resolves all critical security, rate limiting, and route integrity findings outlined in `prompts/fix_wp5.md`. All tasks have been implemented according to architectural requirements, thoroughly covered by automated tests, and verified without breaking existing contracts.

---

## Resolved Tasks and Changes

### 1. Fix 4.4 — `apps/api/auth.py`
- Implemented `get_optional_user` using `HTTPBearer(auto_error=False)`.
- Allows guest/unauthenticated calls to proceed without raising HTTP 403/401 when the `Authorization` header is omitted or None.
- Decodes valid bearer tokens and returns standard user dictionary; returns `None` for invalid or missing tokens.

### 2. Fix 5.3 — `apps/api/rate_limiting/rate_limiter.py`
- Implemented atomic Redis sliding window rate limiting using a custom Lua script `LUA_SLIDING_WINDOW_RATE_LIMIT`.
- Atomic evaluation of per-minute and per-day sorted sets (`ZREMRANGEBYSCORE`, `ZCARD`, `ZADD`, `EXPIRE`) eliminating race conditions.
- Upgraded the in-memory fallback to be thread-safe via `threading.Lock()` and bounded with periodic sweep to prevent memory leaks.
- Configured sane defaults (60 req/min, 1000 req/day) in `core/config.py` preventing accidental lockout of regular users.

### 3. Fix 5.4 — Deprecated Service Removal
- Removed duplicate file `apps/api/services/rate_limiter_service.py`.
- Replaced references across the codebase and updated `tests/test_phase1_infra_frontend.py` to use `RedisRateLimiter` directly.
- Exported clean rate limiter instances from `apps/api/rate_limiting`.

### 4. Fix 6.1 — `apps/api/routes/shopper.py`
- Changed `/shopper/predict-price-batch` dependency from `get_current_user` to `get_optional_user`, allowing high-concurrency cart quotes for guest shoppers.
- Added `rate_limit_dependency` to `/shopper/predict-price` to protect pricing inference from scraping and overload.

### 5. Fix 5.2 — `apps/api/middleware/security_ban_middleware.py` & `apps/api/routes/bot.py`
- Updated middleware to inspect `Authorization: Bearer <token>` and decode JWT claims for `sub` if `X-User-ID` or query params are absent.
- Wrapped decoding safely so malformed tokens never trigger unhandled 500 crashes in the middleware.
- Ensured WebSocket and HTTP endpoints reject banned users with HTTP 403 Forbidden.

### 6. Fix 5.1 — `apps/api/main.py`
- Replaced `allow_origins=["*"]` with explicit origins configured via `settings.ALLOWED_ORIGINS` (defaulting to Reflex/FastAPI ports `localhost:3000`, `localhost:8000`, `127.0.0.1:3000`, `127.0.0.1:8000`).
- Registered structured exception handlers for `ValueError` (mapping to HTTP 400 Bad Request) and domain exceptions (`NotFoundError` -> 404, `DataUnavailableError` -> 404, `ValidationError` -> 400).
- Reorganized module-level imports, moving late imports from `startup_event` to the top of `main.py`.

### 7. Fix 10.1 — Domain Exceptions
- Created `core/exceptions.py` defining custom exception hierarchy: `AppException`, `NotFoundError`, `DataUnavailableError`, `ValidationError`, `InferenceError`, `AuthenticationError`, `RateLimitExceededError`.
- Updated `apps/api/services/analytics_service.py` to raise specific domain exceptions instead of generic `RuntimeError` or `ValueError`.

### 8. Fix 7.1 — `apps/api/routes/bot.py`
- Implemented `tokenize_for_stream(text)` using regex `r"\S+\s*|\s+"` to preserve exact spaces, tabs, and newlines without dropping markdown formatting.
- Replaced artificial `asyncio.sleep(0.015)` with streaming chunk yields.
- Applied rate limiting dependencies to both `POST` and `GET` stream endpoints.

### 9. Fix 2.3 — Password Security & Seamless Upgrades
- Mitigated bcrypt 72-byte truncation and UTF-8 multi-byte character issues by applying a SHA-256 pre-hash before bcrypt hashing in `core/security.py`.
- Implemented `verify_password_with_upgrade` to identify legacy hashes and trigger an automatic upgrade on login.
- Added `update_user_password` in `core/db/repositories/user_repo.py` and hooked seamless re-hashing into `AuthService.authenticate_user`.

---

## Verification & Test Results
- Created 7 dedicated test suites:
  - `tests/test_fix_4_4_optional_user.py` (3 passed)
  - `tests/test_fix_5_3_rate_limiter.py` (3 passed)
  - `tests/test_fix_2_3_passwords.py` (5 passed)
  - `tests/test_fix_7_1_bot_stream.py` (3 passed)
  - `tests/test_fix_5_2_ban_middleware.py` (4 passed)
  - `tests/test_fix_5_1_cors_errors.py` (4 passed)
  - `tests/test_fix_6_1_shopper_routes.py` (2 passed)
- Ran regression suite: `24 passed in 25.74s`.
- Ran Phase 1 rate limiter test suite: `2 passed in 3.54s`.
- No regressions observed.
