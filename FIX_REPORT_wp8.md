# Work Package 8 Fix Report: Frontend Reactive State & Client Integration

## Summary of Findings and Fix Status

| Fix ID | Topic | Status | Files Changed | Evidence |
|---|---|---|---|---|
| **WP8a (7.6, F-03, F-04, F-10, F-12)** | Member pricing cache lookup, quantity-aware checkout, batch checkout with per-item fallback | **DONE** | `apps/reflex_app/reflex_app/state.py`, `apps/reflex_app/reflex_app/components/cart_drawer.py` | `tests/test_fix_wp8a_cart_checkout.py` (6 passed) |
| **WP8b (7.6, F-01, F-06, F-16)** | Async SSE bot streaming (`httpx.AsyncClient`), incremental token yielding, grounding citation parsing | **DONE** | `apps/reflex_app/reflex_app/state.py` | `tests/test_fix_wp8b_bot_streaming.py` (4 passed) |
| **WP8c (7.6, F-08)** | Auth token persistence (`rx.LocalStorage`), session restoration on load, clean logout | **DONE** | `apps/reflex_app/reflex_app/state.py` | `tests/test_fix_wp8c_auth_persistence.py` (4 passed), `tests/test_api_client.py` (2 passed) |

---

## Detailed Findings and Verification

### 1. WP8a: Member Pricing and Cart Checkout Durability (F-03, F-04, F-10, F-12)
- **Finding**:
  - Direct "Add to Bag" from the catalog grid or "Shop Now" from the Hero card bypassed `cached_price_estimates`, charging authenticated members full catalog price unless they first opened the Quick-View modal (`F-03`).
  - Checkout performed N+1 blocking calls ignoring item quantity (ordering 3 sent 1 purchase record), unconditionally emptied the cart on network failures or 500 errors, and lacked batch purchase utilization (`F-04`).
  - Duplication existed in recommendation parsing (`F-10`) and magic currency conversion numbers were hardcoded (`F-12`).
- **Solution**:
  - In `ShoppingState._add_with_size`, verified authentication and checked `cached_price_estimates[product_id]` before falling back to `estimated_price_usd`.
  - In `ShoppingState.checkout`, respected `item.quantity` and called `POST /shopper/purchase/batch` from WP6. Implemented fallback to per-item `POST /shopper/purchase` for each quantity unit only when the batch endpoint is unavailable (e.g. 404).
  - Maintained cart items and populated `checkout_error` if checkout requests fail, clearing the cart only upon confirmed success.
  - Bound `INR_TO_USD_RATE = 80.0` constant and reused `_extract_recs` helper for bundles and similars.
- **Verification**:
  - `pytest tests/test_fix_wp8a_cart_checkout.py` -> 6 passed (authenticated member pricing, guest pricing, hero pricing, quantity-aware batch checkout, failure preservation, per-item fallback).

### 2. WP8b: Async Generator Bot SSE Streaming (F-01, F-06, F-16)
- **Finding**:
  - `_execute_bot_query` previously made a synchronous `httpx.post(..., timeout=30.0)` blocking call directly on the Reflex event loop, freezing worker threads for up to 30s and preventing token-by-token streaming (`F-01`).
  - Grounding citations emitted by the backend SSE stream (`type: "grounding"`) were discarded (`F-06`).
  - `bot_loading` typing indicator and error recovery were incomplete (`F-16`).
- **Solution**:
  - Converted `_execute_bot_query`, `click_action_chip`, and `send_bot_message` to `async` generators using `httpx.AsyncClient(timeout=30.0).stream("POST", ...)`.
  - Parsed SSE chunks asynchronously line by line (`resp.aiter_lines()`), incrementally updating `self.bot_messages[-1]["content"]` and yielding to Reflex after each token to drive real-time UI animation.
  - Parsed `type: "grounding"` citations, stored structured citations in `self.bot_citations`, and appended Markdown links/sources to assistant responses.
  - Managed `self.bot_loading = True` on start and reset to `False` in `finally:`, handling 403 strike lockouts, timeouts, and server errors gracefully without locking the UI.
- **Verification**:
  - `pytest tests/test_fix_wp8b_bot_streaming.py` -> 4 passed (progressive token streaming in order, UI cards and citations parsed, timeout fallback usable, server error leaves UI usable, message dispatch progressive yield).

### 3. WP8c: Session Persistence & Logout Resets (F-08)
- **Finding**:
  - `auth_token` was stored as an ephemeral in-memory string; refreshing the browser cleared the token and logged the user out (`F-08`).
  - Session restoration logic did not hydrate profile or member discounts on page reload.
- **Solution**:
  - Wrapped `auth_token` in `rx.LocalStorage("", name="bf_access_token")`.
  - In `load_catalog()`, checked if `self.auth_token` is present from LocalStorage and restored user profile (`/auth/me`), fetched batch member price quotes (`/shopper/predict-price-batch`), and repriced existing cart items.
  - In `do_logout()`, set `self.auth_token = ""` (clearing client storage), reset all user profile fields, cleared `cached_price_estimates`, and reset cart prices back to catalog rates.
  - Confirmed `API_BASE_URL` is dynamically read via `os.getenv("API_BASE_URL", "http://127.0.0.1:8000")` with zero hardcoded URLs remaining in `state.py`.
- **Security Trade-off Note**:
  - Persisting tokens in browser `localStorage` allows tokens to survive page refreshes, but makes them accessible to JavaScript running in the same origin (subject to token theft in the event of an XSS vulnerability).
  - To mitigate this risk, backend JWT lifetimes must stay short (e.g. 15-60 minutes), and sensitive account actions should require re-authentication.
- **Verification**:
  - `pytest tests/test_fix_wp8c_auth_persistence.py` -> 4 passed (`_is_client_storage` verified, restored token populates profile and batch prices, logout clears token and resets state, dynamic `API_BASE_URL` confirmed).
  - `pytest tests/test_api_client.py` -> 2 passed.

---

## Test Suite Execution Summary
- `pytest tests/test_fix_wp8a_cart_checkout.py`: 6 passed
- `pytest tests/test_fix_wp8b_bot_streaming.py`: 4 passed
- `pytest tests/test_fix_wp8c_auth_persistence.py`: 4 passed
- `pytest tests/test_api_client.py`: 2 passed
- **Total WP8 tests**: 16 passed in 10.14s
- **Compile/import smoke test**: `python -m py_compile apps/reflex_app/reflex_app/state.py apps/reflex_app/reflex_app/reflex_app.py` passed with exit code 0.

## Noticed but Not Fixed
- `ShoppingState` remains a single large state class (~1360 LOC). A modular split into substates (`AuthState`, `CartState`, `BotState`, `DashboardState`) as noted in `F-13` would further reduce WebSocket state serialization overhead in a future architectural refactor.
