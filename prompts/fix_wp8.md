# WP8: Frontend state (apps/reflex_app/reflex_app/state.py)

Read prompts/fix_common.md and follow it. Fix from FIX_PLAN.md: 7.6, split into the three parts below. Do them in order, committing after each part (messages WP8a, WP8b, WP8c). Evidence is in REVIEW_frontend.md (F-01, F-03, F-04, F-06, F-08, F-10, F-12, F-16).

Testing approach: Reflex states can be tested by instantiating the state class and calling handlers with httpx mocked (monkeypatch or respx if available). Also run an import/compile smoke of the Reflex app.

## 8a Money bugs (highest priority)
- Member pricing: direct "add to cart" from the catalog grid or hero card must apply cached_price_estimates for authenticated users. Test: authenticated add uses the member price, guest uses catalog price.
- Checkout: respect item quantity; do NOT clear the cart when requests fail; report errors to the user; use the new POST /shopper/purchase/batch from WP6 (fall back to per-item calls only if it is unavailable). Tests: quantity 3 sends quantity 3; a failing request keeps the cart; success clears it.

## 8b Streaming
- Convert _execute_bot_query to an async generator using httpx.AsyncClient streaming so tokens appear progressively and the worker is not blocked for up to 30 seconds. Parse grounding citations from the SSE events (match the contract from apps/api/routes/bot.py as changed in WP5/WP7). Tests: a mocked SSE stream yields incremental state updates in order; a timeout or server error leaves the UI in a usable state.

## 8c Auth persistence
- Persist auth_token with rx.LocalStorage so a page refresh keeps the session. Note in the report the security trade-off (token readable by any XSS bug) and that the backend token lifetime should stay short. Test: token restored into state on load; logout clears it.

Also confirm no hardcoded API URL remains (state.py reads API_BASE_URL).
