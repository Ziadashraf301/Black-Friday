# WP5: Backend security and routes

Read prompts/fix_common.md and follow it. Fixes from FIX_PLAN.md: 2.3, 4.4, 5.1, 5.2, 5.3, 5.4, 6.1, 7.1, 10.1. The rate limiter file was already moved by WP0 (apps/api/rate_limiting/). Work in the order: 4.4, 5.3, 5.4, 6.1, 5.2, 5.1, 10.1, 7.1, 2.3.

Required tests (use FastAPI TestClient; real Redis from docker where noted):
- 4.4 apps/api/auth.py: get_optional_user with HTTPBearer(auto_error=False) returns None without a header and the user with a valid token.
- 6.1 shopper routes: a guest POST to /shopper/predict-price-batch returns 200 (it returned 403 before); repeated requests to /shopper/predict-price produce HTTP 429 once the limit is exceeded. Make sure normal test traffic and the Reflex UI are not blocked by sane limits (check the configured limits and say what you chose).
- 5.3 rate limiter: use the public cache_manager.client (WP3); make the check-and-add atomic (Lua script or MULTI/EXEC with ZADD+ZREMRANGEBYSCORE+ZCARD in one round trip); fix the unbounded in-memory fallback growth (cleanup on reject, lock for thread safety). Test: 50 concurrent requests against a limit of 10 produce exactly 10 allowed (real Redis); in-memory fallback memory stays bounded.
- 5.4 delete RateLimiterService pass-through and update all imports (prove with grep).
- 5.2 security_ban_middleware.py: decode the Bearer JWT (sub) when X-User-ID is absent. Test: a banned user's token without X-User-ID gets 403; an unbanned token passes; an invalid token does not crash the middleware. Also confirm the WebSocket ban check in routes/bot.py still works (it is the only place bans are enforced for WebSockets).
- 5.1 main.py: explicit CORS origins when allow_credentials=True (read from settings, default to the Reflex origins); structured handlers for ValueError and unhandled exceptions; late imports moved to the top. Test: a preflight request with Origin returns the right headers; ValueError returns JSON 400.
- 10.1: create core/exceptions.py (NotFoundError, DataUnavailableError, ...), raise them from apps/api/services/analytics_service.py instead of HTTPException, map them in main.py handlers. Test the mapping.
- 7.1 routes/bot.py: attach the rate limit dependency to /bot/stream; stream tokens preserving whitespace and newlines with no artificial sleep. WARNING: the plan's regex \S+|\n+ DROPS spaces. Use a boundary rule that preserves them (for example \S+\s*|\s+). Test with a markdown list and indented text: concatenating the streamed tokens must equal the original text exactly.
- 2.3 passwords: implement with upgrade-on-login per fix_common.md decisions. Test: legacy-hash user can log in once and is re-hashed; new users get the new scheme; passwords longer than 72 bytes and multi-byte characters round-trip correctly.
