# WP3: Redis layer and its consumers

Read prompts/fix_common.md and follow it. Fixes from FIX_PLAN.md: 2.2, 3.7, 4.5, 8.3. Treat them as ONE work item: 3.7 and 4.5 must use the PUBLIC API created by 2.2, never the private _client.

First check (record the result in your report): open core/cache/redis_client.py and list every public method. Confirm whether get_json / set_json exist and how TTL is passed. The reviews claim ai/guardrails/strike_tracker.py and ai/tools/cart_tools.py call methods that do not exist (.get, .set, .client): confirm by reading the code before changing anything.

- 2.2 redis_client.py: public thread-safe `client` property; 15 second reconnection cooldown after a failure so a dead Redis never costs 2 seconds per call; delete_pattern uses scan_iter in batches. Tests: with an unreachable Redis, is_available returns False in under 100 ms on the second and later calls; delete_pattern removes only matching keys (use real Redis from docker); concurrent access from threads does not crash.
- 3.7 strike_tracker.py: use the public API; strikes increment under the strike key, bans write with a 24h TTL. Test with REAL Redis: strike, ban, expiry (use a short TTL override), and the in-memory fallback when Redis is down. Check the existing tests/test_phase4_security_strikes.py: if it passed only because of the in-memory fallback, say so and strengthen it to cover Redis.
- 4.5 cart_tools.py: use get_json/set_json; carts persist in Redis. Test: write a cart, build a NEW manager instance, read it back. Check the sliding TTL issue in REVIEW_HANDOFF.md section 6 (adding an item should refresh TTL) and fix it if simple.
- 8.3 bundle_tools.py: reuse the singleton cache_manager. Grep ai/ and apps/ for any other `RedisCacheManager()` instantiation and fix them too.
- Also update apps/api rate limiter ONLY if it still touches the private client (WP5 will finish it); otherwise leave it.

## Extra: test isolation
Also fix the 2 baseline failures (tests/test_phase3_langgraph_agent.py::test_search_agent_node_hybrid_execution and ::test_details_agent_node_retrieval). They fail because cached responses from earlier runs send the router to END. Fix the TEST ISOLATION (dedicated REDIS_DB for tests in conftest.py, clear the test namespace in a fixture), not the router. Show both pass on two consecutive runs.
