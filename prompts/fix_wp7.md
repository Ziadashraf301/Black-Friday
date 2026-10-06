# WP7: AI subsystem

Read prompts/fix_common.md and follow it. Fixes from FIX_PLAN.md: 5.6, 6.4, 10.2 (ONE design, do them together), logging unification 6.3 + 7.3 + 8.2 (one change across all ai/ files), 7.4, 9.5, 9.6, 8.5. Redis fixes in ai/ were done in WP3; the SSE route changes were done in WP5.

- ui_payload (5.6, 6.4, 10.2): today the aggregator builds multi-card UI payloads and synthesis_service overwrites them. Define ONE typed UI payload schema (cards + action_chips + citations) in ai/workflow/state.py with an explicit reducer so specialist nodes' cards merge, then make aggregator_node and synthesis_service use it. Test with a fake LLM (no network): a fanout run with two specialists yields both card sets and the action chips in the final payload; a single-intent run still works. Also extract the deterministic templating from synthesis_service if it keeps the file coherent.
- Logging: replace every `from loguru import logger` in ai/ (grep the whole package, not only the three nodes) with core.logging get_logger. Then check whether loguru is still imported anywhere; if not, remove it from requirements.txt. Test: importing the nodes and running one logs through the core logger.
- 7.4 hybrid_extractor.py: remove the duplicated shoe/waist size parsing; test with sizing queries before/after (same extracted entities).
- 9.5 embeddings now live in core/embeddings/ (WP0). Migrate from google.generativeai to the google.genai Client. Check that the package is installed/pinned in requirements.txt; test with a mocked client for the request/response shape and that missing API key degrades gracefully (no crash at import).
- 9.6 policy_kb.py: reload store_policies.json when its mtime changes. Test: change the file in a temp dir and see the new content without restart.
- 8.5 tests/test_phase2_jev_router.py: stop asserting 3ms/10ms on unit runs: move timing checks behind a `benchmark` marker (register the marker in pytest.ini) with generous thresholds; correctness assertions stay.
- Verify: run tests/test_phase2*, test_phase3*, all test_phase4*; verify the observability code still traces through core.tracking (WP0).
