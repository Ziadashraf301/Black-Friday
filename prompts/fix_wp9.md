# WP9: Frontend components and API-client tests

Read prompts/fix_common.md and follow it. Fixes from FIX_PLAN.md: 4.6, 5.7, 6.7, 9.7, 10.5, 7.7. Evidence: REVIEW_frontend.md (F-02, F-09, F-05, F-17, F-18, F-11, F-15, F-14, F-19).

- 4.6 hero_card.py: bind image/title/tagline/price/click to ShoppingState.hero_product (no hardcoded P00025442). Test: changing hero_product changes what the card adds to the bag.
- 5.7 sidebar.py: compute category/brand/style/season chips from the loaded catalog instead of 40 hardcoded items. Put the computation in a pure function so it is unit-testable; test that all categories in a sample catalog are selectable.
- 6.7 bot_drawer.py: render assistant messages with rx.markdown(); make recommended product cards clickable (open the product); replace the non-functional voice toggle with a disabled "Beta soon" badge.
- 9.7 auth_modal.py: extract a reusable error-banner component; add the occupation select using OCCUPATION_LABELS to signup. Check that the signup request payload includes the occupation field the backend schema expects (apps/api/schemas.py); fix the mismatch if there is one.
- 10.5 product_card.py: single on_click on the root container (no duplicate events from child elements).
- 7.7 tests/test_api_client.py: the existing test mocks httpx.Client.get without calling the real method: make it call state.load_dashboard() and assert the parsed result; replace the static API_BASE_URL assertion with a monkeypatched-environment test.
- Verify: import/compile smoke of the Reflex app (`python -c "import reflex_app.reflex_app"` from apps/reflex_app) and, if it finishes in a few minutes, `reflex export --no-zip` or the equivalent compile step; run the frontend tests. Component rendering cannot be fully verified without a browser: state clearly what you verified and what you could not.
