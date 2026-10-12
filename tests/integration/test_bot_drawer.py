"""Reflex bot drawer: auth gating and preset action chips.

The action chips call the bot endpoint of the running API, so this test needs the API container."""

import sys

from core.config import settings


# ==============================================================================
# P1-06: Frontend Layer 1 (Reflex Bot Drawer Auth Check)
# ==============================================================================
def test_frontend_bot_auth_check():
    """Validates Reflex ShoppingState bot drawer auth gating and preset action chips."""
    import sys

    sys.path.insert(0, str(settings.BASE_DIR / "apps" / "reflex_app"))
    from reflex_app.components.bot_drawer import bot_drawer, bot_trigger_button
    from reflex_app.state import ShoppingState

    # 1. Unauthenticated test
    state = ShoppingState()
    state.auth_token = ""
    assert state.is_authenticated is False

    # Clicking trigger while unauthenticated must prompt login
    res = state.handle_bot_trigger_click()
    assert res is False
    assert state.show_auth is True, "Must prompt auth modal when guest clicks bot trigger"
    assert state.bot_auth_checked is True
    assert "sign in" in state.bot_auth_warning.lower()

    # 2. Authenticated test
    auth_state = ShoppingState()
    auth_state.auth_token = "mock_jwt_access_token_12345"
    auth_state.user_name = "Alex"
    auth_state.user_cluster_persona = "Urban Trendsetter"
    assert auth_state.is_authenticated is True

    opened = auth_state.handle_bot_trigger_click()
    assert opened is True
    assert auth_state.is_bot_open is True
    assert len(auth_state.bot_messages) > 0

    # 3. Action chips test (*"Top Deals Today"*, *"Sale Products"*)
    import asyncio

    async def consume_chip(chip_name: str):
        async for _ in auth_state.click_action_chip(chip_name):
            pass

    asyncio.run(consume_chip("Top Deals Today"))
    last_assistant_msg = [m for m in auth_state.bot_messages if m["role"] == "assistant"][-1]
    assert "deal" in last_assistant_msg["content"].lower()

    asyncio.run(consume_chip("Sale Products"))
    last_assistant_msg = [m for m in auth_state.bot_messages if m["role"] == "assistant"][-1]
    assert "sale" in last_assistant_msg["content"].lower()

    # 4. Component compilation / rendering check
    drawer_cmp = bot_drawer()
    trigger_cmp = bot_trigger_button()
    assert drawer_cmp is not None
    assert trigger_cmp is not None
