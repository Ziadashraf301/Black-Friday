"""INF-19: the bot drawer handlers return nothing (Reflex raises TypeError on a bool); the outcome is state only."""

import pytest
from reflex_app.state import ShoppingState


@pytest.mark.parametrize("handler", ["handle_bot_trigger_click", "open_bot_drawer", "toggle_bot"])
def test_guest_is_asked_to_sign_in(handler):
    state = ShoppingState(_reflex_internal_init=True)
    state.auth_token = ""
    assert getattr(state, handler)() is None
    assert state.is_bot_open is False and state.show_auth is True
    assert "sign in" in state.bot_auth_warning.lower()


@pytest.mark.parametrize("handler", ["handle_bot_trigger_click", "open_bot_drawer", "toggle_bot"])
def test_member_gets_the_drawer_and_a_greeting(handler):
    state = ShoppingState(_reflex_internal_init=True)
    state.auth_token = "token"
    state.user_name = "Alex"
    assert getattr(state, handler)() is None
    assert state.is_bot_open is True and state.bot_auth_warning == ""
    assert "Alex" in state.bot_messages[0]["content"]
