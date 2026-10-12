"""UI-1: the Reflex client sends a random per-browser session_id, never a shared literal."""

import asyncio
import re
import sys
from unittest.mock import patch

from core.config import settings

sys.path.insert(0, str(settings.BASE_DIR / "apps" / "reflex_app"))
from reflex_app.state import ShoppingState  # noqa: E402

SESSION_ID_PATTERN = re.compile(r"^[A-Za-z0-9_.:-]{1,64}$")


def test_session_id_is_random_valid_and_stable_per_browser():
    first = ShoppingState(_reflex_internal_init=True)
    second = ShoppingState(_reflex_internal_init=True)

    a = first._ensure_session_id()
    b = second._ensure_session_id()

    assert a != b
    assert SESSION_ID_PATTERN.match(a)
    assert first._ensure_session_id() == a


def _run_bot_query(state):
    captured = {}

    class _Stream:
        status_code = 500

        async def __aenter__(self):
            return self

        async def __aexit__(self, *exc):
            return False

    class _Client:
        def __init__(self, *a, **k):
            pass

        async def __aenter__(self):
            return self

        async def __aexit__(self, *exc):
            return False

        def stream(self, method, url, headers=None, json=None):
            captured["json"] = json
            return _Stream()

    async def drive():
        with patch("reflex_app.state.httpx.AsyncClient", _Client):
            async for _ in state._execute_bot_query("hello"):
                pass

    asyncio.run(drive())
    return captured["json"]


def test_bot_query_sends_session_id_and_no_user_id_literal():
    state = ShoppingState(_reflex_internal_init=True)

    payload = _run_bot_query(state)

    assert payload["session_id"] == state.session_id
    assert SESSION_ID_PATTERN.match(payload["session_id"])
    assert "guest_user" not in str(payload)
    assert "user_id" not in payload


def test_logged_in_user_still_gets_the_browser_session_id():
    state = ShoppingState(_reflex_internal_init=True)
    state.user_id = 7

    payload = _run_bot_query(state)

    assert payload["session_id"] == state.session_id
    assert "session_7" not in payload["session_id"]
