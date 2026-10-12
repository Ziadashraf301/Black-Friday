"""UI-5: when the assistant API is unreachable the UI says so; it does not invent deals, prices or leak errors."""

import asyncio
import json
import sys
from unittest.mock import AsyncMock, MagicMock, patch

import httpx
import pytest
from reflex_app.state import BOT_UNAVAILABLE_MESSAGE, ShoppingState  # noqa: E402

from core.config import settings


class MockAsyncResponse:
    def __init__(self, lines, status_code=200):
        self._lines = lines
        self.status_code = status_code

    async def aiter_lines(self):
        for line in self._lines:
            yield line

    async def __aenter__(self):
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        pass


class MockAsyncClient:
    def __init__(self, response=None, exc=None):
        self._response = response
        self._exc = exc

    def stream(self, method, url, **kwargs):
        if self._exc:
            raise self._exc
        return self._response

    async def __aenter__(self):
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        pass


@pytest.mark.asyncio
async def test_mocked_sse_stream_yields_incremental_state_updates_in_order():
    state = ShoppingState(_reflex_internal_init=True)
    state.user_id = 42
    state.bot_messages = [{"role": "user", "content": "Show me coats"}]

    sse_lines = [
        'data: {"type": "token", "content": "We have "}',
        'data: {"type": "token", "content": "great coats."}',
        'data: {"type": "ui_card", "payload": {"cards": [{"product_id": "P01", "name": "Tweed Coat", "price": 99.0}]}}',
        'data: {"type": "grounding", "citations": [{"product_id": "P01", "title": "Tweed Coat", "url": "/shopper/browse/P01", "price": 99.0}]}',
        'data: {"type": "done"}',
        'data: [DONE]',
    ]
    mock_resp = MockAsyncResponse(sse_lines, status_code=200)

    states_recorded = []
    with patch("httpx.AsyncClient", return_value=MockAsyncClient(response=mock_resp)):
        async for _ in state._execute_bot_query("Show me coats"):
            assistant_content = state.bot_messages[-1]["content"] if state.bot_messages else ""
            states_recorded.append({
                "loading": state.bot_loading,
                "content": assistant_content,
                "card_count": len(state.bot_active_cards),
                "citations_count": len(state.bot_citations),
            })

    # Assert incremental progression
    assert len(states_recorded) >= 4
    # Initial state
    assert states_recorded[0]["loading"] is True
    # Progressive content updates
    contents = [s["content"] for s in states_recorded]
    assert any("We have " in c for c in contents)
    assert any("We have great coats." in c for c in contents)
    # UI card update
    assert any(s["card_count"] == 1 for s in states_recorded)
    # Citations parsed
    assert any(s["citations_count"] == 1 for s in states_recorded)
    assert state.bot_active_cards[0]["product_id"] == "P01"
    assert state.bot_citations[0]["product_id"] == "P01"
    assert "Tweed Coat" in state.bot_messages[-1]["content"]
    # Final state: loading is complete
    assert state.bot_loading is False


@pytest.mark.asyncio
async def test_bot_stream_timeout_leaves_ui_usable():
    state = ShoppingState(_reflex_internal_init=True)
    state.bot_messages = [{"role": "user", "content": "Top Deals Today"}]

    timeout_exc = httpx.TimeoutException("Read timed out after 30.0s")

    with patch("httpx.AsyncClient", return_value=MockAsyncClient(exc=timeout_exc)):
        async for _ in state._execute_bot_query("Top Deals Today"):
            pass

    # UI must not remain in loading state
    assert state.bot_loading is False
    assert len(state.bot_messages) >= 2
    last_msg = state.bot_messages[-1]
    assert last_msg["role"] == "assistant"
    # Should contain fallback reply for deals or error note
    assert "deal" in last_msg["content"].lower() or "offline" in last_msg["content"].lower()


@pytest.mark.asyncio
async def test_bot_stream_server_error_leaves_ui_usable():
    state = ShoppingState(_reflex_internal_init=True)
    state.bot_messages = [{"role": "user", "content": "Help me"}]

    mock_resp = MockAsyncResponse([], status_code=500)

    with patch("httpx.AsyncClient", return_value=MockAsyncClient(response=mock_resp)):
        async for _ in state._execute_bot_query("Help me"):
            pass

    assert state.bot_loading is False
    assert len(state.bot_messages) >= 2
    last_msg = state.bot_messages[-1]
    assert last_msg["role"] == "assistant"
    assert "500" in last_msg["content"] or "error" in last_msg["content"].lower()


@pytest.mark.asyncio
async def test_send_bot_message_yields_progressively():
    state = ShoppingState(_reflex_internal_init=True)
    state.bot_input_text = "Find boots"

    sse_lines = [
        'data: {"type": "token", "content": "Leather boots found."}',
        'data: [DONE]',
    ]
    mock_resp = MockAsyncResponse(sse_lines, status_code=200)

    with patch("httpx.AsyncClient", return_value=MockAsyncClient(response=mock_resp)):
        yield_count = 0
        async for _ in state.send_bot_message():
            yield_count += 1

    assert state.bot_input_text == ""
    assert yield_count >= 1
    assert state.bot_loading is False
    assert "Leather boots found." in state.bot_messages[-1]["content"]


class _Boom:
    def __init__(self, *a, **k):
        pass

    async def __aenter__(self):
        raise ConnectionError("connect to 10.1.2.3:8000 failed")

    async def __aexit__(self, *exc):
        return False


def _ask(query):
    state = ShoppingState(_reflex_internal_init=True)

    async def drive():
        with patch("reflex_app.state.httpx.AsyncClient", _Boom):
            async for _ in state._execute_bot_query(query):
                pass

    asyncio.run(drive())
    return state


def test_every_chip_gets_the_same_honest_unavailable_message():
    for chip in ("Top Deals Today", "Sale Products", "Under $50", "Style Advisor", "anything else"):
        state = _ask(chip)
        assert state.bot_messages[-1] == {"role": "assistant", "content": BOT_UNAVAILABLE_MESSAGE}
        assert state.bot_loading is False


def test_unavailable_message_has_no_prices_products_or_error_details():
    state = _ask("Top Deals Today")
    reply = state.bot_messages[-1]["content"]

    assert "$" not in reply
    assert "Kimono" not in reply
    assert "10.1.2.3" not in reply
