"""UI-5: when the assistant API is unreachable the UI says so; it does not invent deals, prices or leak errors."""

import asyncio
import sys
from unittest.mock import patch

from core.config import settings

sys.path.insert(0, str(settings.BASE_DIR / "apps" / "reflex_app"))
from reflex_app.state import BOT_UNAVAILABLE_MESSAGE, ShoppingState  # noqa: E402


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
