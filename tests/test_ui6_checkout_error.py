"""UI-6: a checkout network failure shows a fixed message, not the raw exception text."""

import sys
from unittest.mock import MagicMock, patch

from core.config import settings

sys.path.insert(0, str(settings.BASE_DIR / "apps" / "reflex_app"))
from reflex_app.state import CHECKOUT_NETWORK_ERROR, CartItem, ShoppingState  # noqa: E402


def test_network_failure_does_not_leak_exception_text_and_keeps_the_cart():
    state = ShoppingState(_reflex_internal_init=True)
    state.auth_token = "token"
    state.cart_items = [CartItem(key="P1_M", product_id="P1", price=10.0)]
    client = MagicMock()
    client.__enter__.return_value.post.side_effect = ConnectionError("db at 10.1.2.3 refused")

    with patch("reflex_app.state.httpx.Client", return_value=client):
        state.checkout()

    assert state.checkout_error == CHECKOUT_NETWORK_ERROR
    assert "10.1.2.3" not in state.checkout_error
    assert len(state.cart_items) == 1
