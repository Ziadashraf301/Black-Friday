"""AI-14: the fallback guest identity is one constant, not a string repeated in four places."""

from pathlib import Path

from ai.schemas import CartState
from ai.schemas.identity import GUEST_USER_ID

ROOT = Path(__file__).resolve().parents[3]


def test_constant_value_is_unchanged():
    assert GUEST_USER_ID == "guest_user"


def test_cart_state_defaults_to_the_constant():
    assert CartState(session_id="s").user_id == GUEST_USER_ID


def test_no_other_literal_in_ai_package():
    offenders = [
        str(p.relative_to(ROOT))
        for p in (ROOT / "ai").rglob("*.py")
        if p.name != "identity.py" and '"guest_user"' in p.read_text(encoding="utf-8")
    ]
    assert offenders == []


def test_nodes_use_the_constant_when_state_has_no_user(monkeypatch):
    from ai.nodes import cart_node as cart_module
    from ai.nodes import support_node as support_module

    seen = {}

    class FakeCart:
        def modify_cart(self, **kw):
            seen["cart"] = kw["user_id"]
            return CartState(user_id=kw["user_id"], session_id="s")

    class FakeOrders:
        def track_order(self, order_id, user_id):
            seen["order"] = user_id
            from ai.schemas import OrderStatus

            return OrderStatus(order_id="ORD-1", status="Not Found")

        def get_policy(self, topic):
            return {}

    monkeypatch.setattr(cart_module, "cart_tool", FakeCart())
    monkeypatch.setattr(support_module, "order_tool", FakeOrders())
    cart_module.cart_agent_node({"query": "view cart"})
    support_module.support_agent_node({"query": "track my order"})
    assert seen == {"cart": GUEST_USER_ID, "order": GUEST_USER_ID}
