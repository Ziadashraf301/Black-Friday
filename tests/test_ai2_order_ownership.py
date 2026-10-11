"""AI-2: order tracking only reveals orders owned by the caller."""

import uuid

import pytest
from sqlalchemy import text

from ai.tools.order_tools import OrderSupportTool


@pytest.fixture
def orders():
    tool = OrderSupportTool()
    repo = tool.repo
    if repo is None:
        pytest.skip("PostgreSQL unavailable")
    tag = uuid.uuid4().hex[:8]
    ids = {}
    try:
        with repo.engine.begin() as conn:
            for who in ("alice", "bob"):
                ids[who] = conn.execute(
                    text("INSERT INTO app_users (name, email, password_hash) VALUES (:n, :e, 'x') RETURNING user_id"),
                    {"n": who, "e": f"{who}-{tag}@example.com"},
                ).scalar()
            alice_orders = [
                conn.execute(
                    text(
                        "INSERT INTO user_purchases (user_id, product_id, predicted_usd, purchased_at) "
                        "VALUES (:u, :p, :usd, now() - make_interval(days => :d)) RETURNING id"
                    ),
                    {"u": ids["alice"], "p": "P00025442", "usd": 10.0 * (d + 1), "d": d},
                ).scalar()
                for d in (10, 0)
            ]
            ids["alice_old"], ids["alice_new"] = alice_orders
    except Exception as exc:
        pytest.skip(f"PostgreSQL unavailable: {exc}")
    yield tool, ids
    with repo.engine.begin() as conn:
        conn.execute(text("DELETE FROM app_users WHERE user_id IN (:a, :b)"), {"a": ids["alice"], "b": ids["bob"]})


def test_owner_sees_own_order(orders):
    tool, ids = orders
    res = tool.track_order(f"ORD-{ids['alice_old']}", str(ids["alice"]))
    assert res.order_id == f"ORD-{ids['alice_old']}"
    assert res.status == "Delivered"


def test_other_user_cannot_read_order_by_id(orders):
    tool, ids = orders
    res = tool.track_order(f"ORD-{ids['alice_old']}", str(ids["bob"]))
    assert res.status == "Not Found"
    assert res.items == []


def test_other_user_does_not_get_a_different_order_under_the_requested_id(orders):
    tool, ids = orders
    # Bob has no purchases: the old code fell back to "the user's latest purchase" for any unknown id
    res = tool.track_order("ORD-999999999", str(ids["alice"]))
    assert res.status == "Not Found"


def test_no_order_id_means_latest_own_order(orders):
    tool, ids = orders
    res = tool.track_order(None, str(ids["alice"]))
    assert res.order_id == f"ORD-{ids['alice_new']}"
    assert tool.track_order(None, str(ids["bob"])).status == "Not Found"


def test_guest_cannot_read_real_orders(orders):
    tool, ids = orders
    assert tool.track_order(f"ORD-{ids['alice_new']}", "guest_abc123").status == "Not Found"
    assert tool.track_order(f"ORD-{ids['alice_new']}", None).status == "Not Found"


def test_former_demo_order_ids_are_not_answered_for_everyone():
    tool = OrderSupportTool()
    for order_id in ("ORD-9842", "ORD-1102", "ORD-5541"):
        assert tool.track_order(order_id, "guest_x").status == "Not Found"
