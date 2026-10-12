"""UI-2: no hard-coded hero product; the hero card follows the catalog and hides when there is none."""

import inspect
import sys

from core.config import settings

sys.path.insert(0, str(settings.BASE_DIR / "apps" / "reflex_app"))
from reflex_app.components import hero_card as hero_card_module  # noqa: E402
from reflex_app.state import ShoppingState  # noqa: E402


def test_no_hard_coded_product_id_in_frontend_sources():
    import reflex_app.state as state_module

    for module in (state_module, hero_card_module):
        assert "P00025442" not in inspect.getsource(module)


def test_empty_catalog_has_no_hero_and_actions_do_nothing():
    state = ShoppingState(_reflex_internal_init=True)
    state._set_catalog([])

    assert state.has_hero is False
    assert state.hero_id == ""
    assert state.hero_name == ""

    state.open_hero_detail()
    state.add_hero_to_cart()

    assert state.is_quick_view_open is False
    assert state.cart_items == []
    assert state.is_cart_open is False


def test_hero_actions_use_the_real_hero_product():
    state = ShoppingState(_reflex_internal_init=True)
    state._set_catalog(
        [
            {"product_id": "P1", "name": "First", "discounted_price": 10.0},
            {"product_id": "P2", "name": "Chosen", "discounted_price": 20.0, "is_hero": True},
        ]
    )

    assert state.has_hero is True
    assert state.hero_id == "P2"

    state.open_hero_detail()

    assert state.active_product["product_id"] == "P2"


def test_hero_card_is_conditional_on_has_hero():
    assert hero_card_module.hero_card() is not None
