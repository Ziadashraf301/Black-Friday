"""UI-3: the Reflex state never substitutes made-up prices; a missing price shows as unavailable."""

import sys
from unittest.mock import MagicMock, patch

from core.config import settings

sys.path.insert(0, str(settings.BASE_DIR / "apps" / "reflex_app"))
from reflex_app.state import CartItem, ShoppingState  # noqa: E402


def test_product_without_a_price_is_not_added_to_the_cart():
    state = ShoppingState(_reflex_internal_init=True)
    state.products = [{"product_id": "P1", "name": "No price"}]

    state.add_to_cart("P1")

    assert state.cart_items == []


def test_original_price_is_used_only_when_the_sale_price_is_missing():
    state = ShoppingState(_reflex_internal_init=True)
    state.products = [{"product_id": "P1", "name": "Full price", "original_price": 80.0}]

    state.add_to_cart("P1")

    assert [i.price for i in state.cart_items] == [80.0]


def test_hero_and_quick_view_show_price_unavailable_when_prices_are_missing():
    state = ShoppingState(_reflex_internal_init=True)
    state._set_catalog([{"product_id": "P1", "name": "No price"}])

    assert state.hero_discounted_price_display == "Price unavailable"
    assert state.hero_original_price_display == ""

    state.open_product_detail("P1")

    assert state.active_sale_price_display == "Price unavailable"
    assert state.active_estimated_price_display == "Price unavailable"
    assert state.active_original_price_display == ""
    assert state.price_prediction_model == "Price unavailable"


def test_known_prices_still_format():
    state = ShoppingState(_reflex_internal_init=True)
    state._set_catalog([{"product_id": "P1", "discounted_price": 49.9, "original_price": 99.9}])

    assert state.hero_discounted_price_display == "$49.90"
    assert state.hero_original_price_display == "$99.90"


def test_recommendation_without_a_price_has_no_invented_one():
    state = ShoppingState(_reflex_internal_init=True)
    state.active_product = {
        "product_id": "P1",
        "apriori_bundles": [{"product_id": "P2", "name": "Bundle"}, {"product_id": "P3", "price": 12.5}],
    }

    recs = {r.product_id: r for r in state.active_apriori_bundles_list}

    assert recs["P2"].price_display == "Price unavailable"
    assert recs["P3"].price_display == "$12.50"


def test_api_fetched_product_carries_no_invented_prices():
    state = ShoppingState(_reflex_internal_init=True)
    response = MagicMock(status_code=200)
    response.json.return_value = {"order_count": 3, "apriori_bundles": ["P2"], "item2vec_similar": ["P3"]}
    client = MagicMock()
    client.__enter__.return_value.get.return_value = response

    with patch("reflex_app.state.httpx.Client", return_value=client):
        product = state._fetch_product_from_api("P9")

    assert "original_price" not in product and "discounted_price" not in product
    assert all("price" not in r for r in product["apriori_bundles"] + product["item2vec_similars"])


def test_reprice_keeps_catalog_price_for_items_without_a_quote():
    state = ShoppingState(_reflex_internal_init=True)
    state.auth_token = "token"
    state.cart_items = [
        CartItem(key="P1_M", product_id="P1", price=100.0),
        CartItem(key="P2_M", product_id="P2", price=60.0),
    ]
    response = MagicMock(status_code=200)
    response.json.return_value = {"quotes": [{"product_id": "P1", "predicted_usd": 70.0}]}
    client = MagicMock()
    client.__enter__.return_value.post.return_value = response

    with patch("reflex_app.state.httpx.Client", return_value=client):
        state._reprice_cart()

    quoted, unquoted = state.cart_items
    assert (quoted.has_personalized, quoted.personalized_price) == (True, 70.0)
    assert unquoted.has_personalized is False
    assert unquoted.personalized_price == 0.0
    assert state.cart_personalized_total == 70.0 + 60.0
