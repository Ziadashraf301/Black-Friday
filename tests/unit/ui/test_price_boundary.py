"""UI-7 leftovers: prices are validated once when the catalog is loaded, and every dollar amount uses one format."""

from reflex_app.state import PRICE_UNAVAILABLE, ShoppingState, _format_price, _usd, _validated_catalog

RAW = [
    {
        "product_id": "P1",
        "is_hero": True,
        "original_price": "99.9",
        "discounted_price": 49.9,
        "apriori_bundles": [{"product_id": "P2", "price": "x"}, {"product_id": "P3", "price": 12}],
        "item2vec_similars": [{"product_id": "P2", "price": -5}],
    },
    {"product_id": "P2", "original_price": None, "discounted_price": 0},
]


def test_catalog_prices_are_validated_once_at_the_boundary():
    p1, p2 = _validated_catalog(RAW)
    assert (p1["original_price"], p1["discounted_price"]) == (99.9, 49.9)  # numeric strings become floats
    assert [b["price"] for b in p1["apriori_bundles"]] == [None, 12.0]
    assert p1["item2vec_similars"][0]["price"] is None
    assert (p2["original_price"], p2["discounted_price"]) == (None, None)  # missing or zero is unavailable, never invented
    assert RAW[0]["original_price"] == "99.9"  # input untouched


def test_set_catalog_stores_validated_products():
    state = ShoppingState(_reflex_internal_init=True)
    state._set_catalog(RAW)
    assert state.hero_product["original_price"] == 99.9
    assert state.products[1]["discounted_price"] is None
    assert state.hero_discounted_price_display == "$49.90"


def test_one_dollar_format():
    assert _usd(0) == "$0.00" and _usd("12.5") == "$12.50"
    assert _format_price(None) == PRICE_UNAVAILABLE and _format_price(3) == "$3.00"
