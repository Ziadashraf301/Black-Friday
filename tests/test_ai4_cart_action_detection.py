"""AI-4: cart intent words are matched as whole words, not substrings ("to" inside "total", "from" inside a sentence)."""

import pytest

from ai.nodes.cart_node import detect_cart_action


@pytest.mark.parametrize(
    "query, sizes, expected",
    [
        ("change my jacket from size l to xl", [], ("update_size", "L", "XL")),
        ("switch the size please", ["M", "L"], ("update_size", "M", "L")),
        ("add the jacket from the sale page to my cart", [], ("add", None, None)),
        ("add this to my cart in size m", ["M"], ("add", None, None)),
        ("remove the boots from my cart", [], ("remove", None, None)),
        ("take out the scarf", [], ("remove", None, None)),
        ("clear my cart", [], ("clear", None, None)),
        ("what is my cart total", [], ("get", None, None)),
        ("show my address book subtotal", [], ("get", None, None)),
        ("do you have a droplet print", [], ("get", None, None)),
    ],
)
def test_detect_cart_action(query, sizes, expected):
    assert detect_cart_action(query, sizes) == expected
