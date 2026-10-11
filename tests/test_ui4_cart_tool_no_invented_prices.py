"""UI-4: the cart tool never adds a product at a made-up price."""

import uuid
from unittest.mock import MagicMock, patch

import pytest

from ai.extractor.catalog_index import CatalogIndex
from ai.tools.cart_tools import CartManagementTool
from core.exceptions import DomainError

PID = "P00000001"


def _tool(repo_product=None):
    repo = MagicMock()
    repo.get_curated_product_by_id.return_value = repo_product
    repo.is_size_offered.return_value = True
    tool = CartManagementTool(repository=repo, cache=MagicMock(is_available=False))
    return tool


@pytest.fixture
def user():
    return f"ui4-{uuid.uuid4().hex}"


def test_repo_product_without_price_is_rejected_not_priced_at_a_default(user):
    tool = _tool({"product_id": PID, "name": "No price", "original_price": 80.0})
    with patch.dict(CatalogIndex._ID_TO_PRODUCT, {}, clear=False):
        with pytest.raises(DomainError) as exc:
            tool.modify_cart(user, "s", action="add", product_id=PID)
    assert exc.value.code == "PRICE_UNAVAILABLE"
    assert tool.get_cart(user, "s").items == []


def test_catalog_row_with_null_or_zero_price_is_rejected(user):
    tool = _tool(None)
    for bad in (None, 0, "n/a"):
        with patch.dict(CatalogIndex._ID_TO_PRODUCT, {PID: {"name": "Bad", "discounted_price": bad}}):
            with pytest.raises(DomainError):
                tool.modify_cart(user, "s", action="add", product_id=PID)


def test_catalog_price_is_used_when_the_repository_row_has_none(user):
    tool = _tool({"product_id": PID, "name": "No price"})
    with patch.dict(CatalogIndex._ID_TO_PRODUCT, {PID: {"name": "Cat", "discounted_price": 42.5}}):
        cart = tool.modify_cart(user, "s", action="add", product_id=PID)
    assert [(i.product_id, i.unit_price) for i in cart.items] == [(PID, 42.5)]


def test_priced_repo_product_is_added_at_its_price(user):
    tool = _tool({"product_id": PID, "name": "Real", "discounted_price": 30.0, "original_price": 60.0})
    cart = tool.modify_cart(user, "s", action="add", product_id=PID, quantity=2)
    assert cart.subtotal == 60.0


def test_unknown_product_is_still_just_unknown(user):
    tool = _tool(None)
    assert tool._lookup_product_info("P99999999") is None
