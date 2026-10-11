"""CORE-11: there is no inventory, so the availability check is honestly named and takes no quantity."""

import inspect
from unittest.mock import MagicMock

from core.db.repositories.warehouse_repo import WarehouseRepository


def _repo_with_row(row):
    engine = MagicMock()
    engine.connect.return_value.__enter__.return_value.execute.return_value.fetchone.return_value = row
    return WarehouseRepository(engine=engine)


def test_signature_has_no_quantity():
    assert list(inspect.signature(WarehouseRepository.is_size_offered).parameters) == ["self", "product_id", "size"]
    assert not hasattr(WarehouseRepository, "check_product_stock")


def test_unknown_product_is_not_offered():
    assert _repo_with_row(None).is_size_offered("P0") is False


def test_listed_size_is_offered_case_insensitively():
    repo = _repo_with_row((["S", "M"],))
    assert repo.is_size_offered("p1", "m") is True
    assert repo.is_size_offered("p1", "XL") is False
    assert repo.is_size_offered("p1") is True
