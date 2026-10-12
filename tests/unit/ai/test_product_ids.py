import pytest

from ai.tools.product_ids import normalize_product_id


@pytest.mark.parametrize(
    "raw, expected",
    [(" p00025442 ", "P00025442"), ("25442", "P00025442"), ("P00025442", "P00025442"), ("", ""), (None, "")],
)
def test_normalize_product_id(raw, expected):
    assert normalize_product_id(raw) == expected
