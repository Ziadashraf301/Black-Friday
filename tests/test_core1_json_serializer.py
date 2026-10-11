"""CORE-1: the Redis JSON serializer must terminate on arbitrary objects (e.g. MagicMock) and unwrap numpy types."""

import datetime
import decimal
import json
from unittest.mock import MagicMock

import numpy as np

from core.cache.redis_client import _json_serializer


def _dumps(value):
    return json.dumps(value, default=_json_serializer)


def test_numpy_scalars_and_arrays_are_unwrapped():
    payload = {"i": np.int64(3), "f": np.float32(1.5), "b": np.bool_(True), "a": np.array([1, 2])}
    assert json.loads(_dumps(payload)) == {"i": 3, "f": 1.5, "b": True, "a": [1, 2]}


def test_decimal_and_dates_are_converted():
    out = json.loads(_dumps({"d": decimal.Decimal("1.5"), "t": datetime.date(2026, 1, 2)}))
    assert out == {"d": 1.5, "t": "2026-01-02"}


def test_object_with_item_attribute_does_not_hang():
    out = json.loads(_dumps({"m": MagicMock()}))
    assert isinstance(out["m"], str)
