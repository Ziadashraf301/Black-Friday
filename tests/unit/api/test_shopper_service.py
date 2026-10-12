import sys
from unittest.mock import MagicMock

import pandas as pd

from apps.api.services.shopper_service import ShopperService


def test_same_product_different_demographics_get_own_quotes(monkeypatch):
    quotes = [
        {"product_id": "P1", "predicted_usd": 10.0, "model_used": "m"},
        {"product_id": "P1", "predicted_usd": 20.0, "model_used": "m"},
    ]
    monkeypatch.setattr(ShopperService, "estimate_price_batch", classmethod(lambda cls, **kw: quotes))
    repo = MagicMock()
    repo.record_purchases_batch.side_effect = lambda recs: [
        {"id": i, "product_id": r["product_id"], "predicted_usd": r["predicted_usd"],
         "model_used": r["model_used"], "purchased_at": "now"}
        for i, r in enumerate(recs)
    ]
    items = [
        {"product_id": "P1", "quantity": 1, "gender": "M"},
        {"product_id": "P1", "quantity": 2, "gender": "F"},
    ]
    out = ShopperService.process_batch_purchase(user_id=1, items=items, repo=repo)
    assert [r["predicted_usd"] for r in out] == [10.0, 20.0, 20.0]


def test_estimate_price_batch_keeps_alignment_with_items(monkeypatch):
    mod = sys.modules[ShopperService.__module__]

    cached = {"k": {"product_id": "P2", "predicted_usd": 1.0}}
    monkeypatch.setattr(mod.cache_manager, "mget_json", lambda keys: {k: cached.get(k) for k in keys})
    monkeypatch.setattr(mod, "make_price_cache_key", lambda pid, *a: "k" if pid == "P2" else f"miss:{pid}")
    monkeypatch.setattr(ShopperService, "get_curated_catalog", staticmethod(lambda repo=None: []))
    fake_df = pd.DataFrame([{"normalized": 0.5, "model_used": "m"}])
    monkeypatch.setattr(mod.model_service, "predict_price_batch_matrix", lambda df: fake_df)
    monkeypatch.setattr(mod.cache_manager, "mset_json", lambda *a, **k: None)
    repo = MagicMock()
    items = [
        {"product_id": "P1", "product_category_1": 1, "gender": "M", "age": "26-35", "occupation": 1,
         "city_category": "A", "stay_in_current_city_years": "1", "marital_status": 0},
        {"product_id": "P2", "product_category_1": 1},
    ]
    out = ShopperService.estimate_price_batch(items=items, repo=repo, user_demographics={})
    assert [q["product_id"] for q in out] == ["P1", "P2"]
