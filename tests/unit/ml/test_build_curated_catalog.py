"""Curated catalogue rebuild: only data-derived fields change, and every value comes from a pipeline output."""

import json
from unittest.mock import MagicMock

import pandas as pd
import pytest

from ml.market_basket.item2vec import HAS_GENSIM, Item2VecRecommender
from ml.pipelines import build_curated_catalog as bcc


def _product(pid, price, **extra):
    return {
        "product_id": pid,
        "name": f"Name {pid}",
        "tagline": "hand written",
        "image_url": f"/products/{pid}.jpg",
        "original_price": price * 2,
        "discounted_price": price,
        "order_count": 999,
        "apriori_bundles": [{"product_id": "PX", "lift": 9.9, "confidence": 0.9}],
        "item2vec_similars": [],
        **extra,
    }


PRODUCTS = [_product("P001", 10.0, rating=4.8), _product("P002", 20.0), _product("P003", 30.0)]

RULES = pd.DataFrame(
    {
        "antecedents": ["frozenset({'P001'})", "frozenset({'P001'})", "frozenset({'P001', 'P002'})", "frozenset({'P002'})"],
        "consequents": ["frozenset({'P002'})", "frozenset({'P003'})", "frozenset({'P003'})", "frozenset({'P999'})"],
        "lift": [1.234, 1.9, 5.0, 3.0],
        "confidence": [0.456, 0.3, 0.9, 0.5],
    }
)


def test_bundles_come_only_from_one_to_one_rules_inside_the_catalogue():
    bundles = bcc.build_bundles(PRODUCTS, RULES)
    assert [b["product_id"] for b in bundles["P001"]] == ["P003", "P002"]  # by lift
    assert bundles["P001"][1] == {
        "product_id": "P002",
        "name": "Name P002",
        "image_url": "/products/P002.jpg",
        "price": 20.0,  # the referenced product's own catalogue price
        "lift": 1.23,
        "confidence": 0.46,
    }
    assert bundles["P002"] == []  # its only rule points outside the catalogue
    assert bundles["P003"] == []


def test_bundles_are_capped():
    many = pd.DataFrame(
        {
            "antecedents": ["frozenset({'P001'})"] * 2,
            "consequents": ["frozenset({'P002'})", "frozenset({'P003'})"],
            "lift": [2.0, 3.0],
            "confidence": [0.5, 0.5],
        }
    )
    assert [b["product_id"] for b in bcc.build_bundles(PRODUCTS, many, top_n=1)["P001"]] == ["P003"]


def test_similars_rank_catalogue_neighbours_and_skip_unknown_ids():
    table = {frozenset({"P001", "P002"}): 0.91234, frozenset({"P001", "P003"}): 0.5}

    def similarity(a, b):
        if "P003" in (a, b) and "P002" in (a, b):
            return None  # one side outside the model vocabulary
        return table.get(frozenset({a, b}))

    similars = bcc.build_similars(PRODUCTS, similarity, top_n=3)
    assert [(s["product_id"], s["similarity"], s["price"]) for s in similars["P001"]] == [
        ("P002", 0.912, 20.0),
        ("P003", 0.5, 30.0),
    ]
    assert [s["product_id"] for s in similars["P003"]] == ["P001"]


def test_rebuild_keeps_hand_written_fields_and_prices():
    rebuilt = bcc.rebuild_catalog(PRODUCTS, {"P001": 7}, {"P001": ["b"]}, {"P002": ["s"]})
    first = rebuilt[0]
    assert first["order_count"] == 7 and first["apriori_bundles"] == ["b"] and first["item2vec_similars"] == []
    assert rebuilt[1]["order_count"] == 0 and rebuilt[1]["item2vec_similars"] == ["s"]
    for before, after in zip(PRODUCTS, rebuilt):
        for key in before:
            if key not in ("order_count", "apriori_bundles", "item2vec_similars"):
                assert after[key] == before[key]
    assert list(first) == list(PRODUCTS[0])  # key order kept, so the JSON diff stays small
    assert PRODUCTS[0]["order_count"] == 999  # input not mutated


@pytest.mark.skipif(not HAS_GENSIM, reason="gensim not installed")
def test_run_end_to_end_with_a_saved_model(tmp_path):
    baskets = [["P001", "P002"], ["P001", "P002", "P003"], ["P002", "P003"]] * 20
    model_path = (
        Item2VecRecommender(vector_size=8, window=0, min_count=1, epochs=2).fit(baskets).save(tmp_path / "m" / "i2v.model")
    )
    loaded = Item2VecRecommender.load(model_path)
    assert loaded.vector_size == 8 and set(loaded.vocabulary) == {"P001", "P002", "P003"}

    catalog = tmp_path / "catalog.json"
    catalog.write_text(json.dumps(PRODUCTS), encoding="utf-8")
    rules = tmp_path / "rules.csv"
    RULES.to_csv(rules, index=False)
    repo = MagicMock()
    repo.get_product_order_counts.return_value = {"P001": 5, "P002": 6, "P003": 7}

    out = tmp_path / "out.json"
    bcc.run(catalog_path=catalog, rules_path=rules, model_path=model_path, output_path=out, repo=repo)
    written = json.loads(out.read_text(encoding="utf-8"))
    assert [p["order_count"] for p in written] == [5, 6, 7]
    assert all(len(p["item2vec_similars"]) == 2 for p in written)
    assert written[0]["rating"] == 4.8  # untouched field
    assert json.loads(catalog.read_text(encoding="utf-8")) == PRODUCTS  # input file untouched when --output is used


def test_run_asks_for_make_basket_when_the_model_is_missing(tmp_path):
    rules = tmp_path / "rules.csv"
    RULES.to_csv(rules, index=False)
    with pytest.raises(FileNotFoundError, match="make basket"):
        bcc.run(rules_path=rules, model_path=tmp_path / "missing.model", repo=MagicMock())
