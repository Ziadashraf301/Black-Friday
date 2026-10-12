"""
Rebuilds the data-derived fields of data/curated_products.json from the pipeline outputs.

- order_count:        purchase rows per product in the warehouse (black_friday_cleaned)
- apriori_bundles:    single-item Apriori rules product -> other catalogue product (reports/market_basket/apriori_rules.csv)
- item2vec_similars:  nearest catalogue products in the Item2Vec model saved by `make basket`

Everything else (names, taglines, descriptions, images, brand, sizes, badges and the USD prices) is hand-maintained
and left unchanged. Prices shown inside bundles and similars are copied from the referenced product's own entry.

Run `make catalog`, then `make seed` (or `python -m ml.pipelines.seed_curated`) to load the file into PostgreSQL.
"""

import argparse
import json
import re
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional

import pandas as pd

from core.config import settings
from core.db.repository import BlackFridayRepository
from core.logging import get_logger
from ml.market_basket.item2vec import Item2VecRecommender
from ml.pipelines.common import reports_dir

logger = get_logger(__name__)

MAX_BUNDLES = 3
MAX_SIMILARS = 3
_PRODUCT_ID = re.compile(r"P\d+")


def _items(value: Any) -> List[str]:
    """Product ids of a rule side: a frozenset from mlxtend, or its string form read back from the CSV."""
    if isinstance(value, (set, frozenset, list, tuple)):
        return [str(v) for v in value]
    return _PRODUCT_ID.findall(str(value))


def _ref(product: Dict[str, Any]) -> Dict[str, Any]:
    return {
        "product_id": product["product_id"],
        "name": product["name"],
        "image_url": product["image_url"],
        "price": product["discounted_price"],
    }


def build_bundles(products: List[Dict[str, Any]], rules_df: pd.DataFrame, top_n: int = MAX_BUNDLES) -> Dict[str, list]:
    """For each product, the strongest one-to-one rules (by lift, then confidence) whose consequent is in the catalogue."""
    by_id = {p["product_id"]: p for p in products}
    candidates: Dict[str, list] = {pid: [] for pid in by_id}
    for _, rule in rules_df.iterrows():
        ants, cons = _items(rule["antecedents"]), _items(rule["consequents"])
        if len(ants) != 1 or len(cons) != 1:
            continue
        src, dst = ants[0], cons[0]
        if src in by_id and dst in by_id and src != dst:
            candidates[src].append((float(rule["lift"]), float(rule["confidence"]), dst))
    bundles = {}
    for pid, rows in candidates.items():
        rows.sort(key=lambda r: (-r[0], -r[1], r[2]))
        bundles[pid] = [
            {**_ref(by_id[dst]), "lift": round(lift, 2), "confidence": round(conf, 2)} for lift, conf, dst in rows[:top_n]
        ]
    return bundles


def build_similars(
    products: List[Dict[str, Any]],
    similarity: Callable[[str, str], Optional[float]],
    top_n: int = MAX_SIMILARS,
) -> Dict[str, list]:
    """For each product, the most similar other catalogue products; `similarity` returns None when either id is unknown."""
    by_id = {p["product_id"]: p for p in products}
    similars = {}
    for pid in by_id:
        scored = []
        for other in by_id:
            if other == pid:
                continue
            sim = similarity(pid, other)
            if sim is not None:
                scored.append((sim, other))
        scored.sort(key=lambda s: (-s[0], s[1]))
        similars[pid] = [{**_ref(by_id[o]), "similarity": round(sim, 3)} for sim, o in scored[:top_n]]
    return similars


def rebuild_catalog(
    products: List[Dict[str, Any]],
    order_counts: Dict[str, int],
    bundles: Dict[str, list],
    similars: Dict[str, list],
) -> List[Dict[str, Any]]:
    """Returns new product dicts with only the data-derived fields replaced (key order preserved)."""
    rebuilt = []
    for product in products:
        pid = product["product_id"]
        new = dict(product)
        new["order_count"] = int(order_counts.get(pid, 0))
        new["apriori_bundles"] = bundles.get(pid, [])
        new["item2vec_similars"] = similars.get(pid, [])
        rebuilt.append(new)
    return rebuilt


def _model_similarity(model: Item2VecRecommender) -> Callable[[str, str], Optional[float]]:
    wv = model.model.wv

    def similarity(a: str, b: str) -> Optional[float]:
        if a not in wv or b not in wv:
            return None
        return float(wv.similarity(a, b))

    return similarity


def run(
    catalog_path: Optional[Path] = None,
    rules_path: Optional[Path] = None,
    model_path: Optional[Path] = None,
    output_path: Optional[Path] = None,
    repo: Optional[BlackFridayRepository] = None,
) -> List[Dict[str, Any]]:
    catalog_path = catalog_path or settings.BASE_DIR / "data" / "curated_products.json"
    rules_path = rules_path or Path(reports_dir("market_basket")) / "apriori_rules.csv"
    model_path = model_path or settings.resolved_item2vec_model_path
    output_path = output_path or catalog_path
    for path, hint in ((rules_path, "make basket"), (model_path, "make basket")):
        if not Path(path).exists():
            raise FileNotFoundError(f"{path} not found; run `{hint}` first.")

    with open(catalog_path, "r", encoding="utf-8") as f:
        products = json.load(f)

    order_counts = (repo or BlackFridayRepository()).get_product_order_counts()
    missing = [p["product_id"] for p in products if p["product_id"] not in order_counts]
    if missing:
        logger.warning(f"[CATALOG] {len(missing)} catalogue products have no warehouse purchases: {missing}")

    bundles = build_bundles(products, pd.read_csv(rules_path))
    similars = build_similars(products, _model_similarity(Item2VecRecommender.load(model_path)))
    rebuilt = rebuild_catalog(products, order_counts, bundles, similars)

    with open(output_path, "w", encoding="utf-8") as f:
        json.dump(rebuilt, f, indent=2, ensure_ascii=False)
        f.write("\n")
    logger.info(
        f"[CATALOG] Rebuilt {len(rebuilt)} products -> {output_path} "
        f"({sum(len(b) for b in bundles.values())} bundles, {sum(len(s) for s in similars.values())} similars)"
    )
    return rebuilt


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Rebuild data-derived fields of the curated catalogue.")
    parser.add_argument("--output", type=Path, default=None, help="Write here instead of overwriting the catalogue")
    run(output_path=parser.parse_args().output)
