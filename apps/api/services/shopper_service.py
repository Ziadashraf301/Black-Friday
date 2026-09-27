"""
Shopper service — product catalog, browsing with smart recommendations, price quotes, and purchase management.
"""
import json
import re
from typing import Dict, Any, List, Optional
from fastapi import HTTPException, status

from core.db.repository import BlackFridayRepository
from apps.api.services.model_service import model_service
from core.logging import get_logger

from cachetools import TTLCache
from pathlib import Path
from core.config import settings

logger = get_logger(__name__)


def parse_recommendation_ids(value: Any) -> List[str]:
    """Safely parses stored JSON list or string into list of clean product IDs."""
    if value is None:
        return []

    def _extract(v):
        if isinstance(v, dict):
            return str(v.get("product_id") or v.get("item") or v)
        return str(v)

    if isinstance(value, list):
        return [_extract(v) for v in value]

    s = str(value).strip()
    if not s or s in ("[]", "null", "None"):
        return []

    # Strip extra quotes and doubled CSV quotes
    while (s.startswith('"') and s.endswith('"')) or (s.startswith("'") and s.endswith("'")):
        s = s[1:-1].strip()
    s = s.replace('""', '"')

    try:
        parsed = json.loads(s)
        if isinstance(parsed, str):
            parsed = json.loads(parsed)
        if isinstance(parsed, list):
            return [_extract(v) for v in parsed]
        if isinstance(parsed, dict):
            return list(parsed.keys())
    except Exception:
        pass

    matches = re.findall(r'P\d+', s)
    return matches if matches else []


_curated_cache = TTLCache(maxsize=10, ttl=3600)
_catalog_cache = TTLCache(maxsize=100, ttl=600)
_browse_cache = TTLCache(maxsize=500, ttl=600)


class ShopperService:
    """Service handling catalog browsing, smart recommendations, pricing, and purchase transactions."""

    @staticmethod
    def get_curated_catalog() -> List[Dict[str, Any]]:
        """Returns the high-performance enriched curated products catalog with zero-copy caching."""
        if "curated" in _curated_cache:
            return _curated_cache["curated"]
        
        json_path = settings.BASE_DIR / "data" / "curated_products.json"
        if json_path.exists():
            with open(json_path, "r", encoding="utf-8") as f:
                data = json.load(f)
                _curated_cache["curated"] = data
                return data
        return []

    @staticmethod
    def get_catalog(limit: int, repo: BlackFridayRepository) -> List[Dict[str, Any]]:
        cache_key = f"catalog_{limit}"
        if cache_key in _catalog_cache:
            return _catalog_cache[cache_key]

        products = repo.get_top_network_products(limit=limit)
        res = [
            {
                "product_id": p["product_id"],
                "order_count": int(p["order_count"]),
                "pagerank_score": float(p["pagerank_score"]),
            }
            for p in products
        ]
        _catalog_cache[cache_key] = res
        return res

    @staticmethod
    def browse_product(product_id: str, repo: BlackFridayRepository) -> Dict[str, Any]:
        """Fetches product graph centrality metrics and bundle associations with TTL caching."""
        if product_id in _browse_cache:
            return _browse_cache[product_id]

        product = repo.get_product_recommendations(product_id)
        if not product:
            curated = ShopperService.get_curated_catalog()
            match = next((p for p in curated if p["product_id"] == product_id), None)
            if match:
                res = {
                    "product_id": match["product_id"],
                    "order_count": int(match.get("order_count", 0)),
                    "pagerank_score": 0.0,
                    "hub_score": None,
                    "authority_score": None,
                    "top_associated_product": match["apriori_bundles"][0]["product_id"] if match.get("apriori_bundles") else None,
                    "highest_lift_rule": float(match["apriori_bundles"][0]["lift"]) if match.get("apriori_bundles") and match["apriori_bundles"][0].get("lift") else None,
                    "apriori_bundles": [b["product_id"] for b in match.get("apriori_bundles", [])],
                    "item2vec_similar": [s["product_id"] for s in match.get("item2vec_similars", [])],
                }
                _browse_cache[product_id] = res
                return res

            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail=f"Product '{product_id}' not found in catalog."
            )

        bundles = parse_recommendation_ids(product.get("top_bundle_recommendations"))
        similars = parse_recommendation_ids(product.get("item2vec_recommendations"))

        # If bundle associations are empty in the row, dynamically retrieve top network neighbors
        if not bundles or not similars:
            network_neighbors = repo.get_top_network_products(limit=12)
            valid_ids = [p["product_id"] for p in network_neighbors if p["product_id"] != product_id]
            if not bundles:
                bundles = valid_ids[:4]
            if not similars:
                similars = list(reversed(valid_ids))[:4]

        res = {
            "product_id": product["product_id"],
            "order_count": int(product["order_count"]),
            "pagerank_score": float(product["pagerank_score"]),
            "hub_score": float(product["hub_score"]) if product.get("hub_score") is not None else None,
            "authority_score": float(product["authority_score"]) if product.get("authority_score") is not None else None,
            "top_associated_product": product.get("top_associated_product") or (bundles[0] if bundles else None),
            "highest_lift_rule": float(product["highest_lift_rule"]) if product.get("highest_lift_rule") is not None else None,
            "apriori_bundles": bundles,
            "item2vec_similar": similars,
        }
        _browse_cache[product_id] = res
        return res

    @classmethod
    def estimate_price_batch(
        cls,
        items: List[Dict[str, Any]],
        repo: BlackFridayRepository
    ) -> List[Dict[str, Any]]:
        """Batch-processes price predictions for high-throughput shopping carts."""
        results = []
        for item in items:
            product_id = item.get("product_id") or ""
            cat1 = item.get("product_category_1")
            cat2 = item.get("product_category_2")
            cat3 = item.get("product_category_3")
            results.append(cls.estimate_price(product_id, cat1, cat2, cat3, repo))
        return results

    @staticmethod
    def estimate_price(
        product_id: str,
        cat1: Optional[int],
        cat2: Optional[int],
        cat3: Optional[int],
        repo: BlackFridayRepository
    ) -> Dict[str, Any]:
        """Calculates personalized member price using local ONNX model pipeline.
        
        The member price represents an exclusive personalized discount calibrated to
        the product's catalog price using the customer's propensity prediction.
        Guaranteed to be lower than the regular catalog price.
        """
        if cat1 is None:
            db_cats = repo.get_product_categories(product_id)
            if db_cats:
                cat1 = int(db_cats["product_category_1"] or 1)
                cat2 = cat2 or (int(db_cats["product_category_2"]) if db_cats.get("product_category_2") else None)
                cat3 = cat3 or (int(db_cats["product_category_3"]) if db_cats.get("product_category_3") else None)
            else:
                cat1 = 1

        pred_res = model_service.predict_price(
            product_id=product_id,
            cat1=cat1,
            cat2=cat2,
            cat3=cat3
        )

        # Lookup catalog base price
        curated = ShopperService.get_curated_catalog()
        match = next((p for p in curated if p["product_id"] == product_id), None)
        base_price = float(match.get("discounted_price", 49.90)) if match else 49.90

        # Calibrate member personalized price:
        # norm is between 0.0 and 1.0 (mean ~0.51).
        # Member receives a 12% to 28% discount off the catalog discounted price.
        norm = max(0.0, min(1.0, float(pred_res.get("normalized", 0.5))))
        member_discount_factor = 0.72 + 0.16 * norm  # range [0.72, 0.88]
        member_price = round(base_price * member_discount_factor, 2)

        # Enforce that member price is strictly lower than catalog price
        if member_price >= base_price:
            member_price = round(base_price * 0.85, 2)

        return {
            "product_id": product_id,
            "predicted_usd": member_price,
            "catalog_price": base_price,
            "normalized_prediction": pred_res["normalized"],
            "model_used": pred_res["model_used"],
        }

    @classmethod
    def process_purchase(
        cls,
        user_id: int,
        product_id: str,
        cat1: Optional[int],
        cat2: Optional[int],
        cat3: Optional[int],
        repo: BlackFridayRepository
    ) -> Dict[str, Any]:
        """Executes price calculation and stores purchase record in database."""
        try:
            repo.create_app_tables()
        except Exception:
            pass

        quote = cls.estimate_price(product_id, cat1, cat2, cat3, repo)

        record = repo.record_purchase({
            "user_id": user_id,
            "product_id": product_id,
            "product_category_1": cat1 or 1,
            "product_category_2": cat2,
            "product_category_3": cat3,
            "predicted_usd": quote["predicted_usd"],
            "model_used": quote["model_used"],
        })

        return {
            "id": record["id"],
            "product_id": record["product_id"],
            "predicted_usd": float(record["predicted_usd"]),
            "model_used": record["model_used"],
            "purchased_at": str(record["purchased_at"]),
        }

    @staticmethod
    def get_purchase_history(user_id: int, repo: BlackFridayRepository) -> Dict[str, Any]:
        purchases = repo.get_user_purchase_history(user_id)
        serialized = [
            {**p, "purchased_at": str(p["purchased_at"])} for p in purchases
        ]
        return {
            "user_id": user_id,
            "total_purchases": len(serialized),
            "purchases": serialized,
        }


shopper_service = ShopperService()
