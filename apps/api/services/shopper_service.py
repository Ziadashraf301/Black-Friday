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


class ShopperService:
    """Service handling catalog browsing, smart recommendations, pricing, and purchase transactions."""

    @staticmethod
    def get_catalog(limit: int, repo: BlackFridayRepository) -> List[Dict[str, Any]]:
        products = repo.get_top_network_products(limit=limit)
        return [
            {
                "product_id": p["product_id"],
                "order_count": int(p["order_count"]),
                "pagerank_score": float(p["pagerank_score"]),
            }
            for p in products
        ]

    @staticmethod
    def browse_product(product_id: str, repo: BlackFridayRepository) -> Dict[str, Any]:
        product = repo.get_product_recommendations(product_id)
        if not product:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail=f"Product '{product_id}' not found in catalog."
            )

        bundles = parse_recommendation_ids(product.get("top_bundle_recommendations"))
        similars = parse_recommendation_ids(product.get("item2vec_recommendations"))

        # Fallback ensuring customer always receives recommendations
        if not bundles or not similars:
            try:
                top_items = [p["product_id"] for p in repo.get_top_network_products(limit=12) if p["product_id"] != product_id]
            except Exception:
                top_items = ["P00025442", "P00057642", "P00112142", "P00237542", "P00265242", "P00145042"]
            if not bundles:
                bundles = [pid for pid in top_items if pid != product_id][:4]
            if not similars:
                similars = [pid for pid in reversed(top_items) if pid != product_id][:4]

        return {
            "product_id": product["product_id"],
            "order_count": int(product["order_count"]),
            "pagerank_score": float(product["pagerank_score"]),
            "hub_score": float(product["hub_score"]) if product.get("hub_score") else None,
            "authority_score": float(product["authority_score"]) if product.get("authority_score") else None,
            "top_associated_product": product.get("top_associated_product") or (bundles[0] if bundles else None),
            "highest_lift_rule": float(product["highest_lift_rule"]) if product.get("highest_lift_rule") else 1.85,
            "apriori_bundles": bundles,
            "item2vec_similar": similars,
        }

    @staticmethod
    def estimate_price(
        product_id: str,
        cat1: Optional[int],
        cat2: Optional[int],
        cat3: Optional[int],
        repo: BlackFridayRepository
    ) -> Dict[str, Any]:
        """Calculates personalized member price using local model pipeline."""
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

        return {
            "product_id": product_id,
            "predicted_usd": pred_res["usd"],
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
