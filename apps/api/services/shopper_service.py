"""
Shopper service — product catalog, browsing with smart recommendations, price quotes, and purchase management.
"""
import json
import re
import pandas as pd
import numpy as np
from typing import Dict, Any, List, Optional
from fastapi import HTTPException, status

from core.db.repository import BlackFridayRepository
from apps.api.services.model_service import model_service
from apps.api.services.helpers import (
    parse_recommendation_ids,
    make_price_cache_key,
    resolve_item_demographics,
    calculate_member_discount_price,
)
from core.cache import cache_manager
from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)



class ShopperService:
    """Service handling catalog browsing, smart recommendations, pricing, and purchase transactions."""

    @staticmethod
    def get_curated_catalog(repo: Optional[BlackFridayRepository] = None) -> List[Dict[str, Any]]:
        """Returns the curated products catalog with persistent 6-hour Redis caching."""
        cache_key = "shopper:curated_catalog"
        cached = cache_manager.get_json(cache_key)
        if cached:
            return cached

        db_repo = repo or BlackFridayRepository()
        data = db_repo.get_curated_products()
        if not data:
            json_path = settings.BASE_DIR / "data" / "curated_products.json"
            if json_path.exists():
                with open(json_path, "r", encoding="utf-8") as f:
                    data = json.load(f)

        if data:
            cache_manager.set_json(cache_key, data, ttl=settings.REDIS_DEFAULT_TTL)
        return data or []

    @staticmethod
    def get_catalog(limit: int, repo: BlackFridayRepository) -> List[Dict[str, Any]]:
        """Returns top network products with persistent 6-hour Redis caching."""
        cache_key = f"shopper:catalog:{limit}"
        cached = cache_manager.get_json(cache_key)
        if cached:
            return cached

        products = repo.get_top_network_products(limit=limit)
        res = [
            {
                "product_id": p["product_id"],
                "order_count": int(p["order_count"]),
                "pagerank_score": float(p["pagerank_score"]),
            }
            for p in products
        ]
        cache_manager.set_json(cache_key, res, ttl=settings.REDIS_DEFAULT_TTL)
        return res

    @staticmethod
    def browse_product(product_id: str, repo: BlackFridayRepository) -> Dict[str, Any]:
        """Fetches product graph centrality metrics and bundle associations with 6-hour Redis caching."""
        cache_key = f"shopper:browse:{product_id}"
        cached = cache_manager.get_json(cache_key)
        if cached:
            return cached

        product = repo.get_product_recommendations(product_id)
        if not product:
            curated = ShopperService.get_curated_catalog(repo=repo)
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
                cache_manager.set_json(cache_key, res, ttl=settings.REDIS_DEFAULT_TTL)
                return res

            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail=f"Product '{product_id}' not found in catalog."
            )

        bundles = parse_recommendation_ids(product.get("top_bundle_recommendations"))
        similars = parse_recommendation_ids(product.get("item2vec_recommendations"))

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
        cache_manager.set_json(cache_key, res, ttl=settings.REDIS_DEFAULT_TTL)
        return res

    @classmethod
    def estimate_price_batch(
        cls,
        items: List[Dict[str, Any]],
        repo: BlackFridayRepository,
        user_id: Optional[int] = None,
        user_demographics: Optional[Dict[str, Any]] = None,
    ) -> List[Dict[str, Any]]:
        """
        High-performance vectorized batch price quote calculation.
        Uses Redis mget for fast cache hits and 2D ONNX C++ matrix prediction for uncached items.
        """
        if not items:
            return []

        if not user_demographics and user_id is not None:
            user_demographics = repo.get_user_demographics(user_id)

        # 1. Prepare item feature dictionary & construct Redis cache keys for all items
        item_keys: List[str] = []
        item_metas: List[Dict[str, Any]] = []

        # Pre-fetch categories in ONE bulk query for all items missing product_category_1
        missing_cat_pids = [
            item.get("product_id") for item in items if item.get("product_category_1") is None and item.get("product_id")
        ]
        cat_map: Dict[str, Dict[str, Any]] = {}
        if missing_cat_pids:
            cat_map = repo.get_bulk_product_categories(missing_cat_pids)

        for item in items:
            product_id = item.get("product_id") or ""
            demo = resolve_item_demographics(item, user_demographics)
            gender = demo["gender"]
            age = demo["age"]
            occ = demo["occupation"]
            city = demo["city_category"]
            stay = demo["stay_in_current_city_years"]
            mar = demo["marital_status"]

            cat1 = item.get("product_category_1")
            cat2 = item.get("product_category_2")
            cat3 = item.get("product_category_3")

            if cat1 is None:
                db_cats = cat_map.get(product_id)
                if db_cats:
                    cat1 = int(db_cats["product_category_1"] or 1)
                    cat2 = cat2 or (int(db_cats["product_category_2"]) if db_cats.get("product_category_2") else None)
                    cat3 = cat3 or (int(db_cats["product_category_3"]) if db_cats.get("product_category_3") else None)
                else:
                    cat1 = 1

            ckey = make_price_cache_key(product_id, gender, age, occ, city, stay, mar)
            item_keys.append(ckey)
            item_metas.append({
                "product_id": product_id,
                "cat1": cat1,
                "cat2": cat2,
                "cat3": cat3,
                "gender": gender,
                "age": age,
                "occupation": occ,
                "city_category": city,
                "stay_in_current_city_years": stay,
                "marital_status": mar,
                "cache_key": ckey,
            })

        # 2. Multi-GET from Redis for all items in single network call
        cached_results = cache_manager.mget_json(item_keys)
        final_quotes: List[Optional[Dict[str, Any]]] = [cached_results.get(ckey) for ckey in item_keys]

        # 3. Identify uncached items indices
        uncached_indices = [idx for idx, quote in enumerate(final_quotes) if quote is None]

        if uncached_indices:
            # Load curated catalog to get base discounted prices
            curated = ShopperService.get_curated_catalog(repo=repo)
            price_map = {p["product_id"]: float(p.get("discounted_price", 49.90)) for p in curated}

            # Construct 2D DataFrame matrix for all uncached items
            matrix_rows = []
            for idx in uncached_indices:
                meta = item_metas[idx]
                matrix_rows.append({
                    "gender": str(meta["gender"] or "M"),
                    "age": str(meta["age"] or "26-35"),
                    "occupation": int(meta["occupation"] if meta["occupation"] is not None else 4),
                    "city_category": str(meta["city_category"] or "A"),
                    "stay_in_current_city_years": str(meta["stay_in_current_city_years"] or "1"),
                    "marital_status": int(meta["marital_status"] if meta["marital_status"] is not None else 0),
                    "product_category_1": int(meta["cat1"]),
                    "product_category_2": meta["cat2"],
                    "product_category_3": meta["cat3"],
                    "product_id": str(meta["product_id"]),
                })

            matrix_df = pd.DataFrame(matrix_rows)

            # Run ONNX Runtime 2D Parallel Matrix Inference in C++
            pred_df = model_service.predict_price_batch_matrix(matrix_df)

            new_cache_entries: Dict[str, Dict[str, Any]] = {}

            for row_idx, orig_idx in enumerate(uncached_indices):
                meta = item_metas[orig_idx]
                pid = meta["product_id"]
                base_price = price_map.get(pid, 49.90)

                member_price = calculate_member_discount_price(base_price, float(pred_df.iloc[row_idx]["normalized"]))

                quote = {
                    "product_id": pid,
                    "predicted_usd": member_price,
                    "catalog_price": base_price,
                    "normalized_prediction": float(pred_df.iloc[row_idx]["normalized"]),
                    "model_used": str(pred_df.iloc[row_idx]["model_used"]),
                }
                final_quotes[orig_idx] = quote
                new_cache_entries[meta["cache_key"]] = quote

            # Pipeline cache insertion for all newly calculated quotes
            if new_cache_entries:
                cache_manager.mset_json(new_cache_entries, ttl=settings.REDIS_DEFAULT_TTL)

        return [q for q in final_quotes if q is not None]

    @staticmethod
    def estimate_price(
        product_id: str,
        cat1: Optional[int],
        cat2: Optional[int],
        cat3: Optional[int],
        repo: BlackFridayRepository,
        user_id: Optional[int] = None,
        gender: Optional[str] = None,
        age: Optional[str] = None,
        occupation: Optional[int] = None,
        city_category: Optional[str] = None,
        stay_in_current_city_years: Optional[str] = None,
        marital_status: Optional[int] = None,
    ) -> Dict[str, Any]:
        """Calculates personalized member price with 6-hour Redis demographic caching."""
        if user_id is not None and (gender is None or age is None or occupation is None):
            user_demo = repo.get_user_demographics(user_id)
            if user_demo:
                gender = gender or user_demo.get("gender")
                age = age or user_demo.get("age")
                occupation = occupation if occupation is not None else user_demo.get("occupation")
                city_category = city_category or user_demo.get("city_category")
                stay_in_current_city_years = stay_in_current_city_years or user_demo.get("stay_in_current_city_years")
                marital_status = marital_status if marital_status is not None else user_demo.get("marital_status")

        if cat1 is None:
            db_cats = repo.get_product_categories(product_id)
            if db_cats:
                cat1 = int(db_cats["product_category_1"] or 1)
                cat2 = cat2 or (int(db_cats["product_category_2"]) if db_cats.get("product_category_2") else None)
                cat3 = cat3 or (int(db_cats["product_category_3"]) if db_cats.get("product_category_3") else None)
            else:
                cat1 = 1

        cache_key = make_price_cache_key(product_id, gender, age, occupation, city_category, stay_in_current_city_years, marital_status)
        cached = cache_manager.get_json(cache_key)
        if cached:
            return cached

        try:
            pred_res = model_service.predict_price(
                product_id=product_id,
                cat1=cat1,
                cat2=cat2,
                cat3=cat3,
                gender=gender,
                age=age,
                occupation=occupation,
                city_category=city_category,
                stay_in_current_city_years=stay_in_current_city_years,
                marital_status=marital_status,
            )
        except ValueError as val_err:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail=f"Price Prediction Failed: {val_err}"
            )
        except RuntimeError as run_err:
            raise HTTPException(
                status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
                detail=f"Inference Service Failure: {run_err}"
            )

        curated = ShopperService.get_curated_catalog(repo=repo)
        match = next((p for p in curated if p["product_id"] == product_id), None)
        base_price = float(match.get("discounted_price", 49.90)) if match else 49.90

        member_price = calculate_member_discount_price(base_price, float(pred_res.get("normalized", 0.5)))

        result = {
            "product_id": product_id,
            "predicted_usd": member_price,
            "catalog_price": base_price,
            "normalized_prediction": pred_res["normalized"],
            "model_used": pred_res["model_used"],
        }
        cache_manager.set_json(cache_key, result, ttl=settings.REDIS_DEFAULT_TTL)
        return result

    @classmethod
    def process_purchase(
        cls,
        user_id: int,
        product_id: str,
        cat1: Optional[int],
        cat2: Optional[int],
        cat3: Optional[int],
        repo: BlackFridayRepository,
        gender: Optional[str] = None,
        age: Optional[str] = None,
        occupation: Optional[int] = None,
        city_category: Optional[str] = None,
        stay_in_current_city_years: Optional[str] = None,
        marital_status: Optional[int] = None,
    ) -> Dict[str, Any]:
        """Executes price calculation and stores purchase record in database. Uncached transaction path."""
        quote = cls.estimate_price(
            product_id=product_id,
            cat1=cat1,
            cat2=cat2,
            cat3=cat3,
            repo=repo,
            user_id=user_id,
            gender=gender,
            age=age,
            occupation=occupation,
            city_category=city_category,
            stay_in_current_city_years=stay_in_current_city_years,
            marital_status=marital_status,
        )

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

    @classmethod
    def process_batch_purchase(
        cls,
        user_id: int,
        items: List[Dict[str, Any]],
        repo: BlackFridayRepository,
        user_demographics: Optional[Dict[str, Any]] = None,
    ) -> List[Dict[str, Any]]:
        """
        Processes batch purchase of cart items in ONE transaction with quantities.
        Returns per-item purchase records. If any item fails, the entire transaction rolls back.
        """
        if not items:
            return []

        # 1. Validate items and quantities
        for it in items:
            if not it.get("product_id"):
                raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="Missing product_id")
            qty = it.get("quantity", 1)
            if not isinstance(qty, int) or qty < 1:
                raise HTTPException(
                    status_code=status.HTTP_400_BAD_REQUEST,
                    detail=f"Invalid quantity {qty} for product '{it.get('product_id')}'"
                )

        # 2. Get price estimates for each cart item
        quotes = cls.estimate_price_batch(
            items=items,
            repo=repo,
            user_id=user_id,
            user_demographics=user_demographics,
        )

        quote_map = {q["product_id"]: q for q in quotes}

        # 3. Assemble all purchase unit records
        records_to_insert = []
        for it in items:
            pid = it["product_id"]
            quote = quote_map.get(pid)
            if not quote:
                raise HTTPException(
                    status_code=status.HTTP_400_BAD_REQUEST,
                    detail=f"Unable to price product '{pid}'"
                )
            qty = int(it.get("quantity", 1))
            cat1 = it.get("product_category_1")
            cat2 = it.get("product_category_2")
            cat3 = it.get("product_category_3")

            for _ in range(qty):
                records_to_insert.append({
                    "user_id": user_id,
                    "product_id": pid,
                    "product_category_1": cat1 or 1,
                    "product_category_2": cat2,
                    "product_category_3": cat3,
                    "predicted_usd": quote["predicted_usd"],
                    "model_used": quote["model_used"],
                })

        # 4. Insert all records in ONE atomic transaction
        records = repo.record_purchases_batch(records_to_insert)

        return [
            {
                "id": r["id"],
                "product_id": r["product_id"],
                "predicted_usd": float(r["predicted_usd"]),
                "model_used": r["model_used"],
                "purchased_at": str(r["purchased_at"]),
            }
            for r in records
        ]

    @staticmethod
    def get_purchase_history(user_id: int, repo: BlackFridayRepository) -> Dict[str, Any]:
        """Fetches live purchase history. Uncached transaction path."""
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
