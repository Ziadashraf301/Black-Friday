"""
Bundle Recommendations Tool (Apriori Frequent Itemsets & Item2Vec Pairings).
Live Database & Redis Caching Subsystem (Phase 4 - Task P4-05).
Extracts statistical cross-sell pairings and dynamic bundle discounts directly from
curated_products JSONB with 24-hour Redis caching (TTL 86,400s) and dynamic pricing.
"""
from typing import List, Optional, Dict, Any
from ai.schemas import BundleItem
from ai.extractor.catalog_index import CatalogIndex
from core.db.repository import BlackFridayRepository
from core.cache.redis_client import RedisCacheManager, cache_manager as default_cache_manager
from core.logging import get_logger

logger = get_logger(__name__)


class BundleRecommendationsTool:
    """Retrieves Apriori cross-sell associations and Item2Vec complementary items with 24h caching."""

    CACHE_PREFIX = "cache:bundles"
    DEFAULT_24H_TTL = 86400

    def __init__(
        self,
        repo: Optional[BlackFridayRepository] = None,
        cache_manager: Optional[RedisCacheManager] = None,
    ):
        self._repo = repo or BlackFridayRepository()
        self._cache = cache_manager or default_cache_manager
        CatalogIndex.ensure_loaded()

    def get_recommendations(
        self,
        product_id: str,
        bundle_type: str = "all",
    ) -> List[BundleItem]:
        """
        Retrieves bundled accessories and complementary outfits for a product.
        bundle_type: 'apriori', 'item2vec', or 'all'
        """
        clean_id = product_id.strip().upper()
        if not clean_id.startswith("P"):
            clean_id = f"P{clean_id.zfill(8)}"

        logger.info(f"[TOOL: BUNDLE] Fetching bundles for product_id={clean_id} (type={bundle_type})")

        # 1. Check Redis 24h Cache
        cache_key = f"{self.CACHE_PREFIX}:{clean_id}:{bundle_type}"
        if self._cache.is_available:
            cached_data = self._cache.get_json(cache_key)
            if cached_data:
                logger.info(f"[TOOL: BUNDLE] Redis 24h cache HIT for product_id={clean_id}")
                return [BundleItem(**item) for item in cached_data]

        # 2. Query Live Database curated_products JSONB
        prod_data = None
        try:
            prod_data = self._repo.get_product_bundles(clean_id)
        except Exception as e:
            logger.debug(f"[TOOL: BUNDLE] Live DB bundle fetch failed ({e}), falling back to in-memory catalog")

        if not prod_data:
            prod_data = CatalogIndex._ID_TO_PRODUCT.get(clean_id)

        if not prod_data:
            return []

        results: List[BundleItem] = []
        base_price = float(prod_data.get("discounted_price", 50.0))

        # 3. Process Apriori Frequent Itemsets
        if bundle_type in ("apriori", "all"):
            apriori_list = prod_data.get("apriori_bundles") or []
            for item in apriori_list:
                item_price = float(item.get("price", 50.0))
                results.append(
                    BundleItem(
                        product_id=item["product_id"],
                        name=item["name"],
                        category_name=item.get("category_name", "Apparel"),
                        image_url=item.get("image_url", f"/products/{item['product_id']}.jpg"),
                        price=round(item_price, 2),
                        lift=float(item.get("lift", 1.5)),
                        confidence=float(item.get("confidence", 0.40)),
                        similarity=None,
                        relationship_type="apriori",
                        savings_pct=15.0,
                        bundle_tag="Frequently Bought Together",
                    )
                )

        # 4. Process Item2Vec Semantic Similarity
        if bundle_type in ("item2vec", "all"):
            sim_list = prod_data.get("item2vec_similars") or []
            for item in sim_list:
                if not any(r.product_id == item["product_id"] for r in results):
                    item_price = float(item.get("price", 50.0))
                    results.append(
                        BundleItem(
                            product_id=item["product_id"],
                            name=item["name"],
                            category_name=item.get("category_name", "Apparel"),
                            image_url=item.get("image_url", f"/products/{item['product_id']}.jpg"),
                            price=round(item_price, 2),
                            lift=None,
                            confidence=None,
                            similarity=float(item.get("similarity", 0.80)),
                            relationship_type="item2vec",
                            savings_pct=10.0,
                            bundle_tag="Similar Style Pairing",
                        )
                    )

        # 5. Dynamic Pricing Calculation & MLflow Tracing
        if results:
            total_items_price = sum(b.price for b in results)
            original_bundle_total = base_price + total_items_price
            # Compute weighted savings
            savings_amount = sum(b.price * (b.savings_pct / 100.0) for b in results)
            discounted_bundle_total = round(original_bundle_total - savings_amount, 2)
            avg_savings_pct = round((savings_amount / original_bundle_total) * 100.0, 1)

            try:
                from ai.observability.tracing import agent_tracer
                agent_tracer.trace_bundle_pricing(
                    target_product_id=clean_id,
                    bundle_items=[b.model_dump() for b in results],
                    original_price=original_bundle_total,
                    bundle_price=discounted_bundle_total,
                    savings_pct=avg_savings_pct,
                )
            except Exception:
                pass

        # 6. Store in Redis 24h Cache
        if self._cache.is_available and results:
            serialized = [b.model_dump() for b in results]
            self._cache.set_json(cache_key, serialized, ttl=self.DEFAULT_24H_TTL)
            logger.info(f"[TOOL: BUNDLE] Stored {len(results)} bundles in Redis 24h cache for product_id={clean_id}")

        return results


# Global singleton instance
bundle_tool = BundleRecommendationsTool()

__all__ = ["bundle_tool", "BundleRecommendationsTool"]
