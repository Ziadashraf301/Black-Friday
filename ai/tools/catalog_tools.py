"""
Product Details Retrieval Tool (Phase 3).
Provides comprehensive specifications, fabrics, materials, care instructions,
and sizing availability for specific catalog items.
"""
from typing import Optional, Dict, Any
from ai.schemas import ProductDetails
from ai.extractor.catalog_index import CatalogIndex
from core.db.repository import BlackFridayRepository
from core.logging import get_logger

logger = get_logger(__name__)


class ProductDetailsTool:
    """Retrieves full product specifications from in-memory cache or PostgreSQL."""

    def __init__(self, repo: Optional[BlackFridayRepository] = None):
        self._repo = repo or BlackFridayRepository()
        CatalogIndex.ensure_loaded()

    def get_details(self, product_id: str) -> Optional[ProductDetails]:
        """Fetches product details by canonical product ID (e.g. P00025442)."""
        clean_id = product_id.strip().upper()
        if not clean_id.startswith("P"):
            clean_id = f"P{clean_id.zfill(8)}"

        logger.info(f"[TOOL: DETAILS] Fetching specs for product_id={clean_id}")

        # 1. Check in-memory CatalogIndex
        prod_data = CatalogIndex._ID_TO_PRODUCT.get(clean_id)
        if prod_data:
            return self._build_details_model(prod_data)

        # 2. Query PostgreSQL via WarehouseRepository
        try:
            prod_row = self._repo.get_curated_product_by_id(clean_id)
            if prod_row:
                return self._build_details_model(prod_row)
        except Exception as e:
            logger.warning(f"[TOOL: DETAILS] DB query for {clean_id} failed: {e}")

        return None

    def _build_details_model(self, data: Dict[str, Any]) -> ProductDetails:
        return ProductDetails(
            product_id=data.get("product_id", "P00000000"),
            name=data.get("name", "Product"),
            tagline=data.get("tagline", "Curated Black Friday Selection"),
            description=data.get("description", ""),
            category_name=data.get("category_name", "Apparel"),
            materials=data.get("materials", ["Silk", "Wool", "Cotton"]),
            care_instructions=data.get("care_instructions", "Hand wash cold, hang dry"),
            sizes=data.get("sizes", ["S", "M", "L", "XL"]),
            original_price=float(data.get("original_price", 99.0)),
            discounted_price=float(data.get("discounted_price", 49.0)),
            badge=data.get("badge", "Sale"),
            image_url=data.get("image_url", f"/products/{data.get('product_id')}.jpg"),
            in_stock=bool(data.get("in_stock", True)),
            gender=data.get("gender", "Unisex"),
            brand=data.get("brand", "Heritage Guild"),
        )


# Global singleton instance
catalog_details_tool = ProductDetailsTool()
