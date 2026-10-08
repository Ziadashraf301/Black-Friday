"""
Product Search Service (Facade & Business Logic Layer).
Coordinates semantic vector embedding, category hierarchy expansion,
and delegates database retrieval to WarehouseRepository hybrid search.
Lives in ai.services alongside embedding_service.py to maintain strict Clean Architecture boundaries.
"""
from typing import List, Optional
from ai.schemas import ProductSearchResult
from core.embeddings import embedding_service
from ai.extractor.taxonomy import CATEGORY_HIERARCHY
from core.db.repository import BlackFridayRepository
from core.logging import get_logger

logger = get_logger(__name__)


class ProductSearchService:
    """
    Core AI business service orchestrating product search.
    Decoupled from low-level database operations and transport-level agent tools.
    """

    def __init__(self, repo: Optional[BlackFridayRepository] = None):
        self._repo = repo or BlackFridayRepository()

    def search(
        self,
        query: str,
        category: Optional[str] = None,
        max_price: Optional[float] = None,
        size: Optional[str] = None,
        top_k: int = 5,
    ) -> List[ProductSearchResult]:
        """
        Executes hybrid vector + full-text product search with shopping constraint filters.
        """
        logger.info(
            f"[SEARCH-SERVICE] Query='{query}', category={category}, "
            f"max_price={max_price}, size={size}, top_k={top_k}"
        )

        # 1. Expand category hierarchy if supplied (bidirectional fine-grained <-> department)
        target_categories: Optional[List[str]] = None
        if category:
            target_categories = [category]
            cat_clean = category.strip().lower()
            # Forward: fine-grained -> parent departments
            for parent in CATEGORY_HIERARCHY.get(category, []):
                if parent not in target_categories:
                    target_categories.append(parent)
            # Reverse: department or keyword -> catalog child categories
            for child, parents in CATEGORY_HIERARCHY.items():
                if cat_clean in child.lower() or any(cat_clean in p.lower() for p in parents):
                    if child not in target_categories:
                        target_categories.append(child)

        # 2. Generate dense query embedding via embedding_service
        query_vec = embedding_service.generate_embedding(query)

        # 3. Delegate progressive hybrid retrieval to the warehouse repository
        records, relaxation_level = self._repo.hybrid_search_products(
            query_vec=query_vec,
            query_text=query,
            target_categories=target_categories,
            max_price=max_price,
            size=size,
            top_k=top_k,
        )

        # 4. Emit MLflow GenAI Trace
        try:
            from ai.observability.tracing import agent_tracer
            agent_tracer.trace_search_ladder(
                query=query,
                relaxation_level=relaxation_level,
                items_found=len(records),
                filters_applied={"category": category, "max_price": max_price, "size": size},
            )
        except Exception:
            pass

        # 5. Map into typed domain models
        return [
            ProductSearchResult(
                product_id=r["product_id"],
                name=r["name"],
                category_name=r["category_name"],
                original_price=r["original_price"],
                discounted_price=r["discounted_price"],
                badge=r.get("badge"),
                sizes=r.get("sizes", []),
                image_url=r.get("image_url"),
                style=r.get("style"),
                score=r.get("score", 0.0),
                relaxation_level=r.get("relaxation_level", relaxation_level),
            )
            for r in records
        ]


# Singleton instance
search_service = ProductSearchService()
