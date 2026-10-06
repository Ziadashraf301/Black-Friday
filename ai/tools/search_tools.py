"""
Hybrid Product Search Tool Adapter (Phase 3).
Lean adapter adhering to SOLID: delegates embedding orchestration, category expansion,
and PostgreSQL hybrid RRF retrieval to ProductSearchService.
"""
from typing import List, Optional
from ai.schemas import ProductSearchResult
from ai.services.search_service import search_service, ProductSearchService


class HybridProductSearchTool:
    """
    Agent tool adapter providing product search capabilities to LangGraph specialist nodes.
    Delegates all business and retrieval logic to ProductSearchService.
    """

    def __init__(self, service: Optional[ProductSearchService] = None):
        self._service = service or search_service

    def search(
        self,
        query: str,
        category: Optional[str] = None,
        max_price: Optional[float] = None,
        size: Optional[str] = None,
        top_k: int = 5,
    ) -> List[ProductSearchResult]:
        """Executes catalog search with entity constraints via the search service."""
        return self._service.search(
            query=query,
            category=category,
            max_price=max_price,
            size=size,
            top_k=top_k,
        )


# Global singleton instance for LangGraph agent nodes
search_tool = HybridProductSearchTool()
