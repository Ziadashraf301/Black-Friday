"""
Hybrid Strategy Entity Extractor.
Combines deterministic regex extraction (exact continuous budgets, open-ended product IDs)
with Jev 17-category semantic classification and context-aware size disambiguation.
"""
from typing import Optional
from ai.schemas import ExtractedEntities
from ai.extractor.base import BaseEntityExtractor
from ai.extractor.regex_extractor import RegexEntityExtractor
from ai.extractor.jev_extractor import JevEntityExtractor


class HybridEntityExtractor(BaseEntityExtractor):
    """
    Hybrid Strategy: Runs Regex for deterministic tokens (exact continuous budgets, open-ended product IDs).
    Runs Jev for 17-category semantic classification with softmax probabilities.
    Applies Category-Aware Size Disambiguation based on the detected categories.
    """

    def __init__(self, api_key: Optional[str] = None, prompt_version: str = "v2.0.0-extractor"):
        self._regex_extractor = RegexEntityExtractor()
        self._jev_extractor = JevEntityExtractor(api_key=api_key, prompt_version=prompt_version)

    def extract(self, query: str, jev_entities: Optional[ExtractedEntities] = None) -> ExtractedEntities:
        jev_res = jev_entities if jev_entities is not None else self._jev_extractor.extract(query)
        reg_res = self._regex_extractor.extract(query, category_hints=jev_res.categories)

        # Merge categories: Jev provides semantic understanding; Regex provides keyword & catalog backup
        merged_cats = list(dict.fromkeys(jev_res.categories + reg_res.categories))

        return ExtractedEntities(
            max_price=reg_res.max_price,
            min_price=reg_res.min_price,
            sizes=reg_res.sizes,
            product_ids=reg_res.product_ids,
            categories=merged_cats,
            materials=reg_res.materials,
            styles=reg_res.styles,
            product_name=reg_res.product_name,
            action=reg_res.action,
            quantity=reg_res.quantity,
            order_id=reg_res.order_id,
            sentiment=reg_res.sentiment,
            extraction_strategy="hybrid_v2",
        )


__all__ = ["HybridEntityExtractor"]
