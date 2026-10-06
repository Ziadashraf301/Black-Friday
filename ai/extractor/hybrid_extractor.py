"""
Hybrid Strategy Entity Extractor.
Combines deterministic regex extraction (exact continuous budgets, open-ended product IDs)
with Jev 17-category semantic classification and context-aware size disambiguation.
"""
from typing import Optional
import re
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
        reg_res = self._regex_extractor.extract(query)
        jev_res = jev_entities if jev_entities is not None else self._jev_extractor.extract(query)

        # Merge categories: Jev provides semantic understanding; Regex provides keyword & catalog backup
        merged_cats = list(dict.fromkeys(jev_res.categories + reg_res.categories))

        # Category-Aware Size Disambiguation:
        is_footwear = any("footwear" in c.lower() or "boot" in c.lower() or "shoe" in c.lower() for c in merged_cats) or bool(re.search(r"\b(?:boots?|shoes?|sneakers?|loafers?|footwear)\b", query, re.I))
        is_bottoms = any("bottom" in c.lower() or "pant" in c.lower() or "trouser" in c.lower() or "jean" in c.lower() or "denim" in c.lower() for c in merged_cats) or bool(re.search(r"\b(?:jeans?|pants?|trousers?|denim)\b", query, re.I))

        resolved_sizes = list(reg_res.sizes)

        # In footwear context: extract shoe numbers (e.g. 7-13)
        if is_footwear:
            shoe_m = re.findall(r"\b(?:shoe\s+size\s+|size\s+)?(7|8|9|10|11|12|13)(?:\.5)?\b", query, re.I)
            for sm in shoe_m:
                if not re.search(rf"\$\s*{sm}\b", query) and not re.search(rf"P\d*{sm}", query):
                    if sm not in resolved_sizes:
                        resolved_sizes.append(sm)

        # In bottoms context: extract waist inches (e.g. 28-40)
        if is_bottoms:
            waist_m = re.findall(r"\b(?:waist\s+(?:size\s+)?|size\s+)?(28|30|32|34|36|38|40)\b(?:\s*waist)?", query, re.I)
            for wm in waist_m:
                if not re.search(rf"\$\s*{wm}\b", query) and not re.search(rf"P\d*{wm}", query):
                    if wm not in resolved_sizes:
                        resolved_sizes.append(wm)

        # Spelled-out apparel words (small, medium, large, extra large)
        word_map = {"small": "S", "medium": "M", "large": "L", "extra large": "XL"}
        for word, sz in word_map.items():
            if re.search(rf"\b(?:size\s+)?{word}\b", query, re.I):
                if sz not in resolved_sizes:
                    resolved_sizes.append(sz)

        return ExtractedEntities(
            max_price=reg_res.max_price,
            min_price=reg_res.min_price,
            sizes=resolved_sizes,
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
