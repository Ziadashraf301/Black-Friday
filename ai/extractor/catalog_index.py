"""
In-Memory Catalog Index for sub-millisecond lookup and token-overlap fuzzy matching.
"""
from typing import Dict, List, Any, Optional
import json
import re
from pathlib import Path
from core.logging import get_logger

logger = get_logger(__name__)


class CatalogIndex:
    """Singleton index providing fast sub-millisecond catalog lookup and fuzzy token-matching."""
    _LOADED: bool = False
    _ID_TO_PRODUCT: Dict[str, Dict[str, Any]] = {}
    _PRODUCT_LOOKUP: List[Dict[str, Any]] = []

    @classmethod
    def ensure_loaded(cls) -> None:
        if cls._LOADED:
            return

        candidate_paths = [
            Path(__file__).resolve().parent.parent.parent / "data" / "curated_products.json",
            Path("data/curated_products.json").resolve(),
            Path("../data/curated_products.json").resolve(),
        ]
        products = []
        for p in candidate_paths:
            if p.exists():
                try:
                    with open(p, "r", encoding="utf-8") as f:
                        products = json.load(f)
                    break
                except Exception as e:
                    logger.warning(f"[CATALOG-INDEX] Failed to read {p}: {e}")

        stopwords = {
            "a", "an", "the", "and", "or", "in", "on", "for", "with", "to", "of", "my",
            "your", "this", "that", "from", "into", "can", "i", "is", "it", "me", "do",
            "you", "have", "looking", "how", "much", "what", "made"
        }

        for prod in products:
            pid = prod.get("product_id")
            name = prod.get("name", "")
            cat = prod.get("category_name", "")
            if pid:
                cls._ID_TO_PRODUCT[pid] = prod
                tokens = [w.lower() for w in re.findall(r"\b[a-zA-Z0-9]+\b", name) if w.lower() not in stopwords]
                cls._PRODUCT_LOOKUP.append({
                    "id": pid,
                    "name": name,
                    "cat": cat,
                    "tokens": set(tokens),
                    "name_lower": name.lower(),
                })

        cls._LOADED = True

    @classmethod
    def find_best_product_id(cls, text: str) -> Optional[str]:
        """Matches query text against catalog products using token overlap."""
        cls.ensure_loaded()
        q_tokens = set(w.lower() for w in re.findall(r"\b[a-zA-Z0-9]+\b", text))
        best_id = None
        best_overlap = 0

        for entry in cls._PRODUCT_LOOKUP:
            overlap = len(entry["tokens"] & q_tokens)
            if overlap > best_overlap:
                best_overlap = overlap
                best_id = entry["id"]

        return best_id if best_overlap >= 2 else None


__all__ = ["CatalogIndex"]
