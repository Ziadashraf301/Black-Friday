"""
Tier-0 High-Speed Deterministic Lexical & Catalog Extractor (<0.5ms).
Handles exact & non-exact Product IDs (case variations, delimiters, typos, zero-padding),
fuzzy catalog title overlap matching, exact budgets, and category-aware sizing.
"""
from typing import List, Optional, Set, Tuple
import re

from ai.schemas import ExtractedEntities
from ai.extractor.base import BaseEntityExtractor
from ai.extractor.catalog_index import CatalogIndex
from ai.extractor.taxonomy import CATEGORY_HIERARCHY
from core.logging import get_logger

logger = get_logger(__name__)


class RegexEntityExtractor(BaseEntityExtractor):
    """
    Tier-0 High-Speed Deterministic Lexical & Catalog Extractor (<0.5ms).
    Handles exact & non-exact Product IDs (case variations, delimiters, typos, zero-padding),
    fuzzy catalog title overlap matching, exact budgets, and category-aware sizing.
    """

    PRICE_PATTERNS = [
        r"(?i)\ba\s+hundy\b",  # $100 colloquial
        r"(?i)(?:capped\s+(?:strictly\s+)?(?:at|under)|capped\s+under|price\s+(?:ceiling|limit)|max\s+budget\s+is|budget\s+is\s+strictly|budget\s+around|under|less\s+than|below|budget\s+of|max(?:imum)?\s+of|up\s+to|for|\<\s*)\s*\$?\s*(\d+(?:\.\d{1,2})?)",
        r"(?i)\$?\s*(\d+(?:\.\d{1,2})?)\s*(?:dollars?|bucks?|\$)\s*(?:or\s+less|max|budget)?",
        r"(?i)\$\s*(\d+(?:\.\d{1,2})?)",
    ]

    APPAREL_SIZES: Set[str] = {"XS", "S", "M", "L", "XL", "XXL"}
    WAIST_SIZES: Set[str] = {"28", "30", "32", "34", "36", "38", "40"}
    SHOE_SIZES: Set[str] = {"7", "8", "9", "10", "11", "12", "13", "39", "40", "41", "42", "43", "44", "45"}

    # Delimiter-tolerant Product ID pattern (p00025442, P-00025442, #P00110742, POOO25442, P25442)
    PRODUCT_ID_REGEX = re.compile(r"\b[pP][\s\-_#]?([0-9oO]{4,8})\b")

    CATEGORY_MAPPINGS: List[Tuple[str, str]] = [
        (r"(?i)\b(?:silk|silks|kimono|kimonos)\b", "Silks & Kimonos"),
        (r"(?i)\b(?:sweater|sweaters|knit|knitwear|cardigan|cardigans|mockneck)\b", "Knitwear & Sweaters"),
        (r"(?i)\b(?:jacket|jackets|blazer|blazers)\b", "Jackets & Outerwear"),
        (r"(?i)\b(?:coat|coats|peacoat|peacoats|trench|trenches|parka|parkas)\b", "Coats & Trenches"),
        (r"(?i)\b(?:leather\s+(?:jacket|bomber|outerwear)|bomber|aviator)\b", "Leather & Outerwear"),
        (r"(?i)\b(?:dress|dresses|pinafore|skirt|skirts)\b", "Dresses & Skirts"),
        (r"(?i)\b(?:jean|jeans|denim|selvedge)\b", "Denim & Jeans"),
        (r"(?i)\b(?:pant|pants|trouser|trousers|slacks)\b", "Pants & Trousers"),
        (r"(?i)\b(?:boot|boots|chelsea|shoe|shoes|sneaker|sneakers|footwear|loafers?)\b", "Footwear & Boots"),
        (r"(?i)\b(?:outerwear)\b", "Outerwear"),
        (r"(?i)\b(?:shirt|shirts|blouse|blouses)\b", "Shirts & Blouses"),
        (r"(?i)\b(?:tunic|tunics)\b", "Tops & Tunics"),
        (r"(?i)\b(?:activewear|loungewear|sweatpants)\b", "Activewear & Loungewear"),
    ]

    MATERIAL_KEYWORDS = ["silk", "wool", "corduroy", "velvet", "leather", "denim", "cashmere", "cotton", "linen"]
    STYLE_KEYWORDS = ["boho chic", "boho", "vintage", "americana", "preppy", "heritage", "minimalist", "retro"]

    def __init__(self):
        CatalogIndex.ensure_loaded()

    def extract(self, query: str, category_hints: Optional[List[str]] = None) -> ExtractedEntities:
        max_price: Optional[float] = None
        extracted_sizes: List[str] = []
        product_ids: List[str] = []
        matched_categories: List[str] = list(category_hints) if category_hints else []
        matched_materials: List[str] = []
        matched_styles: List[str] = []
        raw_tokens: List[str] = []

        # 1. Price extraction
        if re.search(r"(?i)\ba\s+hundy\b", query):
            max_price = 100.0
            raw_tokens.append("$100.0")
        else:
            for pattern in self.PRICE_PATTERNS:
                match = re.search(pattern, query)
                if match and match.groups():
                    try:
                        val = float(match.group(1))
                        if 5.0 <= val <= 5000.0:
                            max_price = val
                            raw_tokens.append(f"${max_price}")
                            break
                    except (ValueError, IndexError):
                        pass

        # 2. Non-Exact Product ID Extraction
        for match in self.PRODUCT_ID_REGEX.finditer(query):
            raw_digits = match.group(1).upper().replace("O", "0")
            canonical_id = f"P{raw_digits.zfill(8)}"
            if canonical_id in CatalogIndex._ID_TO_PRODUCT:
                if canonical_id not in product_ids:
                    product_ids.append(canonical_id)
            else:
                for cid in CatalogIndex._ID_TO_PRODUCT:
                    if cid.endswith(raw_digits):
                        if cid not in product_ids:
                            product_ids.append(cid)
                        break
                else:
                    raw_full = f"P{raw_digits}"
                    if raw_full not in product_ids:
                        product_ids.append(raw_full)

        # 3. Non-Exact / Fuzzy Product Name & Keyword Token-Overlap Extraction
        q_lower = query.lower()
        stopwords = {
            "a", "an", "the", "and", "or", "in", "on", "for", "with", "to", "of", "my",
            "your", "this", "that", "from", "into", "can", "i", "is", "it", "me", "do",
            "you", "have", "looking", "how", "much", "what", "made"
        }
        q_tokens = set(w.lower() for w in re.findall(r"\b[a-zA-Z0-9]+\b", q_lower) if w.lower() not in stopwords)

        if not product_ids:
            for item in CatalogIndex._PRODUCT_LOOKUP:
                if item["name_lower"] in q_lower:
                    if item["id"] not in product_ids:
                        product_ids.append(item["id"])
                    if item["cat"] not in matched_categories:
                        matched_categories.append(item["cat"])
                    break

            if not product_ids:
                candidates = []
                for item in CatalogIndex._PRODUCT_LOOKUP:
                    overlap = item["tokens"].intersection(q_tokens)
                    if len(overlap) >= 2:
                        candidates.append((len(overlap), item))
                    elif len(overlap) == 1:
                        token = list(overlap)[0]
                        if token in {"kimono", "pinafore", "peacoat", "terracotta", "selvedge", "aran"}:
                            candidates.append((1.5, item))

                if candidates:
                    candidates.sort(key=lambda x: x[0], reverse=True)
                    top_score = candidates[0][0]
                    for score, item in candidates:
                        if score == top_score and item["id"] not in product_ids:
                            product_ids.append(item["id"])
                            if item["cat"] not in matched_categories:
                                matched_categories.append(item["cat"])
                            break

        # 4. Resolve Category from detected Product IDs
        for pid in product_ids:
            if pid in CatalogIndex._ID_TO_PRODUCT:
                cat = CatalogIndex._ID_TO_PRODUCT[pid]["category_name"]
                if cat not in matched_categories:
                    matched_categories.append(cat)

        # 5. Regex Category keywords
        for cat_regex, canonical_name in self.CATEGORY_MAPPINGS:
            if re.search(cat_regex, query):
                if canonical_name not in matched_categories:
                    matched_categories.append(canonical_name)

        # Hierarchical Category Expansion
        expanded_cats = list(matched_categories)
        for cat in matched_categories:
            for parent in CATEGORY_HIERARCHY.get(cat, []):
                if parent not in expanded_cats:
                    expanded_cats.append(parent)
        matched_categories = expanded_cats

        # 6. Size extraction (including cart mutations: from size L to XL)
        mutation_m = re.search(r"(?i)\bfrom\s+(?:size\s+)?([a-z0-9]+)\s+to\s+(?:size\s+)?([a-z0-9]+)\b", query)
        if mutation_m:
            for s in [mutation_m.group(1).upper(), mutation_m.group(2).upper()]:
                if s in (self.APPAREL_SIZES | self.WAIST_SIZES | self.SHOE_SIZES):
                    if s not in extracted_sizes:
                        extracted_sizes.append(s)

        explicit_size_matches = re.findall(r"(?i)\bsize(?:\s+|:\s*)([a-z0-9]+)\b", query)
        for s in explicit_size_matches:
            upper_s = s.upper()
            if upper_s in (self.APPAREL_SIZES | self.WAIST_SIZES | self.SHOE_SIZES):
                if upper_s not in extracted_sizes:
                    extracted_sizes.append(upper_s)

        in_size_matches = re.findall(r"(?i)\bin\s+(xxl|2xl|xl|xs|s|m|l)\b", query)
        for s in in_size_matches:
            upper_s = s.upper()
            if upper_s not in extracted_sizes:
                extracted_sizes.append(upper_s)

        is_footwear = any("footwear" in c.lower() or "boot" in c.lower() or "shoe" in c.lower() for c in matched_categories) or bool(re.search(r"\b(?:boots?|shoes?|sneakers?|footwear)\b", query, re.I))
        is_bottoms = any("bottom" in c.lower() or "pant" in c.lower() or "trouser" in c.lower() or "jean" in c.lower() or "denim" in c.lower() for c in matched_categories) or bool(re.search(r"\b(?:jeans?|pants?|trousers?|denim)\b", query, re.I))

        if is_footwear:
            shoe_matches = re.findall(r"\b(?:shoe\s+size\s+|size\s+)?(7|8|9|10|11|12|13)(?:\.5)?\b", query, re.I)
            for sm in shoe_matches:
                if not re.search(rf"\$\s*{sm}\b", query) and not re.search(rf"P\d*{sm}", query):
                    if sm not in extracted_sizes:
                        extracted_sizes.append(sm)

        if is_bottoms:
            waist_matches = re.findall(r"\b(?:waist\s+(?:size\s+)?|size\s+)?(28|30|32|34|36|38|40)\b(?:\s*waist)?", query, re.I)
            for wm in waist_matches:
                if not re.search(rf"\$\s*{wm}\b", query) and not re.search(rf"P\d*{wm}", query):
                    if wm not in extracted_sizes:
                        extracted_sizes.append(wm)

        word_sizes = {"small": "S", "medium": "M", "large": "L", "extra large": "XL"}
        for word, sz in word_sizes.items():
            if re.search(rf"\b(?:size\s+)?{word}\b", query, re.IGNORECASE):
                if sz not in extracted_sizes:
                    extracted_sizes.append(sz)

        # 7. Materials & Styles
        for mat in self.MATERIAL_KEYWORDS:
            if re.search(rf"\b{mat}\b", q_lower):
                matched_materials.append(mat.capitalize())

        for st in self.STYLE_KEYWORDS:
            if st in q_lower:
                matched_styles.append(st.title())

        return ExtractedEntities(
            max_price=max_price,
            min_price=None,
            sizes=extracted_sizes,
            product_ids=product_ids,
            categories=matched_categories,
            materials=matched_materials,
            styles=matched_styles,
            product_name=None,
            action=None,
            quantity=None,
            order_id=None,
            sentiment=None,
            extraction_strategy="regex",
        )


__all__ = ["RegexEntityExtractor"]
