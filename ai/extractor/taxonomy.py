"""
Hierarchical Category Taxonomy & Department Mappings.
Maps fine-grained sub-categories to their parent apparel departments for robust semantic expansion.
"""
from typing import Dict, List

# Hierarchical category mapping: fine-grained category -> parent departments
CATEGORY_HIERARCHY: Dict[str, List[str]] = {
    "Silks & Kimonos": ["Silks & Kimonos", "Tops"],
    "Tops & Tunics": ["Tops & Tunics", "Tops"],
    "Shirts & Blouses": ["Shirts & Blouses", "Tops"],
    "Shirts & Polos": ["Shirts & Polos", "Tops"],
    "Coats & Trenches": ["Coats & Trenches", "Outerwear"],
    "Jackets & Outerwear": ["Jackets & Outerwear", "Outerwear"],
    "Leather & Outerwear": ["Leather & Outerwear", "Outerwear"],
    "Jackets & Blazers": ["Jackets & Blazers", "Outerwear"],
    "Knitwear & Sweaters": ["Knitwear & Sweaters", "Knitwear"],
    "Dresses & Skirts": ["Dresses & Skirts", "Dresses"],
    "Dresses & Jumpsuits": ["Dresses & Jumpsuits", "Dresses"],
    "Pants & Trousers": ["Pants & Trousers", "Bottoms"],
    "Denim & Jeans": ["Denim & Jeans", "Bottoms"],
    "Footwear & Boots": ["Footwear & Boots", "Footwear"],
    "Footwear": ["Footwear"],
    "Activewear & Loungewear": ["Activewear & Loungewear"],
    "Accessories": ["Accessories"],
}

__all__ = ["CATEGORY_HIERARCHY"]
