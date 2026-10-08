"""
Domain and serving helper utilities for API services.
"""
import json
import re
from typing import Dict, Any, List, Optional


def parse_recommendation_ids(value: Any) -> List[str]:
    """Safely parses stored JSON list or string into list of clean product IDs."""
    if value is None:
        return []

    def _extract(v):
        if isinstance(v, dict):
            return str(v.get("product_id") or v.get("item") or v)
        return str(v)

    if isinstance(value, list):
        return [_extract(v) for v in value]

    s = str(value).strip()
    if not s or s in ("[]", "null", "None"):
        return []

    while (s.startswith('"') and s.endswith('"')) or (s.startswith("'") and s.endswith("'")):
        s = s[1:-1].strip()
    s = s.replace('""', '"')

    try:
        parsed = json.loads(s)
        if isinstance(parsed, str):
            parsed = json.loads(parsed)
        if isinstance(parsed, list):
            return [_extract(v) for v in parsed]
        if isinstance(parsed, dict):
            return list(parsed.keys())
    except Exception:
        pass

    matches = re.findall(r'P\d+', s)
    return matches if matches else []


def make_price_cache_key(
    product_id: str,
    gender: Optional[str],
    age: Optional[str],
    occupation: Optional[int],
    city_category: Optional[str],
    stay_in_current_city_years: Optional[str],
    marital_status: Optional[int],
) -> str:
    """Constructs a deterministic 6-hour Redis cache key for product + demographic pricing."""
    g = gender or "UNK"
    a = age or "UNK"
    o = occupation if occupation is not None else "UNK"
    c = city_category or "UNK"
    s = stay_in_current_city_years or "UNK"
    m = marital_status if marital_status is not None else "UNK"
    return f"price:{product_id}:{g}:{a}:{o}:{c}:{s}:{m}"


def resolve_item_demographics(
    item: Dict[str, Any],
    user_demographics: Optional[Dict[str, Any]] = None
) -> Dict[str, Any]:
    """Resolves item demographic overrides against user account demographics."""
    demo = user_demographics or {}
    return {
        "gender": item.get("gender") or demo.get("gender"),
        "age": item.get("age") or demo.get("age"),
        "occupation": item.get("occupation") if item.get("occupation") is not None else demo.get("occupation"),
        "city_category": item.get("city_category") or demo.get("city_category"),
        "stay_in_current_city_years": item.get("stay_in_current_city_years") or demo.get("stay_in_current_city_years"),
        "marital_status": item.get("marital_status") if item.get("marital_status") is not None else demo.get("marital_status"),
    }


def calculate_member_discount_price(base_price: float, normalized_prediction: float) -> float:
    """Calculates personalized member price given base price and normalized ML prediction."""
    norm = max(0.0, min(1.0, float(normalized_prediction)))
    member_discount_factor = 0.72 + 0.16 * norm
    member_price = round(base_price * member_discount_factor, 2)
    if member_price >= base_price:
        return round(base_price * 0.85, 2)
    return member_price

