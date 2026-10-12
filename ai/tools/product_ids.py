"""Canonical catalog product ids for the assistant tools."""

from typing import Optional


def normalize_product_id(product_id: Optional[str]) -> str:
    """Trims and upper-cases an id and restores the `P` + 8-digit form ("25442" -> "P00025442"); empty stays empty."""
    clean_id = (product_id or "").strip().upper()
    if clean_id and not clean_id.startswith("P"):
        clean_id = f"P{clean_id.zfill(8)}"
    return clean_id
