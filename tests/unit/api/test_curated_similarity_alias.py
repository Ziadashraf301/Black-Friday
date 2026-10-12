"""The catalogue's Item2Vec score ("similarity") reaches API clients as cosine_similarity instead of being dropped."""

from apps.api.schemas import CuratedBundleItem

BASE = {"product_id": "P00147942", "name": "Shirt", "image_url": "/products/P00147942.jpg", "price": 55.0}


def test_catalogue_similarity_key_is_kept():
    item = CuratedBundleItem(**BASE, similarity=0.925)
    assert item.cosine_similarity == 0.925
    assert item.model_dump()["cosine_similarity"] == 0.925


def test_cosine_similarity_name_still_accepted_and_bundles_have_none():
    assert CuratedBundleItem(**BASE, cosine_similarity=0.5).cosine_similarity == 0.5
    assert CuratedBundleItem(**BASE, lift=1.6, confidence=0.44).cosine_similarity is None
