"""
Regression Tests for Fix 7.4 (Hybrid Entity Extractor Deduplication).
Validates:
1. Sizing queries across shoe sizes, waist inches, and spelled-out word sizes extract expected entities.
2. Sizing extraction matches RegexEntityExtractor results without redundant duplicate parsing logic.
"""
import pytest
from ai.extractor.hybrid_extractor import HybridEntityExtractor
from ai.extractor.regex_extractor import RegexEntityExtractor
from ai.schemas import ExtractedEntities


@pytest.fixture
def hybrid_extractor():
    return HybridEntityExtractor()


@pytest.fixture
def regex_extractor():
    return RegexEntityExtractor()


def test_shoe_sizing_query_extraction(hybrid_extractor, regex_extractor):
    """Validates shoe size extraction from footwear queries."""
    query = "Looking for waterproof boots in shoe size 10 under $120"
    
    mock_jev = ExtractedEntities(categories=["Footwear & Boots"])
    res_hybrid = hybrid_extractor.extract(query, jev_entities=mock_jev)
    res_regex = regex_extractor.extract(query)

    assert "10" in res_hybrid.sizes
    assert res_hybrid.sizes == res_regex.sizes
    assert res_hybrid.max_price == 120.0
    assert any("Footwear" in c or "Boot" in c for c in res_hybrid.categories)


def test_waist_sizing_query_extraction(hybrid_extractor, regex_extractor):
    """Validates waist sizing extraction from denim/bottoms queries."""
    query = "Find raw selvedge denim jeans with 32 waist under $80"
    
    mock_jev = ExtractedEntities(categories=["Denim & Jeans"])
    res_hybrid = hybrid_extractor.extract(query, jev_entities=mock_jev)
    res_regex = regex_extractor.extract(query)

    assert "32" in res_hybrid.sizes
    assert res_hybrid.sizes == res_regex.sizes
    assert res_hybrid.max_price == 80.0
    assert any("Denim" in c or "Jean" in c for c in res_hybrid.categories)


def test_word_sizing_query_extraction(hybrid_extractor, regex_extractor):
    """Validates spelled-out word sizes (small, medium, large) extraction."""
    queries = [
        ("Looking for a cashmere knit sweater in size medium under $90", "M"),
        ("Silk kimono shirt in size large", "L"),
        ("Trench coat in extra large", "XL"),
    ]

    for q, expected_size in queries:
        mock_jev = ExtractedEntities(categories=["Knitwear & Sweaters"])
        res_hybrid = hybrid_extractor.extract(q, jev_entities=mock_jev)
        res_regex = regex_extractor.extract(q)

        assert expected_size in res_hybrid.sizes
        assert res_hybrid.sizes == res_regex.sizes
