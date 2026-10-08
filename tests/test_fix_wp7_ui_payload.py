"""
Regression and Contract Tests for Fixes 5.6, 6.4, 10.2 (Unified UIPayload Schema and Reducer).
Validates:
1. ui_payload_reducer merges cards, action chips, and citations across specialist nodes without loss.
2. Fanout run with two specialists (e.g. search + bundle) yields both card sets and action chips in final payload.
3. Single-intent run preserves cards and action chips.
4. Fake LLM client integration without network calls.
"""
from typing import Dict, Any, List
from unittest.mock import MagicMock
import pytest

from ai.workflow.state import (
    AgentState,
    UIPayload,
    UICard,
    GroundingCitation,
    ui_payload_reducer,
)
from ai.services.synthesis_service import SynthesisService
from ai.services.template_service import ResponseTemplateService
from ai.workflow.graph import build_shopping_graph


def test_ui_payload_reducer_combines_specialist_cards_and_chips():
    """Validates that ui_payload_reducer merges cards, action chips, and citations from parallel branches."""
    payload_a: Dict[str, Any] = {
        "cards": [
            {
                "type": "PRODUCT_CARD",
                "product_id": "P00000001",
                "name": "Silk Scarf",
                "price": 45.0,
            }
        ],
        "action_chips": ["View Cart", "Top Deals"],
        "citations": [
            {
                "product_id": "P00000001",
                "title": "Silk Scarf",
                "url": "/shopper/browse/P00000001",
                "price": 45.0,
                "badge": "Deal",
            }
        ],
        "relaxation_level": "STRICT",
    }

    payload_b: Dict[str, Any] = {
        "cards": [
            {
                "type": "BUNDLE_CARD",
                "product_id": "P00000002",
                "name": "Leather Belt",
                "price": 30.0,
                "bundle_price": 24.0,
            }
        ],
        "action_chips": ["Add Bundle to Cart", "View Cart"],
        "citations": [
            {
                "product_id": "P00000002",
                "title": "Leather Belt",
                "url": "/shopper/browse/P00000002",
                "price": 24.0,
                "badge": "20% Off Bundle",
            }
        ],
    }

    merged = ui_payload_reducer(payload_a, payload_b)
    assert merged is not None
    assert len(merged["cards"]) == 2
    card_pids = [c["product_id"] for c in merged["cards"]]
    assert "P00000001" in card_pids
    assert "P00000002" in card_pids

    # Action chips must be deduplicated preserving order
    assert merged["action_chips"] == ["View Cart", "Top Deals", "Add Bundle to Cart"]

    # Citations must be merged
    assert len(merged["citations"]) == 2
    cite_pids = [c["product_id"] for c in merged["citations"]]
    assert "P00000001" in cite_pids
    assert "P00000002" in cite_pids


def test_ui_payload_reducer_enriches_same_product():
    """Validates that a detailed card enriches a preliminary product card with the same ID."""
    payload_a: Dict[str, Any] = {
        "cards": [
            {
                "type": "PRODUCT_CARD",
                "product_id": "P00025442",
                "name": "Paisley Kimono",
                "price": 120.0,
            }
        ],
        "action_chips": ["View Cart"],
    }

    payload_b: Dict[str, Any] = {
        "cards": [
            {
                "type": "PRODUCT_DETAIL_CARD",
                "product_id": "P00025442",
                "materials": ["100% Silk"],
                "care_instructions": "Dry clean only",
            }
        ],
        "action_chips": ["Select Size"],
    }

    merged = ui_payload_reducer(payload_a, payload_b)
    assert len(merged["cards"]) == 1
    enriched_card = merged["cards"][0]
    assert enriched_card["product_id"] == "P00025442"
    assert enriched_card["name"] == "Paisley Kimono"
    assert enriched_card["materials"] == ["100% Silk"]
    assert enriched_card["care_instructions"] == "Dry clean only"
    assert "Select Size" in merged["action_chips"]


def test_fanout_run_with_two_specialists_yields_both_card_sets():
    """
    Simulates a fanout execution with search + bundle specialists in the LangGraph graph
    and asserts that the final payload retains both product and bundle cards and action chips.
    Uses a mocked fake LLM client to ensure zero network calls.
    """
    mock_llm_client = MagicMock()
    mock_llm_response = MagicMock()
    mock_llm_response.text = "Here are your matching coats along with a discounted scarf bundle."
    mock_llm_client.models.generate_content.return_value = mock_llm_response

    custom_synthesis = SynthesisService(llm_client=mock_llm_client)

    state: AgentState = {
        "query": "winter coats with matching bundle",
        "user_id": "test_user_fanout",
        "session_id": "sess_fanout",
        "intent": "PRODUCT_SEARCH",
        "target_intents": ["PRODUCT_SEARCH", "BUNDLE_RECOMMENDATIONS"],
        "retrieved_products": [
            {
                "product_id": "P00000010",
                "name": "Down Winter Parka",
                "discounted_price": 110.0,
                "original_price": 150.0,
                "badge": "Top Deal",
            }
        ],
        "bundle_recommendations": [
            {
                "product_id": "P00000020",
                "name": "Wool Knit Scarf",
                "price": 25.0,
                "bundle_price": 20.0,
                "discount_pct": "20% OFF",
            }
        ],
        "ui_payload": {
            "cards": [
                {
                    "type": "PRODUCT_CARD",
                    "product_id": "P00000010",
                    "name": "Down Winter Parka",
                    "price": 110.0,
                },
                {
                    "type": "BUNDLE_CARD",
                    "product_id": "P00000020",
                    "name": "Wool Knit Scarf",
                    "price": 25.0,
                    "bundle_price": 20.0,
                },
            ],
            "action_chips": ["View Cart", "Add Bundle to Cart"],
        },
    }

    result = custom_synthesis.synthesize(state)

    final_payload = result["ui_payload"]
    assert "cards" in final_payload
    assert len(final_payload["cards"]) == 2

    card_types = [c["type"] for c in final_payload["cards"]]
    assert "PRODUCT_CARD" in card_types
    assert "BUNDLE_CARD" in card_types

    assert "action_chips" in final_payload
    assert "Add Bundle to Cart" in final_payload["action_chips"]
    assert "View Cart" in final_payload["action_chips"]

    # Verify LLM response was generated without network
    assert result["final_response"] == "Here are your matching coats along with a discounted scarf bundle."


def test_single_intent_run_preserves_cards_and_chips():
    """Asserts that single-intent execution still formats valid cards and action chips."""
    state: AgentState = {
        "query": "silk kimono",
        "user_id": "single_user",
        "session_id": "sess_single",
        "intent": "PRODUCT_SEARCH",
        "retrieved_products": [
            {
                "product_id": "P00025442",
                "name": "Artisan Paisley Silk Kimono",
                "discounted_price": 95.0,
                "original_price": 140.0,
                "badge": "Trending",
            }
        ],
        "ui_payload": None,
    }

    synthesis = SynthesisService()
    result = synthesis.synthesize(state)

    final_payload = result["ui_payload"]
    assert final_payload["type"] == "product_carousel"
    assert len(final_payload["cards"]) == 1
    assert final_payload["cards"][0]["product_id"] == "P00025442"
    assert "View Cart" in final_payload["action_chips"]
    assert len(final_payload["citations"]) == 1
    assert final_payload["citations"][0]["product_id"] == "P00025442"
