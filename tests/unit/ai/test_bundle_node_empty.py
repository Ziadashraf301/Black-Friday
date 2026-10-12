"""A product with no Apriori rule (7 catalogue products) or no pairings at all is answered without an empty offer."""

from unittest.mock import patch

from ai.nodes import bundle_node as node
from ai.schemas import BundleItem


def _similar(pid):
    return BundleItem(
        product_id=pid,
        name=pid,
        category_name="Apparel",
        image_url=f"/products/{pid}.jpg",
        price=10.0,
        lift=None,
        confidence=None,
        similarity=0.9,
        relationship_type="item2vec",
        savings_pct=10.0,
        bundle_tag="Similar Style Pairing",
    )


def _run(bundles):
    state = {"entities": {"product_ids": ["P00184942"]}}
    with patch.object(node.bundle_tool, "get_recommendations", return_value=bundles):
        return node.bundle_agent_node(state)


def test_no_apriori_rule_still_offers_similar_items():
    out = _run([_similar("P00147942")])
    assert [c["product_id"] for c in out["ui_payload"]["cards"]] == ["P00147942"]
    assert out["bundle_recommendations"][0]["bundle_tag"] == "Similar Style Pairing"  # not sold as "bought together"
    assert out["ui_payload"]["action_chips"] == ["Add Bundle to Cart"]


def test_no_pairings_at_all_gives_no_cards_and_no_bundle_chip():
    out = _run([])
    assert out["bundle_recommendations"] == [] and out["ui_payload"]["cards"] == []
    assert out["ui_payload"]["action_chips"] == []
