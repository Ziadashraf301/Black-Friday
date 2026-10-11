"""AI-13: when no product resolves the assistant asks which item is meant; nothing is invented."""

import uuid

import pytest

from ai.nodes.bundle_node import bundle_agent_node
from ai.nodes.cart_node import cart_agent_node
from ai.nodes.details_node import details_agent_node
from ai.schemas.identity import ASK_WHICH_PRODUCT, unknown_product_message
from ai.services.synthesis_service import SynthesisService
from ai.tools.cart_tools import CartManagementTool, cart_tool
from core.exceptions import DomainError

UNKNOWN_ID = "P99999999"
KNOWN_ID = "P00025442"


@pytest.fixture
def shopper():
    user, session = f"ai13-{uuid.uuid4().hex}", "s1"
    yield user, session
    cart_tool.modify_cart(user, session, action="clear")


# ---- cart tool -------------------------------------------------------------------------------------------------
def test_cart_tool_rejects_unknown_product_with_a_clear_message(shopper):
    user, session = shopper
    with pytest.raises(DomainError) as exc:
        cart_tool.modify_cart(user, session, action="add", product_id=UNKNOWN_ID)
    assert UNKNOWN_ID in str(exc.value) and "not in our catalog" in str(exc.value)
    assert cart_tool.get_cart(user, session).items == []


def test_cart_tool_rejects_add_without_a_product(shopper):
    user, session = shopper
    with pytest.raises(DomainError):
        cart_tool.modify_cart(user, session, action="add")
    assert cart_tool.get_cart(user, session).items == []


def test_cart_tool_still_adds_a_real_product(shopper):
    user, session = shopper
    cart = cart_tool.modify_cart(user, session, action="add", product_id=KNOWN_ID)
    assert [i.product_id for i in cart.items] == [KNOWN_ID]


def test_lookup_has_no_default_product():
    assert CartManagementTool()._lookup_product_info(UNKNOWN_ID) is None
    assert CartManagementTool()._lookup_product_info(None) is None


# ---- nodes -----------------------------------------------------------------------------------------------------
def test_details_node_asks_when_no_product_resolves():
    out = details_agent_node({"query": "tell me about the fabric", "entities": {}})
    assert out["clarification"] == ASK_WHICH_PRODUCT
    assert "product_details" not in out


def test_details_node_reports_an_unknown_id():
    out = details_agent_node({"query": f"details for {UNKNOWN_ID}", "entities": {"product_ids": [UNKNOWN_ID]}})
    assert out["clarification"] == unknown_product_message(UNKNOWN_ID)
    assert "product_details" not in out


def test_bundle_node_asks_when_no_product_resolves():
    out = bundle_agent_node({"query": "what goes with it", "entities": {}})
    assert out["clarification"] == ASK_WHICH_PRODUCT
    assert "bundle_recommendations" not in out


def test_cart_node_asks_instead_of_adding_a_default_product(shopper):
    user, session = shopper
    out = cart_agent_node({"query": "add it to my cart", "entities": {}, "user_id": user, "session_id": session})
    assert out["clarification"] == ASK_WHICH_PRODUCT
    assert "cart" not in out
    assert cart_tool.get_cart(user, session).items == []


def test_cart_node_rejects_unknown_id_and_leaves_the_cart_alone(shopper):
    user, session = shopper
    state = {
        "query": f"add {UNKNOWN_ID} to my cart",
        "entities": {"product_ids": [UNKNOWN_ID]},
        "user_id": user,
        "session_id": session,
    }
    out = cart_agent_node(state)
    assert out["clarification"] == unknown_product_message(UNKNOWN_ID)
    assert "cart" not in out
    assert cart_tool.get_cart(user, session).items == []


# ---- synthesis -------------------------------------------------------------------------------------------------
def test_synthesis_answers_with_just_the_question_when_nothing_else_was_found():
    out = SynthesisService(llm_client=None).synthesize(
        {"query": "add it", "intent": "CART_ACTIONS", "user_id": "u", "clarification": ASK_WHICH_PRODUCT}
    )
    assert out["final_response"] == ASK_WHICH_PRODUCT
    assert out["ui_payload"]["type"] == "clarification"


def test_synthesis_appends_the_question_to_other_findings():
    out = SynthesisService(llm_client=None).synthesize(
        {
            "query": "jackets and what goes with it",
            "intent": "PRODUCT_SEARCH",
            "user_id": "u",
            "retrieved_products": [
                {
                    "product_id": "P1",
                    "name": "Jacket",
                    "category_name": "Coats",
                    "original_price": 100.0,
                    "discounted_price": 60.0,
                    "sizes": ["M"],
                }
            ],
            "clarification": ASK_WHICH_PRODUCT,
        }
    )
    assert "Jacket" in out["final_response"] and out["final_response"].endswith(ASK_WHICH_PRODUCT)


def test_clarification_answers_are_not_cached():
    assert SynthesisService._is_cacheable({"clarification": ASK_WHICH_PRODUCT}, "PRODUCT_DETAILS") is False
    assert SynthesisService._is_cacheable({}, "PRODUCT_DETAILS") is True


# ---- whole graph -----------------------------------------------------------------------------------------------
@pytest.mark.parametrize("query", ["Add it to my cart", "What fabric is this made of?"])
def test_graph_asks_which_item(query, shopper):
    from ai.workflow.graph import shopping_graph

    user, session = shopper
    result = shopping_graph.invoke({"query": query, "user_id": user, "session_id": session, "bypass_cache": True})
    assert ASK_WHICH_PRODUCT in result["final_response"]
    assert not result.get("cart") and not result.get("product_details")
