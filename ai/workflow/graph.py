"""
LangGraph Multi-Agent Shopping Graph (Phase 3).
Orchestrates guardrail routing, conditional specialist dispatch,
multi-tier memory checkpointing, and response synthesis.
"""
from typing import Optional, Any
from langgraph.graph import StateGraph, END
from ai.workflow.state import AgentState
from ai.nodes.router_node import guardrail_router_node
from ai.nodes.refusal_node import refusal_node
from ai.nodes.steering_node import steering_node
from ai.nodes.search_node import search_agent_node
from ai.nodes.details_node import details_agent_node
from ai.nodes.bundle_node import bundle_agent_node
from ai.nodes.cart_node import cart_agent_node
from ai.nodes.support_node import support_agent_node
from ai.nodes.aggregator_node import retrieval_aggregator_join
from ai.nodes.synthesis_node import response_synthesis_node
from ai.workflow.edges import route_after_guardrail
from core.logging import get_logger

logger = get_logger(__name__)


def build_shopping_graph(checkpointer: Optional[Any] = None) -> Any:
    """
    Constructs and compiles the complete LangGraph StateGraph workflow.
    Supports multi-intent parallel specialist fan-out and synchronized join barrier.
    """
    workflow = StateGraph(AgentState)

    # 1. Register Nodes
    workflow.add_node("guardrail_router_node", guardrail_router_node)
    workflow.add_node("refusal_node", refusal_node)
    workflow.add_node("steering_node", steering_node)
    workflow.add_node("search_agent_node", search_agent_node)
    workflow.add_node("details_agent_node", details_agent_node)
    workflow.add_node("bundle_agent_node", bundle_agent_node)
    workflow.add_node("cart_agent_node", cart_agent_node)
    workflow.add_node("support_agent_node", support_agent_node)
    workflow.add_node("retrieval_aggregator_join", retrieval_aggregator_join)
    workflow.add_node("response_synthesis_node", response_synthesis_node)

    # 2. Set Root Entry Point
    workflow.set_entry_point("guardrail_router_node")

    # 3. Conditional Edge Dispatch from Router (Supports Parallel Fan-Out)
    workflow.add_conditional_edges(
        "guardrail_router_node",
        route_after_guardrail,
        {
            "refusal_node": "refusal_node",
            "steering_node": "steering_node",
            "search_agent_node": "search_agent_node",
            "details_agent_node": "details_agent_node",
            "bundle_agent_node": "bundle_agent_node",
            "cart_agent_node": "cart_agent_node",
            "support_agent_node": "support_agent_node",
            END: END,
        },
    )

    # 4. Terminal Nodes -> END
    workflow.add_edge("refusal_node", END)
    workflow.add_edge("steering_node", END)

    # 5. Specialist Worker Nodes -> Fan-In Aggregator Join Barrier
    workflow.add_edge("search_agent_node", "retrieval_aggregator_join")
    workflow.add_edge("details_agent_node", "retrieval_aggregator_join")
    workflow.add_edge("bundle_agent_node", "retrieval_aggregator_join")
    workflow.add_edge("cart_agent_node", "retrieval_aggregator_join")
    workflow.add_edge("support_agent_node", "retrieval_aggregator_join")

    # 6. Aggregator Join Barrier -> Response Synthesis -> END
    workflow.add_edge("retrieval_aggregator_join", "response_synthesis_node")
    workflow.add_edge("response_synthesis_node", END)

    # 7. Compile Graph
    return workflow.compile(checkpointer=checkpointer)


# Canonical compiled default instance (in-memory execution)
shopping_graph = build_shopping_graph()

__all__ = [
    "build_shopping_graph",
    "shopping_graph",
]

