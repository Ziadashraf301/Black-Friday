"""AI-7: `bypass_cache` is part of AgentState, so LangGraph delivers it to the router node instead of dropping it."""

from langgraph.graph import END, StateGraph

from ai.schemas.state import AgentState


def test_bypass_cache_reaches_nodes():
    seen = {}

    def node(state):
        seen["bypass_cache"] = state.get("bypass_cache")
        return {}

    graph = StateGraph(AgentState)
    graph.add_node("n", node)
    graph.set_entry_point("n")
    graph.add_edge("n", END)
    graph.compile().invoke({"query": "q", "bypass_cache": True})
    assert seen["bypass_cache"] is True
