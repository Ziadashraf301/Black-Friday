"""
Compatibility module for ai.observability.mlflow_tracer -> ai.observability.tracing.
"""
from ai.observability.tracing import AgentTracer, agent_tracer

__all__ = ["AgentTracer", "agent_tracer"]
