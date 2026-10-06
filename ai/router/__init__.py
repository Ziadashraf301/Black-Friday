"""
System-1 Router Module.
High-throughput, low-latency intent routing using TypeSafe Jev API and local fast rules.
"""
from ai.router.base import BaseRouterStrategy
from ai.router.rule_router import FastRuleRouter
from ai.router.jev_router import JevApiRouter
from ai.router.factory import RouterFactory, intent_router

__all__ = [
    "BaseRouterStrategy",
    "FastRuleRouter",
    "JevApiRouter",
    "RouterFactory",
    "intent_router",
]
