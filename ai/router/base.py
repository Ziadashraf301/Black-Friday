"""
Abstract Base Strategy for System-1 Routers.
"""
from abc import ABC, abstractmethod
from typing import Optional, Dict, Any
from ai.schemas import RoutingDecision


class BaseRouterStrategy(ABC):
    """Abstract Strategy interface for System-1 routing."""

    @abstractmethod
    def route(self, query: str, context: Optional[Dict[str, Any]] = None) -> RoutingDecision:
        """Evaluates query and returns a strongly-typed RoutingDecision."""
        pass


__all__ = ["BaseRouterStrategy"]
