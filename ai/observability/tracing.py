"""
Distributed Tracing & GenAI Observability Engine (Phase 4 - Task P4-03).
Leverages MLflow GenAI Tracing:
  - Framework Autologging: Instruments LangGraph workflows and LangChain runnables via mlflow.langchain.autolog().
  - Custom Trace Spans (@mlflow.trace): Fine-grained observability into Two-Tier Caching,
    Guardrail Safety Checks, Progressive Search Relaxation, and Dynamic Bundle Pricing.
  - User & Session Metadata: Propagates mlflow.trace.user and mlflow.trace.session.
"""
from typing import Optional, Dict, Any, Callable, List
import time
import functools
import os

from core.tracking import (
    SpanType,
    setup_tracking_environment,
    get_tracking_uri,
    get_or_create_experiment,
    enable_langchain_autolog,
    update_current_trace,
    trace,
)
from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)


class AgentTracer:
    """
    Production Observability Manager for Black Friday Multi-Agent Architecture.
    Instruments LangGraph, custom tool execution, and query lifecycle.
    """

    def __init__(self, experiment_name: str = "black-friday-production-agent"):
        self._experiment_name = experiment_name
        self._initialized = False

        try:
            setup_tracking_environment()
            tracking_uri = get_tracking_uri()
            get_or_create_experiment(self._experiment_name)
            self._initialized = True
            logger.info(f"[TRACING: MLFLOW] Tracking URI set to '{tracking_uri}', Experiment: '{self._experiment_name}'")
        except Exception as e:
            logger.warning(f"[TRACING: MLFLOW] MLflow tracking server unavailable ({e}). Running in offline trace mode.")

        # Enable framework autologging for LangGraph and LangChain
        self._enable_autolog()

    def _enable_autolog(self) -> None:
        """Enables zero-code autologging for LangChain and LangGraph."""
        try:
            enable_langchain_autolog(
                log_models=False,
                log_input_examples=False,
                log_traces=True,
            )
            logger.info("[TRACING: MLFLOW] mlflow.langchain.autolog() successfully initialized for LangGraph")
        except Exception as e:
            logger.debug(f"[TRACING: MLFLOW] LangChain autolog initialization notice: {e}")

    # =========================================================================
    # Custom Traced Spans for Architectural Milestones
    # =========================================================================

    @staticmethod
    @trace(name="two_tier_cache_lookup", span_type=SpanType.TOOL)
    def trace_cache_lookup(raw_query: str, tier: str, is_hit: bool, latency_ms: float) -> Dict[str, Any]:
        """Records cache lookup outcome in MLflow trace."""
        return {
            "query_preview": raw_query[:80],
            "tier": tier,
            "hit": is_hit,
            "latency_ms": round(latency_ms, 2),
        }

    @staticmethod
    @trace(name="guardrail_safety_evaluation", span_type=SpanType.CHAIN)
    def trace_guardrail_eval(
        query: str,
        user_id: str,
        session_id: str,
        is_safe: bool,
        adversarial_prob: float,
        target_intents: List[str],
    ) -> Dict[str, Any]:
        """Records safety boundary evaluation and intent routing."""
        try:
            update_current_trace(
                metadata={
                    "mlflow.trace.user": user_id,
                    "mlflow.trace.session": session_id,
                },
                tags={
                    "is_safe": str(is_safe),
                    "adversarial_prob": str(round(adversarial_prob, 4)),
                    "intents": ",".join(target_intents),
                },
            )
        except Exception:
            pass

        return {
            "query": query,
            "is_safe": is_safe,
            "adversarial_prob": adversarial_prob,
            "intents": target_intents,
        }

    @staticmethod
    @trace(name="progressive_search_ladder", span_type=SpanType.RETRIEVER)
    def trace_search_ladder(
        query: str,
        relaxation_level: str,
        items_found: int,
        filters_applied: Dict[str, Any],
    ) -> Dict[str, Any]:
        """Records hybrid retrieval ladder progression."""
        return {
            "query": query,
            "relaxation_level": relaxation_level,
            "items_found": items_found,
            "filters": filters_applied,
        }

    @staticmethod
    @trace(name="bundle_dynamic_pricing", span_type=SpanType.TOOL)
    def trace_bundle_pricing(
        target_product_id: str,
        bundle_items: List[Dict[str, Any]],
        original_price: float,
        bundle_price: float,
        savings_pct: float,
    ) -> Dict[str, Any]:
        """Records apriori bundle recommendations and discount calculations."""
        return {
            "target_product_id": target_product_id,
            "bundle_items_count": len(bundle_items),
            "original_total": round(original_price, 2),
            "bundle_total": round(bundle_price, 2),
            "savings_pct": round(savings_pct, 1),
        }

    def trace_span(self, name: str, span_type: str = "AGENT"):
        """Decorator to record custom execution spans with duration."""
        def decorator(func: Callable):
            @functools.wraps(func)
            def wrapper(*args, **kwargs):
                t0 = time.perf_counter()
                status = "SUCCESS"
                try:
                    return func(*args, **kwargs)
                except Exception as e:
                    status = "ERROR"
                    raise
                finally:
                    duration_ms = (time.perf_counter() - t0) * 1000
                    logger.debug(
                        f"[TRACE: SPAN] name='{name}', type='{span_type}', "
                        f"status='{status}', duration={duration_ms:.2f}ms"
                    )
            return wrapper
        return decorator


# Singleton instance
agent_tracer = AgentTracer()

__all__ = ["agent_tracer", "AgentTracer"]
