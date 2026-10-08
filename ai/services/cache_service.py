"""
Two-Tier AI Caching Service Layer (Phase 4 - Task P4-02).
Provides:
  - Tier 0: Hard LLM Response Message Cache (TTL 6 Hours = 21,600s) in Redis for exact query hits (<1ms).
  - Tier 1: True Vector Semantic Cache in PostgreSQL with pgvector HNSW index
    (Cosine similarity >= 0.92, max_distance <= 0.08) on 768-dim embeddings (<5ms).
"""
import hashlib
import json
import time
from datetime import datetime, timezone
from typing import Optional, Dict, Any, List

from core.cache.redis_client import RedisCacheManager, cache_manager
from core.db.repository import BlackFridayRepository
from core.logging import get_logger

logger = get_logger(__name__)


class TwoTierCacheService:
    """
    Two-Tier Caching Pipeline for Black Friday Conversational AI.
    Tier 0: Sub-millisecond exact SHA-256 Redis cache (6-hour sliding window).
    Tier 1: Vector Semantic Cache using 768-dim query embeddings and pgvector cosine similarity (>= 0.92).
    """

    EXACT_CACHE_PREFIX = "cache:llm:response"
    SEMANTIC_CACHE_PREFIX = "cache:semantic"
    DEFAULT_6H_TTL = 21600  # 6 hours in seconds
    DEFAULT_POLICY_TTL = 2592000  # 30 days in seconds
    DEFAULT_SEMANTIC_TTL = 86400  # 24 hours in seconds

    def __init__(
        self,
        cache_manager_instance: Optional[RedisCacheManager] = None,
        repo: Optional[BlackFridayRepository] = None,
    ):
        self._cache = cache_manager_instance or cache_manager
        self._repo = repo or BlackFridayRepository()

    @staticmethod
    def hash_query(raw_query: str) -> str:
        """Computes deterministic SHA-256 fingerprint for a normalized user query."""
        normalized = raw_query.strip().lower()
        return hashlib.sha256(normalized.encode("utf-8")).hexdigest()

    @staticmethod
    def hash_constraints(intent: str, constraints: Dict[str, Any]) -> str:
        """Computes deterministic SHA-256 fingerprint for intent + sorted constraints."""
        clean_constraints = {}
        for k, v in sorted(constraints.items()):
            if v is not None and v != "" and v != []:
                if isinstance(v, list):
                    clean_constraints[k] = sorted(str(item).lower() for item in v)
                else:
                    clean_constraints[k] = str(v).lower()
        serialized = f"{intent.upper()}:{json.dumps(clean_constraints, sort_keys=True)}"
        return hashlib.sha256(serialized.encode("utf-8")).hexdigest()

    # =========================================================================
    # Tier 0: Hard LLM Response Message Cache (6-Hour Window in Redis)
    # =========================================================================
    def get_exact_llm_response(self, raw_query: str) -> Optional[Dict[str, Any]]:
        """
        Looks up exact raw query in 6-hour hard response cache.
        Returns stored response message and UI cards if within 6-hour window, else None.
        """
        if not self._cache.is_available:
            return None

        t0 = time.perf_counter()
        q_hash = self.hash_query(raw_query)
        cache_key = f"{self.EXACT_CACHE_PREFIX}:{q_hash}"
        data = self._cache.get_json(cache_key)
        latency_ms = (time.perf_counter() - t0) * 1000

        is_hit = bool(data)
        try:
            from ai.observability.tracing import agent_tracer
            agent_tracer.trace_cache_lookup(raw_query=raw_query, tier="TIER_0_HARD_6H", is_hit=is_hit, latency_ms=latency_ms)
        except Exception:
            pass

        if data:
            logger.info(f"[CACHE: TIER-0 HIT] Query hash={q_hash[:10]} found in 6h LLM response cache ({latency_ms:.2f}ms)")
            data["is_cache_hit"] = True
            data["cache_tier"] = "TIER_0_HARD_6H"
            return data

        return None

    def store_exact_llm_response(
        self,
        raw_query: str,
        response_message: str,
        ui_payload: Optional[Dict[str, Any]] = None,
        target_intents: Optional[List[str]] = None,
        retrieved_products: Optional[List[Dict[str, Any]]] = None,
        product_details: Optional[Dict[str, Any]] = None,
        bundle_recommendations: Optional[List[Dict[str, Any]]] = None,
        order_status: Optional[Dict[str, Any]] = None,
        policy_details: Optional[Dict[str, Any]] = None,
        ttl: Optional[int] = None,
    ) -> bool:
        """Stores generated LLM response, UI cards, and state artifacts into 6-hour hard Redis cache."""
        if not self._cache.is_available:
            return False

        q_hash = self.hash_query(raw_query)
        cache_key = f"{self.EXACT_CACHE_PREFIX}:{q_hash}"
        ttl_seconds = ttl if ttl is not None else self.DEFAULT_6H_TTL

        payload = {
            "query": raw_query,
            "response_message": response_message,
            "ui_payload": ui_payload or {},
            "target_intents": target_intents or [],
            "retrieved_products": retrieved_products or [],
            "product_details": product_details,
            "bundle_recommendations": bundle_recommendations or [],
            "order_status": order_status,
            "policy_details": policy_details,
            "cached_at": datetime.now(timezone.utc).isoformat(),
            "expires_in_seconds": ttl_seconds,
        }

        success = self._cache.set_json(cache_key, payload, ttl=ttl_seconds)
        if success:
            logger.info(f"[CACHE: TIER-0 STORE] Cached LLM response for query hash={q_hash[:10]} (TTL={ttl_seconds}s)")
        return success


    # =========================================================================
    # Tier 1: True Vector Semantic Cache (PostgreSQL pgvector HNSW Index)
    # =========================================================================
    def get_vector_semantic_response(
        self,
        raw_query: str,
        query_vec: Optional[List[float]] = None,
        max_distance: float = 0.08,  # Cosine similarity >= 0.92
    ) -> Optional[Dict[str, Any]]:
        """
        True Vector Semantic Caching.
        Compares 768-dim query embedding against cached queries in PostgreSQL via pgvector.
        Returns cached response if cosine similarity >= 0.92 (distance <= 0.08).
        """
        t0 = time.perf_counter()

        # Generate embedding if not already provided
        if query_vec is None:
            try:
                from core.embeddings import embedding_service
                query_vec = embedding_service.generate_embedding(raw_query)
            except Exception as e:
                logger.debug(f"[CACHE: SEMANTIC] Embedding generation failed: {e}")
                return None

        # Query pgvector HNSW index
        try:
            cached = self._repo.find_semantic_cached_response(query_vec=query_vec, max_distance=max_distance)
            latency_ms = (time.perf_counter() - t0) * 1000

            is_hit = bool(cached)
            try:
                from ai.observability.tracing import agent_tracer
                agent_tracer.trace_cache_lookup(raw_query=raw_query, tier="TIER_1_SEMANTIC_VECTOR", is_hit=is_hit, latency_ms=latency_ms)
            except Exception:
                pass

            if cached:
                sim = cached.get("cosine_similarity", 0.95)
                logger.info(
                    f"[CACHE: TIER-1 HIT] Vector semantic match found! "
                    f"Matched='{cached.get('matched_query')[:40]}...', Sim={sim:.4f} ({latency_ms:.2f}ms)"
                )
                return cached
        except Exception as e:
            logger.debug(f"[CACHE: SEMANTIC] Vector cache lookup error: {e}")

        return None

    def store_vector_semantic_response(
        self,
        query_text: str,
        response_text: str,
        ui_payload: Dict[str, Any],
        intent: str,
        query_vec: Optional[List[float]] = None,
    ) -> bool:
        """Stores a verified query and answer in pgvector semantic_query_cache."""
        if query_vec is None:
            try:
                from core.embeddings import embedding_service
                query_vec = embedding_service.generate_embedding(query_text)
            except Exception as e:
                logger.debug(f"[CACHE: SEMANTIC] Embedding generation for store failed: {e}")
                return False

        try:
            stored = self._repo.save_semantic_cache_entry(
                query_text=query_text,
                query_vec=query_vec,
                response_text=response_text,
                ui_payload=ui_payload,
                intent=intent,
            )
            if stored:
                logger.info(f"[CACHE: TIER-1 STORE] Vector semantic cache entry stored for query: '{query_text[:50]}'")
            return stored
        except Exception as e:
            logger.debug(f"[CACHE: SEMANTIC] Vector cache store error: {e}")
            return False

    # =========================================================================
    # Compatibility Key-Value Semantic Methods (Constraint Hashing)
    # =========================================================================
    def get_semantic_response(self, intent: str, constraints: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        """Structured constraint cache lookup."""
        if not self._cache.is_available:
            return None
        c_hash = self.hash_constraints(intent, constraints)
        cache_key = f"{self.SEMANTIC_CACHE_PREFIX}:{intent.upper()}:{c_hash}"
        data = self._cache.get_json(cache_key)
        if data:
            data["is_cache_hit"] = True
            data["cache_tier"] = "TIER_1_SEMANTIC"
            return data
        return None

    def store_semantic_response(
        self,
        intent: str,
        constraints: Dict[str, Any],
        answer_text: str,
        ui_payload: Optional[Dict[str, Any]] = None,
        ttl: Optional[int] = None,
    ) -> bool:
        """Structured constraint cache store."""
        if not self._cache.is_available:
            return False
        c_hash = self.hash_constraints(intent, constraints)
        cache_key = f"{self.SEMANTIC_CACHE_PREFIX}:{intent.upper()}:{c_hash}"
        ttl_seconds = ttl or self.DEFAULT_SEMANTIC_TTL
        payload = {
            "intent": intent.upper(),
            "constraints": constraints,
            "response_message": answer_text,
            "ui_payload": ui_payload or {},
            "cached_at": datetime.now(timezone.utc).isoformat(),
            "expires_in_seconds": ttl_seconds,
        }
        return self._cache.set_json(cache_key, payload, ttl=ttl_seconds)

    def invalidate_exact_cache(self, raw_query: str) -> bool:
        """Explicitly evicts a query from exact cache."""
        q_hash = self.hash_query(raw_query)
        return self._cache.delete_key(f"{self.EXACT_CACHE_PREFIX}:{q_hash}")

    def clear_all_llm_cache(self) -> int:
        """Clears all cached LLM responses."""
        return self._cache.delete_pattern(f"{self.EXACT_CACHE_PREFIX}:*")


# Singleton instance
cache_service = TwoTierCacheService()

__all__ = ["TwoTierCacheService", "cache_service"]
