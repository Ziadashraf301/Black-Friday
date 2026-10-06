"""
Multi-Tier Memory & Conversation Checkpointing Subsystem (Phase 3).
Provides state checkpointing for LangGraph multi-turn sessions using Redis,
with automatic graceful degradation to MemorySaver for testing and offline environments.
"""
from typing import Optional, Dict, Any, Iterator
import json
import redis
from langgraph.checkpoint.base import (
    BaseCheckpointSaver,
    Checkpoint,
    CheckpointMetadata,
    CheckpointTuple,
)
from langgraph.checkpoint.memory import MemorySaver
from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)


class RedisSessionCheckpointer(BaseCheckpointSaver):
    """
    Redis-backed CheckpointSaver for LangGraph StateGraph execution.
    Persists multi-turn conversation checkpoints under 'checkpoint:{thread_id}:{checkpoint_id}'.
    """

    def __init__(self, redis_url: Optional[str] = None, ttl_seconds: int = 86400):
        super().__init__()
        self._ttl = ttl_seconds
        self._redis = None
        self._memory_fallback = MemorySaver()

        try:
            url = redis_url or settings.redis_url
            self._redis = redis.from_url(url, decode_responses=False)
            self._redis.ping()
            logger.info("[MEMORY: REDIS] Connected to Redis for LangGraph checkpointing.")
        except Exception as e:
            logger.warning(f"[MEMORY: REDIS] Connection failed ({e}). Falling back to MemorySaver.")

    def get_tuple(self, config: Dict[str, Any]) -> Optional[CheckpointTuple]:
        """Retrieves the latest checkpoint tuple for the configured thread."""
        if not self._redis:
            return self._memory_fallback.get_tuple(config)

        thread_id = config.get("configurable", {}).get("thread_id")
        if not thread_id:
            return None

        try:
            latest_key = f"checkpoint_latest:{thread_id}"
            checkpoint_id = self._redis.get(latest_key)
            if not checkpoint_id:
                return None

            checkpoint_id_str = checkpoint_id.decode("utf-8") if isinstance(checkpoint_id, bytes) else str(checkpoint_id)
            data_key = f"checkpoint:{thread_id}:{checkpoint_id_str}"
            raw = self._redis.get(data_key)
            if not raw:
                return None

            # Deserialization
            payload = json.loads(raw.decode("utf-8") if isinstance(raw, bytes) else raw)
            checkpoint: Checkpoint = payload.get("checkpoint", {})
            metadata: CheckpointMetadata = payload.get("metadata", {})
            parent_config = payload.get("parent_config")

            return CheckpointTuple(
                config=config,
                checkpoint=checkpoint,
                metadata=metadata,
                parent_config=parent_config,
            )
        except Exception as e:
            logger.warning(f"[MEMORY: REDIS] Error loading checkpoint: {e}. Checking memory fallback.")
            return self._memory_fallback.get_tuple(config)

    def list(
        self,
        config: Optional[Dict[str, Any]],
        *,
        filter: Optional[Dict[str, Any]] = None,
        before: Optional[Dict[str, Any]] = None,
        limit: Optional[int] = None,
    ) -> Iterator[CheckpointTuple]:
        """Lists historical checkpoints for thread."""
        if not self._redis:
            yield from self._memory_fallback.list(config, filter=filter, before=before, limit=limit)
            return

        thread_id = config.get("configurable", {}).get("thread_id") if config else None
        if not thread_id:
            return

        try:
            pattern = f"checkpoint:{thread_id}:*"
            keys = self._redis.keys(pattern)
            for k in keys[: (limit or 10)]:
                raw = self._redis.get(k)
                if raw:
                    payload = json.loads(raw.decode("utf-8") if isinstance(raw, bytes) else raw)
                    yield CheckpointTuple(
                        config=config or {},
                        checkpoint=payload.get("checkpoint", {}),
                        metadata=payload.get("metadata", {}),
                        parent_config=payload.get("parent_config"),
                    )
        except Exception:
            yield from self._memory_fallback.list(config, filter=filter, before=before, limit=limit)

    def put(
        self,
        config: Dict[str, Any],
        checkpoint: Checkpoint,
        metadata: CheckpointMetadata,
        new_versions: Optional[Dict[str, Any]] = None,
    ) -> Dict[str, Any]:
        """Stores a checkpoint into Redis with 24-hour sliding TTL."""
        # Always update memory fallback for high-speed local reads
        self._memory_fallback.put(config, checkpoint, metadata, new_versions)

        thread_id = config.get("configurable", {}).get("thread_id", "default_thread")
        checkpoint_id = checkpoint.get("id", "chk_default")

        if self._redis:
            try:
                data_key = f"checkpoint:{thread_id}:{checkpoint_id}"
                latest_key = f"checkpoint_latest:{thread_id}"

                payload = json.dumps({
                    "checkpoint": checkpoint,
                    "metadata": metadata,
                    "parent_config": config,
                }, default=str)

                # Store checkpoint and update latest pointer
                self._redis.setex(data_key, self._ttl, payload)
                self._redis.setex(latest_key, self._ttl, checkpoint_id)
            except Exception as e:
                logger.warning(f"[MEMORY: REDIS] Failed to persist checkpoint to Redis: {e}")

        return {
            "configurable": {
                "thread_id": thread_id,
                "checkpoint_id": checkpoint_id,
            }
        }


# Global checkpointer instance
session_checkpointer = RedisSessionCheckpointer()
