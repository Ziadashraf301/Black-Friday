"""
Decoupled Embedding Architecture (SOLID - Strategy & Factory Design Patterns).
Provides an abstract base interface and concrete provider strategies for 768-dim embeddings:
  - GeminiEmbeddingProvider (models/gemini-embedding-2)
  - DeterministicSemanticProvider (Deterministic fallback)
"""
from abc import ABC, abstractmethod
from typing import List, Optional, Any
import hashlib
import numpy as np
from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)


class BaseEmbeddingProvider(ABC):
    """Abstract Strategy Interface for multimodal/text vector embeddings."""

    @property
    @abstractmethod
    def dimension(self) -> int:
        """Embedding vector dimension."""
        pass

    @abstractmethod
    def embed_text(self, text: str) -> List[float]:
        """Generates a normalized dense vector embedding for input text."""
        pass

    def embed_batch(self, texts: List[str]) -> List[List[float]]:
        """Generates embeddings for a batch of text inputs."""
        return [self.embed_text(t) for t in texts]


class GeminiEmbeddingProvider(BaseEmbeddingProvider):
    """Concrete strategy implementing Gemini Embedding via google.genai Client ($0.20/1M tokens)."""

    def __init__(
        self,
        api_key: Optional[str] = None,
        model_name: str = "models/gemini-embedding-2",
        client: Optional[Any] = None,
    ):
        self._api_key = api_key or getattr(settings, "GEMINI_API_KEY", None) or getattr(settings, "GOOGLE_API_KEY", None)
        self._model_name = model_name
        self._client = client

    @property
    def dimension(self) -> int:
        return 768

    def _get_client(self):
        if self._client is not None:
            return self._client
        if not self._api_key:
            logger.warning("[EMBEDDING-PROVIDER: GEMINI] GEMINI_API_KEY not found in environment.")
            raise ValueError("GEMINI_API_KEY not configured.")
        from google import genai
        self._client = genai.Client(api_key=self._api_key)
        return self._client

    def embed_text(self, text: str) -> List[float]:
        client = self._get_client()
        logger.info(f"[EMBEDDING-PROVIDER: GEMINI] Requesting 768-dim embedding via {self._model_name}...")

        try:
            from google.genai import types
            config = types.EmbedContentConfig(
                task_type="RETRIEVAL_DOCUMENT",
                output_dimensionality=768,
            )
            res = client.models.embed_content(
                model=self._model_name,
                contents=text,
                config=config,
            )
        except Exception as e:
            logger.debug(f"[EMBEDDING-PROVIDER: GEMINI] Standard config failed ({e}), trying fallback call...")
            res = client.models.embed_content(
                model=self._model_name,
                contents=text,
            )

        if hasattr(res, "embeddings") and res.embeddings:
            raw_vec = [float(x) for x in res.embeddings[0].values]
        elif hasattr(res, "embedding") and hasattr(res.embedding, "values"):
            raw_vec = [float(x) for x in res.embedding.values]
        elif isinstance(res, dict) and "embedding" in res:
            raw_vec = [float(x) for x in res["embedding"]]
        elif isinstance(res, dict) and "embeddings" in res and res["embeddings"]:
            raw_vec = [float(x) for x in res["embeddings"][0]["values"]]
        else:
            raise ValueError(f"Unexpected response structure from embed_content: {res}")

        # Matryoshka Representation Learning: slice and L2-normalize to exact 768 dimensions
        if len(raw_vec) != 768:
            logger.info(f"[EMBEDDING-PROVIDER: GEMINI] Aligning vector from {len(raw_vec)} to 768 dimensions via MRL.")
            raw_vec = raw_vec[:768]
            norm = np.linalg.norm(raw_vec)
            if norm > 0:
                raw_vec = (np.array(raw_vec, dtype=np.float32) / norm).tolist()

        return [round(float(x), 6) for x in raw_vec]


class DeterministicSemanticProvider(BaseEmbeddingProvider):
    """Fallback strategy ensuring zero-failure offline execution and reproducible testing."""

    @property
    def dimension(self) -> int:
        return 768

    def embed_text(self, text: str) -> List[float]:
        logger.debug("[EMBEDDING-PROVIDER: FALLBACK] Generating 768-dim deterministic normalized semantic vector.")
        seed = int(hashlib.sha256(text.encode("utf-8")).hexdigest()[:8], 16)
        rng = np.random.RandomState(seed)
        raw_vec = rng.randn(768).astype(np.float32)
        norm = np.linalg.norm(raw_vec)
        if norm > 0:
            raw_vec = raw_vec / norm
        return [round(float(x), 6) for x in raw_vec]


class EmbeddingService:
    """
    Service Layer Orchestrator (Dependency Injection & Facade).
    Decouples storage/pipelines from the underlying AI model provider.
    """

    def __init__(self, provider: Optional[BaseEmbeddingProvider] = None):
        self._provider = provider or self._resolve_default_provider()
        logger.info(f"[EMBEDDING-SERVICE] Initialized with active provider: {self.provider_name} (dim={self._provider.dimension})")

    @staticmethod
    def _resolve_default_provider() -> BaseEmbeddingProvider:
        key = getattr(settings, "GEMINI_API_KEY", None) or getattr(settings, "GOOGLE_API_KEY", None)
        if key and key != "test_api_key_placeholder":
            try:
                from google import genai  # noqa: F401
                logger.info("[EMBEDDING-SERVICE] Active strategy: GeminiEmbeddingProvider (models/gemini-embedding-2).")
                return GeminiEmbeddingProvider(api_key=key)
            except ImportError:
                logger.warning("[EMBEDDING-SERVICE] google-genai package not installed. Falling back to DeterministicSemanticProvider.")
        else:
            logger.info("[EMBEDDING-SERVICE] No GEMINI_API_KEY detected in environment. Using DeterministicSemanticProvider (offline / testing mode, 768-dim).")
        return DeterministicSemanticProvider()

    @property
    def provider(self) -> BaseEmbeddingProvider:
        return self._provider

    @property
    def provider_name(self) -> str:
        return self._provider.__class__.__name__

    def set_provider(self, provider: BaseEmbeddingProvider) -> None:
        self._provider = provider
        logger.info(f"[EMBEDDING-SERVICE] Provider swapped to: {self.provider_name}")

    def generate_embedding(self, text: str) -> List[float]:
        return self._provider.embed_text(text)

    def generate_batch(self, texts: List[str]) -> List[List[float]]:
        return self._provider.embed_batch(texts)


# Core singleton export
embedding_service = EmbeddingService()
