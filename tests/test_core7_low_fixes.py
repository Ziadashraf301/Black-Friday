"""CORE-7..10: embedding dimension guard, analytics column validation, pool settings, semantic-cache failure logging."""

from unittest.mock import MagicMock

import pytest

from core.config import settings
from core.db.repositories.analytics_repo import AnalyticsRepository
from core.db.repository import BlackFridayRepository
from core.db.session import engine
from core.embeddings.service import GeminiEmbeddingProvider
from core.exceptions import ValidationError


def _client_returning(values):
    client = MagicMock()
    client.models.embed_content.return_value = {"embedding": values}
    return client


def test_short_embedding_is_rejected():
    provider = GeminiEmbeddingProvider(api_key="k", client=_client_returning([0.1] * 100))
    with pytest.raises(ValueError, match="768"):
        provider.embed_text("hello")


def test_long_embedding_is_sliced_and_normalised():
    provider = GeminiEmbeddingProvider(api_key="k", client=_client_returning([1.0] * 1000))
    vec = provider.embed_text("hello")
    assert len(vec) == 768
    assert abs(sum(x * x for x in vec) - 1.0) < 1e-3


def test_unauthorized_demographic_column_is_validation_error():
    with pytest.raises(ValidationError):
        AnalyticsRepository(engine=MagicMock()).get_demographic_distribution("password_hash; DROP TABLE x")


def test_engine_pool_uses_settings():
    assert engine.pool.size() == settings.DB_POOL_SIZE


def test_semantic_cache_insert_failure_is_logged_as_warning(caplog):
    failing = MagicMock()
    failing.begin.side_effect = RuntimeError("boom")
    repo = BlackFridayRepository(engine=failing)
    with caplog.at_level("WARNING"):
        assert repo.save_semantic_cache_entry("q", [0.0], "r", {}, "intent", "user-key") is False
    assert "Failed to insert semantic cache entry" in caplog.text
