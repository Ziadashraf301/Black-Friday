"""AI-8: cache and LLM failures that silently degrade the answer are logged at warning, not debug."""

from unittest.mock import MagicMock

from ai.services.cache_service import TwoTierCacheService
from ai.services.synthesis_service import SynthesisService


def _service(repo):
    return TwoTierCacheService(cache_manager_instance=MagicMock(is_available=False), repo=repo)


def test_vector_lookup_failure_is_a_warning(caplog):
    repo = MagicMock()
    repo.find_semantic_cached_response.side_effect = RuntimeError("db down")
    with caplog.at_level("WARNING"):
        assert _service(repo).get_vector_semantic_response("q", "u", query_vec=[0.0]) is None
    assert any(r.levelname == "WARNING" and "db down" in r.getMessage() for r in caplog.records)


def test_vector_store_failure_is_a_warning(caplog):
    repo = MagicMock()
    repo.save_semantic_cache_entry.side_effect = RuntimeError("insert failed")
    with caplog.at_level("WARNING"):
        assert _service(repo).store_vector_semantic_response("q", "u", "r", {}, "I", query_vec=[0.0]) is False
    assert any(r.levelname == "WARNING" and "insert failed" in r.getMessage() for r in caplog.records)


def test_llm_failure_falls_back_to_template_with_a_warning(caplog):
    client = MagicMock()
    client.models.generate_content.side_effect = RuntimeError("quota exceeded")
    with caplog.at_level("WARNING"):
        assert SynthesisService(llm_client=client).generate_llm_rag("ctx", "q") is None
    assert any(r.levelname == "WARNING" and "quota exceeded" in r.getMessage() for r in caplog.records)
