"""CORE-15: response caches are scoped per shopper, and the pgvector cache expires."""

import uuid
from unittest.mock import MagicMock

import pytest
from sqlalchemy import text

from ai.services.cache_service import TwoTierCacheService
from core.db.repository import BlackFridayRepository

VEC = [0.01 * (i % 10) + 0.001 for i in range(768)]


class FakeRedis:
    is_available = True

    def __init__(self):
        self.data = {}

    def get_json(self, key):
        return self.data.get(key)

    def set_json(self, key, value, ttl=None):
        self.data[key] = value
        return True

    def delete_key(self, key):
        return self.data.pop(key, None) is not None

    def delete_pattern(self, pattern):
        return 0


@pytest.fixture
def service():
    return TwoTierCacheService(cache_manager_instance=FakeRedis(), repo=MagicMock())


def test_exact_cache_is_not_shared_between_shoppers(service):
    service.store_exact_llm_response(raw_query="show me jackets", user_id="alice", response_message="for alice")
    assert service.get_exact_llm_response("show me jackets", "alice")["response_message"] == "for alice"
    assert service.get_exact_llm_response("show me jackets", "bob") is None
    # same shopper, differently spelled query still hits (normalised)
    assert service.get_exact_llm_response("  Show me JACKETS ", " Alice ")["response_message"] == "for alice"


def test_invalidate_only_removes_that_shoppers_entry(service):
    for who in ("alice", "bob"):
        service.store_exact_llm_response(raw_query="q", user_id=who, response_message=who)
    assert service.invalidate_exact_cache("q", "alice")
    assert service.get_exact_llm_response("q", "alice") is None
    assert service.get_exact_llm_response("q", "bob") is not None


def test_constraint_cache_is_not_shared_between_shoppers(service):
    service.store_semantic_response("PRODUCT_SEARCH", {"category": "coats"}, "alice", "alice answer")
    assert (
        service.get_semantic_response("PRODUCT_SEARCH", {"category": "coats"}, "alice")["response_message"] == "alice answer"
    )
    assert service.get_semantic_response("PRODUCT_SEARCH", {"category": "coats"}, "bob") is None


@pytest.mark.parametrize("bad", ["", "   ", None])
def test_a_missing_identity_is_an_error_not_a_shared_bucket(service, bad):
    with pytest.raises(ValueError):
        service.user_key(bad)


@pytest.fixture
def repo():
    repo = BlackFridayRepository()
    try:
        with repo.engine.connect() as conn:
            conn.execute(text("SELECT 1 FROM semantic_query_cache LIMIT 1"))
    except Exception as exc:
        pytest.skip(f"PostgreSQL semantic cache unavailable: {exc}")
    keys = []
    yield repo, keys
    with repo.engine.begin() as conn:
        conn.execute(text("DELETE FROM semantic_query_cache WHERE user_key = ANY(:k) OR user_key IS NULL"), {"k": keys})


def _key(keys):
    key = f"core15-{uuid.uuid4().hex}"
    keys.append(key)
    return key


def _save(repo, key, query="velvet jacket", response="answer"):
    assert repo.save_semantic_cache_entry(query, VEC, response, {"cards": []}, "PRODUCT_SEARCH", key)


def _age(repo, key, seconds):
    with repo.engine.begin() as conn:
        conn.execute(
            text("UPDATE semantic_query_cache SET created_at = now() - make_interval(secs => :s) WHERE user_key = :k"),
            {"s": seconds, "k": key},
        )


def test_other_shopper_cannot_hit_my_semantic_entry(repo):
    repo, keys = repo
    alice, bob = _key(keys), _key(keys)
    _save(repo, alice, response="alice's personalised answer")
    assert repo.find_semantic_cached_response(VEC, alice)["response_message"] == "alice's personalised answer"
    assert repo.find_semantic_cached_response(VEC, bob) is None


def test_entry_expires_after_ttl(repo):
    repo, keys = repo
    key = _key(keys)
    _save(repo, key)
    assert repo.find_semantic_cached_response(VEC, key, ttl_seconds=3600) is not None
    _age(repo, key, 7200)
    assert repo.find_semantic_cached_response(VEC, key, ttl_seconds=3600) is None
    # the configured default (6h) also applies when no ttl is passed
    _age(repo, key, 7 * 3600)
    assert repo.find_semantic_cached_response(VEC, key) is None


def test_saving_purges_expired_rows_and_replaces_same_query(repo):
    repo, keys = repo
    old, fresh = _key(keys), _key(keys)
    _save(repo, old)
    _age(repo, old, 7 * 3600)
    _save(repo, fresh, response="v1")
    _save(repo, fresh, response="v2")
    with repo.engine.connect() as conn:
        rows = conn.execute(
            text("SELECT user_key, response_text FROM semantic_query_cache WHERE user_key = ANY(:k)"), {"k": [old, fresh]}
        ).fetchall()
    assert rows == [(fresh, "v2")]


def test_my_entry_is_found_when_many_closer_rows_belong_to_others(repo):
    """CORE-15: an HNSW scan returns only ef_search candidates before the user filter, losing my entry."""
    repo, keys = repo
    me, others = _key(keys), _key(keys)
    vec_str = "[" + ",".join(str(x) for x in VEC) + "]"
    with repo.engine.begin() as conn:
        conn.execute(
            text(
                "INSERT INTO semantic_query_cache (query_text, embedding, response_text, ui_payload, intent, user_key) "
                "SELECT 'q' || g, CAST(:v AS vector), 'theirs', '{}'::jsonb, 'PRODUCT_SEARCH', :k "
                "FROM generate_series(1, 2000) g"
            ),
            {"v": vec_str, "k": others},
        )
        conn.execute(text("ANALYZE semantic_query_cache"))
    near = list(VEC)
    near[0] += 0.02  # still within max_distance, but farther than every other row
    _save(repo, me, response="mine")
    with repo.engine.connect() as conn:
        assert conn.execute(
            text("SELECT (embedding <=> CAST(:v AS vector)) <= 0.08 FROM semantic_query_cache WHERE user_key = :k"),
            {"v": "[" + ",".join(str(x) for x in near) + "]", "k": me},
        ).scalar()
    hit = repo.find_semantic_cached_response(near, me)
    assert hit is not None and hit["response_message"] == "mine"


def test_legacy_rows_without_a_user_never_match(repo):
    repo, keys = repo
    key = _key(keys)
    with repo.engine.begin() as conn:
        conn.execute(
            text(
                "INSERT INTO semantic_query_cache (query_text, embedding, response_text, ui_payload, intent) "
                "VALUES ('legacy', CAST(:v AS vector), 'old shared answer', '{}'::jsonb, 'PRODUCT_SEARCH')"
            ),
            {"v": "[" + ",".join(str(x) for x in VEC) + "]"},
        )
    assert repo.find_semantic_cached_response(VEC, key) is None
