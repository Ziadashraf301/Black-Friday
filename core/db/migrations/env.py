"""
Alembic environment for the warehouse database.

The URL comes from core.config.settings, unless the caller passes a connection
(config.attributes["connection"]) or a URL (config.attributes["url"]), which is how the tests
migrate a scratch database instead of the warehouse.
"""
from alembic import context
from sqlalchemy import create_engine

from core.config import settings
from core.db.models import Base

config = context.config
target_metadata = Base.metadata


def _include_object(obj, name, type_, reflected, compare_to):
    # Alembic cannot compare the pgvector / tsvector index operators; the migrations create them explicitly.
    return not (type_ == "index" and name in {"idx_curated_embedding_hnsw", "idx_semantic_cache_hnsw"})


def _configure(**kwargs) -> None:
    context.configure(target_metadata=target_metadata, include_object=_include_object, compare_type=True, **kwargs)


def run_migrations_offline() -> None:
    _configure(url=config.attributes.get("url", settings.database_url), literal_binds=True)
    with context.begin_transaction():
        context.run_migrations()


def run_migrations_online() -> None:
    connection = config.attributes.get("connection")
    if connection is not None:
        _configure(connection=connection)
        with context.begin_transaction():
            context.run_migrations()
        return

    engine = create_engine(config.attributes.get("url", settings.database_url))
    try:
        with engine.connect() as conn:
            _configure(connection=conn)
            with context.begin_transaction():
                context.run_migrations()
    finally:
        engine.dispose()


if context.is_offline_mode():
    run_migrations_offline()
else:
    run_migrations_online()
