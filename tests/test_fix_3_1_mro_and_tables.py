"""
Regression test for Fix 3.1:
- Resolving MRO collision so BaseRepository.create_app_tables creates all registered ORM tables
- Verified on an isolated scratch database (never using production/dev data)
- Verify pgvector extension requirement
"""
import uuid
from sqlalchemy import create_engine, text, inspect
from core.config import settings
from core.db.repositories.base import BaseRepository
from core.db.repository import BlackFridayRepository
from core.db.models import Base


def test_base_repository_create_app_tables_on_scratch_db():
    scratch_db_name = f"test_scratch_{uuid.uuid4().hex[:8]}"
    admin_engine = create_engine(settings.database_url, isolation_level="AUTOCOMMIT")

    scratch_engine = None
    try:
        with admin_engine.connect() as conn:
            conn.execute(text(f"CREATE DATABASE {scratch_db_name};"))

        scratch_url = settings.database_url.rsplit("/", 1)[0] + f"/{scratch_db_name}"
        scratch_engine = create_engine(scratch_url)

        # 1. BaseRepository.create_app_tables creates all ORM tables
        base_repo = BaseRepository(engine=scratch_engine)
        base_repo.create_app_tables()

        inspector = inspect(scratch_engine)
        created_tables = set(inspector.get_table_names())
        expected_tables = {
            "raw_black_friday",
            "black_friday_cleaned",
            "customer_segments",
            "product_network_metrics",
            "curated_products",
            "app_users",
            "user_purchases",
            "user_carts",
        }
        orm_tables = set(Base.metadata.tables.keys())

        # Verify all expected tables match ORM models and exist in database
        assert expected_tables == orm_tables, f"Mismatch between expected and ORM tables: {expected_tables ^ orm_tables}"
        assert expected_tables.issubset(created_tables), f"Missing tables: {expected_tables - created_tables}"

        # Verify pgvector extension requirement
        with scratch_engine.connect() as conn:
            ext = conn.execute(text("SELECT extname FROM pg_extension WHERE extname = 'vector';")).scalar()
            assert ext == "vector", "pgvector extension was not created"

            # Verify vector column on curated_products
            col_type = conn.execute(text("""
                SELECT udt_name FROM information_schema.columns 
                WHERE table_name = 'curated_products' AND column_name = 'embedding';
            """)).scalar()
            assert col_type == "vector", f"curated_products.embedding column type is {col_type}, expected vector"

        # 2. BlackFridayRepository facade delegates create_app_tables to BaseRepository
        facade_repo = BlackFridayRepository(engine=scratch_engine)
        assert hasattr(facade_repo, "create_app_tables")
        assert hasattr(facade_repo, "ensure_user_tables")

    finally:
        if scratch_engine:
            scratch_engine.dispose()
        admin_engine.dispose()
        # Drop scratch database forcibly
        drop_engine = create_engine(settings.database_url, isolation_level="AUTOCOMMIT")
        with drop_engine.connect() as conn:
            conn.execute(text(f"DROP DATABASE IF EXISTS {scratch_db_name} WITH (FORCE);"))
        drop_engine.dispose()
