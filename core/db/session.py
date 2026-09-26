from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker, Session
from contextlib import contextmanager
from typing import Generator
from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)

# SQLAlchemy Engine with connection pool
engine = create_engine(
    settings.database_url,
    pool_size=10,
    max_overflow=20,
    pool_recycle=3600,
    pool_pre_ping=True
)

SessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)

def get_db_engine():
    """Returns the central database engine."""
    return engine

@contextmanager
def get_db_session() -> Generator[Session, None, None]:
    """Context manager for scoped database sessions."""
    session = SessionLocal()
    try:
        yield session
        session.commit()
    except Exception as e:
        session.rollback()
        logger.error(f"Database session error: {e}", exc_info=True)
        raise
    finally:
        session.close()

def get_db():
    """FastAPI dependency yielding a database session."""
    with get_db_session() as session:
        yield session
