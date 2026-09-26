from core.db.repository import BlackFridayRepository


def get_repository() -> BlackFridayRepository:
    """Dependency injection provider for database warehouse repository access."""
    return BlackFridayRepository()
