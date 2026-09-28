import json
import os
from pathlib import Path
from core.config import settings
from core.db.repository import BlackFridayRepository
from core.logging import get_logger

logger = get_logger(__name__)


def run_seed_curated_catalog():
    """Reads data/curated_products.json and ingests it into PostgreSQL database."""
    json_path = settings.BASE_DIR / "data" / "curated_products.json"
    if not json_path.exists():
        logger.warning(f"Curated products JSON file not found at: {json_path}")
        return

    with open(json_path, "r", encoding="utf-8") as f:
        products = json.load(f)

    repo = BlackFridayRepository()
    repo.seed_curated_products(products)
    logger.info(f"Successfully seeded {len(products)} curated products into database.")


if __name__ == "__main__":
    run_seed_curated_catalog()
