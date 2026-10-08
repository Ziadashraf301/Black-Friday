import os
import pandas as pd
from core.db.repository import BlackFridayRepository
from ml.features.data_contract import validate_raw_data
from core.config import settings
from core.logging import get_logger

from typing import Optional

logger = get_logger(__name__)

def run_ingestion(csv_path: Optional[str] = None):
    """Ingests raw CSV transaction data into raw_black_friday staging table via repository layer."""
    if csv_path is None:
        csv_path = settings.TRAIN_DATA_PATH

    resolved_path = str(settings.BASE_DIR / csv_path) if not os.path.isabs(csv_path) else csv_path
    if not os.path.exists(resolved_path):
        raise FileNotFoundError(f"Source file not found at: {resolved_path}")

    logger.info(f"Loading raw data from: {resolved_path}")
    raw_df = pd.read_csv(resolved_path)

    # Standardize column naming to snake_case matching schema
    raw_df.columns = [c.lower() for c in raw_df.columns]
    logger.info(f"Loaded {len(raw_df):,} raw records with columns: {list(raw_df.columns)}")

    # Validate against Pandera data contract (no-op if pandera not installed)
    raw_df = validate_raw_data(raw_df)

    # Delegate all database execution to repository
    repo = BlackFridayRepository()
    repo.truncate_raw_table()
    repo.insert_raw_batch(raw_df)

    try:
        from core.cache import cache_manager
        cache_manager.delete_pattern("analytics:*")
        logger.info("Invalidated analytics cache entries.")
    except Exception as e:
        logger.warning(f"Analytics cache invalidation warning: {e}")

    logger.info("Raw data ingestion successfully finished.")

if __name__ == "__main__":
    run_ingestion()
