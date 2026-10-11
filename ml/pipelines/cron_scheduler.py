"""
Automated 6-Hour ML Pipeline & Drift Monitor Cron Scheduler.

Executes offline ML pipelines (customer segmentation, market basket network,
Evidently AI drift monitoring), and re-seeds catalog metrics periodically.

Note for Production High-Availability Deployments:
This scheduler runs as an in-process timer loop suitable for standalone/development
environments. For distributed, multi-worker production deployments, scheduled jobs
should be migrated to a distributed task queue (e.g., Celery, ARQ, or APScheduler)
with persistent locking to prevent concurrent overlapping executions across nodes.
"""
import time
import datetime
from core.config import settings
from core.db.repository import BlackFridayRepository
from core.logging import get_logger

from ml.pipelines.common import invalidate_caches
from ml.pipelines.segmentation import run_customer_segmentation
from ml.pipelines.market_basket import run_market_basket_pipeline
from ml.pipelines.monitor import run_production_drift_monitor
from ml.pipelines.seed_curated import run_seed_curated_catalog

logger = get_logger(__name__)

CRON_INTERVAL_SECONDS = 6 * 3600  # 6 hours = 21,600 seconds
DRIFT_BATCH_SIZE = 5000


def _load_drift_batch(repo: BlackFridayRepository):
    """Holdout rows to monitor. Outliers are excluded like the training reference, and the sample is seeded
    (a bare LIMIT would return arbitrary rows and flag spurious target drift from the outlier tail)."""
    batch_df = repo.get_cleaned_records_df(split="test", exclude_outliers=True)
    if len(batch_df) > DRIFT_BATCH_SIZE:
        batch_df = batch_df.sample(n=DRIFT_BATCH_SIZE, random_state=settings.RANDOM_SEED)
    return batch_df


def run_cron_cycle():
    """Executes a full 6-hour batch cycle for offline ML pipelines and catalog seeding."""
    now_str = datetime.datetime.now(datetime.timezone.utc).isoformat()
    logger.info(f"=== Starting 6-Hour Scheduled ML Pipeline Cycle [{now_str}] ===")
    repo = BlackFridayRepository()

    try:
        # 1. Customer Segmentation Pipeline
        logger.info("Executing Customer Segmentation (Gower complete-linkage)...")
        run_customer_segmentation()
    except Exception as e:
        logger.error(f"Cron cycle segmentation error: {e}", exc_info=True)

    try:
        # 2. Market Basket & Item2Vec Pipeline
        logger.info("Executing Market Basket & Item2Vec Network Pipeline...")
        run_market_basket_pipeline()
    except Exception as e:
        logger.error(f"Cron cycle market basket error: {e}", exc_info=True)

    try:
        # 3. Production Drift Monitor (monitors recent batch against training baseline)
        logger.info("Executing Evidently AI Production Drift Monitor...")
        batch_df = _load_drift_batch(repo)
        if not batch_df.empty:
            run_production_drift_monitor(current_batch_df=batch_df, auto_trigger_retrain=False)
    except Exception as e:
        logger.error(f"Cron cycle drift monitor error: {e}", exc_info=True)

    try:
        # 4. Catalog Database Re-Seeding
        logger.info("Refreshing database curated catalog table...")
        run_seed_curated_catalog()
    except Exception as e:
        logger.error(f"Cron cycle curated catalog seed error: {e}", exc_info=True)

    # Prices, catalog and analytics payloads may all have changed during the cycle.
    invalidate_caches("price:*", "shopper:*", "analytics:*")

    logger.info(f"=== Finished 6-Hour Scheduled ML Pipeline Cycle ===")


def start_daemon_scheduler():
    """Runs continuous daemon loop triggering execution every 6 hours."""
    logger.info(f"Starting 6-Hour Daemon Cron Scheduler (interval = {CRON_INTERVAL_SECONDS}s)...")
    while True:
        run_cron_cycle()
        logger.info(f"Cron Scheduler sleeping for 6 hours until next cycle...")
        time.sleep(CRON_INTERVAL_SECONDS)


if __name__ == "__main__":
    run_cron_cycle()
