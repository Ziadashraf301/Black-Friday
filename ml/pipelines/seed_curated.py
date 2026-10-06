"""
Ingestion & Incremental Vector Embedding Pipeline (Task P1-07).
Decoupled orchestrator following SOLID principles:
- Uses BlackFridayRepository for data persistence (Repository Pattern)
- Uses core.ai.embedding_service for vector generation (Strategy & Dependency Injection)
"""
import json
from pathlib import Path
from typing import Dict, Any, Optional
from core.config import settings
from core.db.repository import BlackFridayRepository
from core.embeddings import embedding_service, EmbeddingService
from core.logging import get_logger

logger = get_logger(__name__)


class CuratedCatalogSeeder:
    """
    Decoupled ingestion pipeline coordinator.
    Depends on abstractions for repository and embedding services.
    """

    def __init__(
        self,
        repo: Optional[BlackFridayRepository] = None,
        embedder: Optional[EmbeddingService] = None,
    ):
        self.repo = repo or BlackFridayRepository()
        self.embedder = embedder or embedding_service

    def run(self, json_path: Optional[Path] = None, force_reembed: bool = False) -> Dict[str, Any]:
        catalog_path = json_path or (settings.BASE_DIR / "data" / "curated_products.json")
        if not catalog_path.exists():
            logger.warning(f"Curated catalog file not found: {catalog_path}")
            return {"seeded_count": 0, "embedded_count": 0, "skipped_count": 0}

        with open(catalog_path, "r", encoding="utf-8") as f:
            products = json.load(f)

        # 1. Ingest/Merge into PostgreSQL repository
        self.repo.seed_curated_products(products)
        logger.info(f"Seeded {len(products)} products into warehouse.")

        # 2. Incremental vector embedding skip logic
        if force_reembed:
            items_to_embed = products
        else:
            items_to_embed = self.repo.get_products_missing_embeddings()

        embedded_count = 0
        skipped_count = len(products) - len(items_to_embed)

        logger.info(f"Incremental embeddings: {len(items_to_embed)} items to embed, {skipped_count} skipped.")
        if items_to_embed:
            logger.info(f"[CATALOG INGESTION] Embedding {len(items_to_embed)} items via Strategy: {self.embedder.provider_name} (dim={self.embedder.provider.dimension})")

        for item in items_to_embed:
            pid = item["product_id"]
            context = " ".join(filter(None, [
                item.get("name"),
                item.get("tagline"),
                item.get("description"),
                item.get("category_name"),
                item.get("style"),
                item.get("brand"),
            ]))
            vector = self.embedder.generate_embedding(context)
            self.repo.update_product_embedding(pid, vector, context)
            embedded_count += 1

        logger.info(f"Catalog seeding finished: {len(products)} total, {embedded_count} embedded, {skipped_count} skipped.")
        return {
            "seeded_count": len(products),
            "embedded_count": embedded_count,
            "skipped_count": skipped_count,
        }


def run_seed_curated_catalog(force_reembed: bool = False) -> Dict[str, Any]:
    return CuratedCatalogSeeder().run(force_reembed=force_reembed)


if __name__ == "__main__":
    run_seed_curated_catalog(force_reembed=False)

