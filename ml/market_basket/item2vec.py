import numpy as np
import pandas as pd
from typing import List, Dict, Tuple, Optional, Any
from core.logging import get_logger

logger = get_logger(__name__)

try:
    from gensim.models import Word2Vec
    HAS_GENSIM = True
except ImportError:
    HAS_GENSIM = False
    logger.warning("Gensim is not installed in the local environment.")


class Item2VecRecommender:
    """Computes dense 32-dimensional product embedding vectors from customer baskets."""

    def __init__(self, vector_size: int = 32, window: int = 5, min_count: int = 5, epochs: int = 10):
        self.vector_size = vector_size
        self.window = window
        self.min_count = min_count
        self.epochs = epochs
        self.model = None

    def fit(self, baskets: List[List[str]]) -> "Item2VecRecommender":
        """Trains Word2Vec / Item2Vec model treating baskets as sentences and products as words."""
        if not HAS_GENSIM:
            logger.warning("Gensim is unavailable. Skipping Item2Vec fitting.")
            return self

        logger.info(f"Training Item2Vec model on {len(baskets):,} customer baskets (vector_size={self.vector_size})...")
        self.model = Word2Vec(
            sentences=baskets,
            vector_size=self.vector_size,
            window=self.window,
            min_count=self.min_count,
            sg=1,  # Skip-gram
            workers=4,
            epochs=self.epochs
        )
        logger.info(f"Item2Vec training complete. Vocabulary size: {len(self.model.wv):,} products.")
        return self

    def get_similar_products(self, product_id: str, top_n: int = 5) -> List[Tuple[str, float]]:
        """Finds most similar products in dense embedding space via Cosine Similarity."""
        if not HAS_GENSIM or not self.model:
            return []

        if product_id not in self.model.wv:
            logger.warning(f"Product {product_id} not found in Item2Vec vocabulary.")
            return []

        similar = self.model.wv.most_similar(product_id, topn=top_n)
        return [(prod, round(float(sim), 4)) for prod, sim in similar]

    def get_all_similar_products(self, top_n: int = 5, min_similarity: float = 0.0) -> Dict[str, List[Tuple[str, float]]]:
        """Computes top-N nearest neighbors for all products in vocabulary in a single efficient pass."""
        if not HAS_GENSIM or not self.model:
            return {}

        results = {}
        for pid in self.model.wv.index_to_key:
            try:
                similar = self.model.wv.most_similar(pid, topn=top_n)
                filtered = [(prod, round(float(sim), 4)) for prod, sim in similar if sim >= min_similarity]
                results[pid] = filtered
            except Exception:
                results[pid] = []
        return results

    @property
    def vocabulary(self) -> List[str]:
        """Returns the list of product IDs in the model's vocabulary."""
        if HAS_GENSIM and self.model and hasattr(self.model.wv, "index_to_key"):
            return list(self.model.wv.index_to_key)
        return []

    def get_embedding(self, product_id: str) -> Optional[np.ndarray]:
        """Returns embedding vector for a given product ID."""
        if HAS_GENSIM and self.model and product_id in self.model.wv:
            return self.model.wv[product_id]
        return None

    def evaluate_embeddings(
        self,
        baskets: Optional[List[List[str]]] = None,
        rules_df: Optional[pd.DataFrame] = None,
        top_n: int = 5
    ) -> Dict[str, float]:
        """Evaluates trained Item2Vec embeddings using vector norms, cosine similarity compactness,
        and alignment with mined Apriori association rules."""
        metrics = {
            "item2vec_vocab_size": 0.0,
            "item2vec_catalog_coverage_pct": 0.0,
            "item2vec_avg_top_k_cosine_sim": 0.0,
            "item2vec_mean_vector_norm": 0.0,
            "item2vec_apriori_alignment_score": 0.0
        }

        if not HAS_GENSIM or not self.model or len(self.vocabulary) == 0:
            return metrics

        vocab = self.vocabulary
        metrics["item2vec_vocab_size"] = float(len(vocab))

        # Catalog Coverage
        if baskets:
            all_unique_items = set(item for basket in baskets for item in basket)
            coverage = (len(vocab) / max(len(all_unique_items), 1)) * 100.0
            metrics["item2vec_catalog_coverage_pct"] = round(float(coverage), 2)

        # Average Vector Norm
        norms = [float(np.linalg.norm(self.model.wv[pid])) for pid in vocab]
        metrics["item2vec_mean_vector_norm"] = round(float(np.mean(norms)), 4) if norms else 0.0

        # Cosine Similarity Compactness (average top-N neighbor similarity across vocab)
        all_sims = []
        for pid in vocab:
            sims = [sim for _, sim in self.get_similar_products(pid, top_n=top_n)]
            all_sims.extend(sims)
        if all_sims:
            metrics["item2vec_avg_top_k_cosine_sim"] = round(float(np.mean(all_sims)), 4)

        # Helper to extract item IDs as a list of strings from frozenset/set/list/str
        def extract_items(val: Any) -> List[str]:
            if isinstance(val, (set, frozenset, list, tuple)):
                return [str(x) for x in val]
            elif isinstance(val, str):
                cleaned = val.replace("frozenset(", "").replace(")", "").replace("[", "").replace("]", "").replace("{", "").replace("}", "").replace("'", "").replace('"', "").strip()
                return [s.strip() for s in cleaned.split(",") if s.strip()]
            return []

        # Apriori Rule Alignment Score (mean cosine similarity for product pairs present in Apriori rules)
        if rules_df is not None and not rules_df.empty:
            apriori_sims = []
            for _, row in rules_df.iterrows():
                ants = extract_items(row["antecedents"])
                cons = extract_items(row["consequents"])
                for ant in ants:
                    for con in cons:
                        if ant in self.model.wv and con in self.model.wv:
                            sim = float(self.model.wv.similarity(ant, con))
                            apriori_sims.append(sim)
            if apriori_sims:
                metrics["item2vec_apriori_alignment_score"] = round(float(np.mean(apriori_sims)), 4)

        logger.info(f"Item2Vec embedding evaluation metrics: {metrics}")
        return metrics
