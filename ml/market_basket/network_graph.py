import networkx as nx
import pandas as pd
from typing import Dict, Any, List, Tuple, Optional
from core.logging import get_logger

logger = get_logger(__name__)


class ProductNetworkGraph:
    """Builds product association directed graphs and computes centrality metrics (PageRank, Hubs, Authorities).
    Decoupled from database formats and storage concerns (SRP).
    """

    def __init__(self):
        self.graph = nx.DiGraph()

    def _extract_similarity_map(self, item2vec_model: Any, top_k: int = 5) -> Dict[str, List[Tuple[str, float]]]:
        """Extracts top-k nearest neighbors once using Item2VecRecommender interface or fallback."""
        if hasattr(item2vec_model, "get_all_similar_products"):
            return item2vec_model.get_all_similar_products(top_n=top_k)

        wv = getattr(item2vec_model, "wv", getattr(getattr(item2vec_model, "model", None), "wv", None))
        if wv is not None and hasattr(wv, "index_to_key"):
            results = {}
            for pid in wv.index_to_key:
                try:
                    similar = wv.most_similar(pid, topn=top_k)
                    results[pid] = [(s[0], round(float(s[1]), 4)) for s in similar]
                except Exception:
                    results[pid] = []
            return results
        return {}

    def build_unified_metrics(
        self,
        rules_df: Optional[pd.DataFrame],
        item2vec_model: Any,
        order_counts: Optional[Dict[str, int]] = None,
        top_k: int = 5
    ) -> pd.DataFrame:
        """Constructs unified product metrics table with PageRank, Apriori bundles, and Item2Vec embeddings."""
        logger.info("Building unified product network metrics (Apriori + Item2Vec)...")

        # 1. Compute Item2Vec k-NN similarities ONCE
        sim_map = self._extract_similarity_map(item2vec_model, top_k=top_k)

        # 2. Build graph structure (prefer Apriori rules if present, fallback to Item2Vec similarity map)
        if rules_df is not None and not rules_df.empty:
            metrics_df = self.build_graph_from_rules(rules_df, order_counts=order_counts)
        else:
            logger.info("No Apriori rules found; constructing graph directly from Item2Vec similarity map...")
            metrics_df = self.build_graph_from_similarity_map(sim_map, order_counts=order_counts)

        if metrics_df.empty:
            return pd.DataFrame()

        # 3. Attach Item2Vec recommendations directly as Python lists (storage layer handles serialization)
        metrics_df["item2vec_recommendations"] = metrics_df["product_id"].map(
            lambda pid: [item[0] for item in sim_map.get(pid, [])]
        )

        logger.info(f"Unified product metrics table built successfully ({len(metrics_df):,} products).")
        return metrics_df

    def build_graph_from_rules(
        self, rules_df: pd.DataFrame, order_counts: Optional[Dict[str, int]] = None
    ) -> pd.DataFrame:
        """Constructs graph with products as nodes and rule confidence/lift as edge weights.
        Strictly uses 1-to-1 pairwise rules to prevent edge overwriting from multi-item antecedents.
        """
        logger.info("Building directed network graph from association rules...")
        self.graph.clear()

        # Filter strictly for pairwise rules
        pairwise_rules = rules_df[
            (rules_df["antecedents"].apply(len) == 1) & 
            (rules_df["consequents"].apply(len) == 1)
        ]
        logger.info(f"Filtered to {len(pairwise_rules):,} strictly pairwise rules for the graph.")

        for _, row in pairwise_rules.iterrows():
            u = list(row["antecedents"])[0]
            v = list(row["consequents"])[0]
            confidence = float(row["confidence"])
            lift = float(row["lift"])

            self.graph.add_edge(u, v, weight=confidence, lift=lift)

        return self._compute_metrics(order_counts=order_counts, score_attr="lift")

    def build_graph_from_similarity_map(
        self,
        similarity_map: Dict[str, List[Tuple[str, float]]],
        order_counts: Optional[Dict[str, int]] = None,
        min_similarity: float = 0.30
    ) -> pd.DataFrame:
        """Constructs a k-NN directed similarity graph from a precomputed embedding similarity map."""
        self.graph.clear()
        logger.info(f"Connecting edges for {len(similarity_map):,} products from similarity map...")

        for product_id, neighbors in similarity_map.items():
            for neighbor_id, sim_score in neighbors:
                if sim_score >= min_similarity:
                    self.graph.add_edge(product_id, neighbor_id, weight=sim_score, lift=sim_score)

        return self._compute_metrics(order_counts=order_counts, score_attr="lift")

    def build_graph_from_item2vec(
        self,
        item2vec_model: Any,
        order_counts: Optional[Dict[str, int]] = None,
        top_k: int = 5,
        min_similarity: float = 0.30
    ) -> pd.DataFrame:
        """Convenience method to construct k-NN similarity graph from an Item2Vec model."""
        sim_map = self._extract_similarity_map(item2vec_model, top_k=top_k)
        return self.build_graph_from_similarity_map(sim_map, order_counts=order_counts, min_similarity=min_similarity)

    def _compute_metrics(
        self, order_counts: Optional[Dict[str, int]] = None, score_attr: str = "lift"
    ) -> pd.DataFrame:
        """Calculates PageRank, HITS Hubs, Authorities and formats pure metrics table."""
        if self.graph.number_of_nodes() == 0:
            logger.warning("Network graph is empty. Returning empty metrics DataFrame.")
            return pd.DataFrame()

        counts = order_counts or {}
        logger.info(f"Graph constructed with {self.graph.number_of_nodes()} nodes and {self.graph.number_of_edges()} edges.")

        # Compute PageRank
        logger.info("Computing PageRank scores...")
        pagerank_dict = nx.pagerank(self.graph, weight="weight")

        # Compute HITS (Hubs and Authorities)
        logger.info("Computing HITS Hubs and Authority scores...")
        try:
            hubs_dict, authorities_dict = nx.hits(self.graph, max_iter=200, normalized=True)
        except Exception as e:
            logger.warning(f"HITS algorithm convergence fallback: {e}")
            hubs_dict = {node: 1.0 / len(self.graph) for node in self.graph.nodes()}
            authorities_dict = {node: 1.0 / len(self.graph) for node in self.graph.nodes()}

        # Build product network metrics DataFrame
        records = []
        for node in self.graph.nodes():
            out_edges = list(self.graph.out_edges(node, data=True))
            top_assoc = max(out_edges, key=lambda e: e[2].get(score_attr, 0))[1] if out_edges else None
            max_score = max(out_edges, key=lambda e: e[2].get(score_attr, 0))[2].get(score_attr, 1.0) if out_edges else 1.0

            # Store rich dicts so the API can serve real confidence/lift per recommendation
            bundle_recommendations = sorted(
                [
                    {
                        "product_id": e[1],
                        "confidence": round(float(e[2].get("weight", 0.0)), 4),
                        "lift": round(float(e[2].get("lift", 1.0)), 4),
                    }
                    for e in out_edges
                ],
                key=lambda x: x["confidence"],
                reverse=True
            )[:5]

            records.append({
                "product_id": node,
                "order_count": int(counts.get(node, 0)),
                "pagerank_score": round(float(pagerank_dict.get(node, 0.0)), 5),
                "hub_score": round(float(hubs_dict.get(node, 0.0)), 5),
                "authority_score": round(float(authorities_dict.get(node, 0.0)), 5),
                "top_associated_product": top_assoc,
                "highest_lift_rule": round(float(max_score), 4),
                "top_bundle_recommendations": bundle_recommendations,
            })

        metrics_df = pd.DataFrame(records).sort_values(by="pagerank_score", ascending=False)
        logger.info("Product network centrality computations completed.")
        return metrics_df
