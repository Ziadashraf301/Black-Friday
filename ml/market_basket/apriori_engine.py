import pandas as pd
from typing import Dict, Any, List
from mlxtend.frequent_patterns import apriori, association_rules
from core.logging import get_logger

logger = get_logger(__name__)


class AprioriAssociationEngine:
    """Mines frequent itemsets and derives association rules using Apriori algorithm."""

    def __init__(self, min_support: float = 0.05, min_confidence: float = 0.40):
        self.min_support = min_support
        self.min_confidence = min_confidence
        self.frequent_itemsets: pd.DataFrame = pd.DataFrame()
        self.rules: pd.DataFrame = pd.DataFrame()

    def mine_rules(self, basket_matrix_df: pd.DataFrame) -> pd.DataFrame:
        """Runs Apriori and association rule generation."""
        logger.info(f"Mining frequent itemsets (min_support={self.min_support})...")
        self.frequent_itemsets = apriori(
            basket_matrix_df,
            min_support=self.min_support,
            use_colnames=True
        )

        logger.info(f"Discovered {len(self.frequent_itemsets):,} frequent itemsets. Generating association rules (min_confidence={self.min_confidence})...")
        if self.frequent_itemsets.empty:
            return pd.DataFrame()

        rules = association_rules(
            self.frequent_itemsets,
            metric="confidence",
            min_threshold=self.min_confidence
        )

        # Categorize lift levels matching R methodology
        def assign_lift_level(lift: float) -> str:
            if lift < 2.0:
                return "1-2"
            elif lift < 3.0:
                return "2-3"
            elif lift < 4.0:
                return "3-4"
            elif lift < 5.0:
                return "4-5"
            elif lift < 6.0:
                return "5-6"
            else:
                return "6+"

        rules["lift_level"] = rules["lift"].apply(assign_lift_level)
        rules = rules.sort_values(by="support", ascending=False)
        self.rules = rules
        logger.info(f"Derived {len(self.rules):,} association rules.")
        return rules

    def get_top_rules(self, limit: int = 15) -> List[Dict[str, Any]]:
        """Returns top rules sorted by support."""
        if self.rules.empty:
            return []
        top_df = self.rules.head(limit).copy()
        top_df["antecedents"] = top_df["antecedents"].apply(lambda s: list(s))
        top_df["consequents"] = top_df["consequents"].apply(lambda s: list(s))
        return top_df.to_dict(orient="records")
