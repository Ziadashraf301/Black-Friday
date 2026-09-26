import pandas as pd
from typing import List, Tuple
from mlxtend.preprocessing import TransactionEncoder
from core.logging import get_logger

logger = get_logger(__name__)

class TransactionBasketEncoder:
    """Encodes transactions into boolean basket vectors for Apriori association mining."""

    def __init__(self):
        self.encoder = TransactionEncoder()

    def build_baskets(self, df: pd.DataFrame) -> List[List[str]]:
        """Groups products into a list of unique baskets keyed by user_id."""
        logger.info(f"Grouping transactions from {len(df):,} rows into user baskets...")
        baskets = df.groupby("user_id")["product_id"].unique().apply(list).tolist()
        logger.info(f"Built {len(baskets):,} unique customer baskets.")
        return baskets

    def fit_transform(self, baskets: List[List[str]]) -> pd.DataFrame:
        """Transforms list of baskets into a boolean DataFrame."""
        logger.info("One-hot encoding baskets into sparse boolean transaction matrix...")
        encoded_array = self.encoder.fit_transform(baskets)
        basket_df = pd.DataFrame(encoded_array, columns=self.encoder.columns_)
        logger.info(f"Encoded matrix shape: {basket_df.shape} (Users x Products).")
        return basket_df
