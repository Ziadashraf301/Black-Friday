import pandas as pd
from typing import Dict, Any
from core.logging import get_logger

logger = get_logger(__name__)

class CustomerFeatureExtractor:
    """Extracts customer-level behavioral and demographic metrics for clustering."""

    def extract_features(self, df: pd.DataFrame) -> pd.DataFrame:
        """Aggregates transactional purchase history into 1 row per unique User_ID."""
        logger.info(f"Extracting customer features from {len(df):,} transactions...")

        # 1. Behavioral KPIs per customer
        agg_funcs = {
            "purchase": ["sum", "mean", lambda x: x.max() - x.min()],
            "product_id": "nunique",
            "product_category_1": lambda x: x.mode()[0] if not x.mode().empty else x.iloc[0]
        }

        user_behavior = df.groupby("user_id").agg(agg_funcs)
        user_behavior.columns = [
            "lifetime_value",
            "average_order_value",
            "purchase_amount_variability",
            "frequency",
            "popular_category"
        ]
        user_behavior = user_behavior.reset_index()

        # 2. Customer Demographics (take first matching row since demographics are invariant per user)
        demographics = df.drop_duplicates(subset=["user_id"])[[
            "user_id", "gender", "marital_status", "age"
        ]].copy()

        # Marital status text normalization
        demographics["marital_status"] = demographics["marital_status"].apply(
            lambda x: "Married" if str(x) in ["1", "Married"] else "Single"
        )

        # Age binned into <=50 vs >51 matching R analysis findings
        demographics["age_binned"] = demographics["age"].apply(
            lambda x: ">51" if x in ["51-55", "55+"] else "<=50"
        )
        demographics["age_group"] = demographics["age"]
        demographics.drop(columns=["age"], inplace=True)

        # 3. Merge behavioral metrics with demographics
        customer_df = pd.merge(user_behavior, demographics, on="user_id", how="inner")

        # Round numerical features
        customer_df["lifetime_value"] = customer_df["lifetime_value"].round(2)
        customer_df["average_order_value"] = customer_df["average_order_value"].round(2)
        customer_df["purchase_amount_variability"] = customer_df["purchase_amount_variability"].round(2)

        logger.info(f"Generated feature profiles for {len(customer_df):,} distinct customers.")
        return customer_df
