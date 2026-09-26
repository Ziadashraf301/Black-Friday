import numpy as np
import pandas as pd
from typing import Dict, Any, List
import gower
from scipy.cluster.hierarchy import linkage, fcluster
from scipy.spatial.distance import squareform
from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)

# Definitive 10 Customer Personas mapped from R analysis findings
PERSONA_PROFILES: Dict[int, Dict[str, str]] = {
    1: {
        "persona": "Single females <= 50",
        "action": "Target lifestyle-oriented ads. Focus on health, wellness, and personal care deals."
    },
    2: {
        "persona": "Single males <= 50 (Low-to-moderate spenders)",
        "action": "Offer introductory discount codes, gadgets, and gaming deals to stimulate higher spend."
    },
    3: {
        "persona": "Married males <= 50 (Moderate spenders)",
        "action": "Send notifications of popular family items and tech deals with cross-category coupons."
    },
    4: {
        "persona": "Single females > 50",
        "action": "Focus on high-quality lifestyle, travel, and premium personal goods."
    },
    5: {
        "persona": "Married females <= 50",
        "action": "Target family-oriented deals, home appliances, children's products, and kitchenware."
    },
    6: {
        "persona": "Single older males > 50",
        "action": "Focus on hobby goods, sports equipment, DIY tools, and outdoor travel deals."
    },
    7: {
        "persona": "Married older males > 50",
        "action": "Market home improvement, premium electronics, retirement lifestyle, and warranty perks."
    },
    8: {
        "persona": "Married females > 50",
        "action": "Family home upgrades, holiday gift bundles, and high-trust loyalty incentives."
    },
    9: {
        "persona": "Single males <= 50 (High-spending VIP)",
        "action": "VIP loyalty tier, exclusive midnight early-access, high-end electronics, and personal recommendations."
    },
    10: {
        "persona": "Married males <= 50 (Ultra High-Value Whales)",
        "action": "Dedicated account perks, luxury bundle discounts, premium electronics, and priority delivery."
    }
}


class CustomerSegmentationEngine:
    """Computes Gower distance matrix and hierarchical clustering for customer personas."""

    def __init__(self, k_clusters: int = settings.K_CLUSTERS):
        self.k_clusters = k_clusters
        self.feature_cols = [
            "lifetime_value", "frequency", "gender", "marital_status", "age_binned"
        ]

    @staticmethod
    def resolve_cluster_persona(
        gender: str, marital_status: str, age_binned: str, mean_ltv: float, mean_freq: float
    ) -> tuple[int, str, str]:
        """Dynamically matches a cluster's empirical demographic and spending profile to its true R persona."""
        # Check for ultra-high spending VIP clusters (clusters 9 and 10 in R)
        if mean_ltv > 2_500_000 or mean_freq > 300:
            if marital_status == "Married":
                return (
                    10,
                    PERSONA_PROFILES[10]["persona"],
                    PERSONA_PROFILES[10]["action"]
                )
            else:
                return (
                    9,
                    PERSONA_PROFILES[9]["persona"],
                    PERSONA_PROFILES[9]["action"]
                )

        # Standard demographic profiles (clusters 1-8 in R)
        if gender == "F":
            if marital_status == "Single" and age_binned == "<=50":
                return 1, PERSONA_PROFILES[1]["persona"], PERSONA_PROFILES[1]["action"]
            elif marital_status == "Single" and age_binned == ">51":
                return 4, PERSONA_PROFILES[4]["persona"], PERSONA_PROFILES[4]["action"]
            elif marital_status == "Married" and age_binned == "<=50":
                return 5, PERSONA_PROFILES[5]["persona"], PERSONA_PROFILES[5]["action"]
            else:
                return 8, PERSONA_PROFILES[8]["persona"], PERSONA_PROFILES[8]["action"]
        else:  # Male
            if marital_status == "Single" and age_binned == ">51":
                return 6, PERSONA_PROFILES[6]["persona"], PERSONA_PROFILES[6]["action"]
            elif marital_status == "Married" and age_binned == ">51":
                return 7, PERSONA_PROFILES[7]["persona"], PERSONA_PROFILES[7]["action"]
            elif marital_status == "Single":
                return 2, PERSONA_PROFILES[2]["persona"], PERSONA_PROFILES[2]["action"]
            else:
                return 3, PERSONA_PROFILES[3]["persona"], PERSONA_PROFILES[3]["action"]

    def fit_predict(self, customer_df: pd.DataFrame) -> pd.DataFrame:
        """Fits complete-linkage hierarchical clustering over Gower distance matrix with dynamic persona resolution."""
        logger.info(f"Computing Gower dissimilarity matrix for {len(customer_df):,} customers across features: {self.feature_cols}...")
        
        feature_df = customer_df[self.feature_cols].copy()
        # Ensure categorical/string columns have standard numpy object dtype for gower compatibility
        object_cast = {c: object for c in feature_df.columns if not pd.api.types.is_numeric_dtype(feature_df[c])}
        if object_cast:
            feature_df = feature_df.astype(object_cast)

        # Compute Gower distance matrix
        gower_matrix = gower.gower_matrix(feature_df)

        logger.info("Performing hierarchical clustering with complete linkage...")
        condensed_dist = squareform(gower_matrix, checks=False)
        linkage_matrix = linkage(condensed_dist, method="complete")

        logger.info(f"Cutting dendrogram into k={self.k_clusters} clusters...")
        raw_cluster_assignments = fcluster(linkage_matrix, t=self.k_clusters, criterion="maxclust")

        result_df = customer_df.copy()
        result_df["raw_cluster_id"] = raw_cluster_assignments

        # Profile each cluster dynamically by its dominant attributes
        cluster_mapping = {}
        for raw_cid in np.unique(raw_cluster_assignments):
            subset = result_df[result_df["raw_cluster_id"] == raw_cid]
            dom_gender = subset["gender"].mode()[0] if not subset["gender"].empty else "M"
            dom_marital = subset["marital_status"].mode()[0] if not subset["marital_status"].empty else "Single"
            dom_age = subset["age_binned"].mode()[0] if not subset["age_binned"].empty else "<=50"
            mean_ltv = float(subset["lifetime_value"].mean())
            mean_freq = float(subset["frequency"].mean())

            r_cid, persona_name, action = self.resolve_cluster_persona(
                dom_gender, dom_marital, dom_age, mean_ltv, mean_freq
            )
            cluster_mapping[raw_cid] = {
                "cluster_id": r_cid,
                "cluster_persona": persona_name,
                "recommended_action": action
            }

        result_df["cluster_id"] = result_df["raw_cluster_id"].map(lambda cid: cluster_mapping[cid]["cluster_id"])
        result_df["cluster_persona"] = result_df["raw_cluster_id"].map(lambda cid: cluster_mapping[cid]["cluster_persona"])
        result_df["recommended_action"] = result_df["raw_cluster_id"].map(lambda cid: cluster_mapping[cid]["recommended_action"])
        result_df.drop(columns=["raw_cluster_id"], inplace=True)

        logger.info("Customer segmentation and dynamic persona resolution successfully complete.")
        return result_df

    @staticmethod
    def compute_cluster_statistics(segmented_df: pd.DataFrame) -> pd.DataFrame:
        """Computes cluster-level summary table with mean for numeric features and mode for categorical features."""
        ignore_cols = {"user_id", "cluster_id", "raw_cluster_id", "recommended_action"}

        numeric_cols = [c for c in segmented_df.columns if pd.api.types.is_numeric_dtype(segmented_df[c]) and c not in ignore_cols]
        categorical_cols = [c for c in segmented_df.columns if not pd.api.types.is_numeric_dtype(segmented_df[c]) and c not in ignore_cols]

        group_col = "cluster_id" if "cluster_id" in segmented_df.columns else "cluster_persona"
        stats_list = []

        for group_val, subset in segmented_df.groupby(group_col, sort=True):
            cid = int(group_val) if isinstance(group_val, (int, np.integer)) else group_val
            row = {"cluster_id": cid}
            if "cluster_persona" in subset.columns:
                row["cluster_persona"] = subset["cluster_persona"].iloc[0]

            row["customer_count"] = int(len(subset))
            row["customer_share_pct"] = round(float(len(subset) / max(len(segmented_df), 1) * 100), 2)

            # Means for numeric features
            for num_col in numeric_cols:
                row[f"mean_{num_col}"] = round(float(subset[num_col].mean()), 2)

            # Modes for categorical features
            for cat_col in categorical_cols:
                if cat_col == "cluster_persona":
                    continue
                mode_vals = subset[cat_col].mode()
                row[f"mode_{cat_col}"] = str(mode_vals.iloc[0]) if not mode_vals.empty else "N/A"

            stats_list.append(row)

        stats_df = pd.DataFrame(stats_list)
        return stats_df
