import pandas as pd
import pytest
from ml.segmentation.clustering import CustomerSegmentationEngine


def test_compute_cluster_statistics():
    sample_df = pd.DataFrame({
        "user_id": [10001, 10002, 10003, 10004],
        "lifetime_value": [5000.0, 7000.0, 15000.0, 25000.0],
        "average_order_value": [500.0, 700.0, 1500.0, 2500.0],
        "frequency": [10, 10, 10, 10],
        "gender": ["F", "F", "M", "M"],
        "marital_status": ["Single", "Single", "Married", "Married"],
        "age_binned": ["<=50", "<=50", "<=50", "<=50"],
        "popular_category": [1, 1, 5, 5],
        "cluster_id": [1, 1, 3, 3],
        "cluster_persona": ["Single females <= 50", "Single females <= 50", "Married males <= 50", "Married males <= 50"],
        "recommended_action": ["Target lifestyle ads", "Target lifestyle ads", "Target tech deals", "Target tech deals"]
    })

    stats_df = CustomerSegmentationEngine.compute_cluster_statistics(sample_df)

    assert not stats_df.empty
    assert len(stats_df) == 2
    assert "mean_lifetime_value" in stats_df.columns
    assert "mean_average_order_value" in stats_df.columns
    assert "mode_gender" in stats_df.columns
    assert "mode_marital_status" in stats_df.columns

    cluster1 = stats_df[stats_df["cluster_id"] == 1].iloc[0]
    assert cluster1["mean_lifetime_value"] == 6000.0
    assert cluster1["mode_gender"] == "F"
    assert cluster1["mode_marital_status"] == "Single"

    cluster3 = stats_df[stats_df["cluster_id"] == 3].iloc[0]
    assert cluster3["mean_lifetime_value"] == 20000.0
    assert cluster3["mode_gender"] == "M"
    assert cluster3["mode_marital_status"] == "Married"
