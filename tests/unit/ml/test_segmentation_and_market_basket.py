"""Step 6 review: segmentation and market basket fixes (ANL-*)."""

import numpy as np
import pandas as pd
import pytest

from core.config import settings
from ml.market_basket.apriori_engine import AprioriAssociationEngine
from ml.market_basket.item2vec import Item2VecRecommender
from ml.segmentation.clustering import CustomerSegmentationEngine

resolve = CustomerSegmentationEngine.resolve_cluster_persona


def test_anl3_vip_thresholds_come_from_settings(monkeypatch):
    assert resolve("M", "Single", "<=50", 1_000_000, 100)[0] == 2
    monkeypatch.setattr(settings, "SEGMENT_VIP_MIN_LTV", 500_000.0)
    assert resolve("M", "Single", "<=50", 1_000_000, 100)[0] == 9
    monkeypatch.setattr(settings, "SEGMENT_VIP_MIN_LTV", 2_500_000.0)
    monkeypatch.setattr(settings, "SEGMENT_VIP_MIN_FREQUENCY", 50.0)
    assert resolve("M", "Married", "<=50", 1_000, 100)[0] == 10


def test_anl1_vip_personas_are_male_only():
    assert resolve("F", "Married", "<=50", 9_000_000, 900)[0] == 5
    assert resolve("F", "Single", ">51", 9_000_000, 900)[0] == 4
    assert resolve("M", "Married", "<=50", 9_000_000, 900)[0] == 10
    assert resolve("M", "Single", "<=50", 9_000_000, 900)[0] == 9


def _customers(n_per_group=6):
    rows = []
    uid = 1
    for gender, marital, age, ltv in [("F", "Single", "<=50", 1000.0), ("M", "Married", ">51", 9000.0)]:
        for i in range(n_per_group):
            rows.append(
                {
                    "user_id": uid,
                    "lifetime_value": ltv + i,
                    "frequency": 10 + i,
                    "gender": gender,
                    "marital_status": marital,
                    "age_binned": age,
                }
            )
            uid += 1
    return pd.DataFrame(rows)


def test_anl2_warns_when_raw_clusters_collapse_into_one_persona(caplog):
    caplog.set_level("WARNING")
    engine = CustomerSegmentationEngine(k_clusters=4)  # 2 demographic groups -> must split one persona in two
    out = engine.fit_predict(_customers())
    assert out["cluster_id"].nunique() == 2
    assert any("same persona" in r.getMessage() for r in caplog.records)


def test_anl4_k_clusters_default_is_read_at_construction(monkeypatch):
    monkeypatch.setattr(settings, "K_CLUSTERS", 7)
    assert CustomerSegmentationEngine().k_clusters == 7
    assert CustomerSegmentationEngine(k_clusters=3).k_clusters == 3


def test_anl4_clustering_needs_two_customers():
    with pytest.raises(ValueError, match="at least 2"):
        CustomerSegmentationEngine().fit_predict(_customers().head(1))


def test_anl11_linkage_method_comes_from_settings(monkeypatch):
    import ml.segmentation.clustering as clustering

    used = []
    real = clustering.linkage
    monkeypatch.setattr(clustering, "linkage", lambda d, method: used.append(method) or real(d, method=method))
    monkeypatch.setattr(settings, "SEGMENTATION_LINKAGE", "average")
    CustomerSegmentationEngine(k_clusters=2).fit_predict(_customers())
    assert used == ["average"]


def _basket_matrix(rows):
    return pd.DataFrame(rows, columns=["a", "b", "c"]).astype(bool)


def test_anl5_negative_association_is_not_labelled_1_2():
    # P(a)=P(b)=4/6, P(a and b)=2/6 -> lift 0.75
    m = _basket_matrix([[1, 0, 0], [1, 0, 0], [1, 1, 0], [1, 1, 0], [0, 1, 0], [0, 1, 0]])
    rules = AprioriAssociationEngine(min_support=0.1, min_confidence=0.1).mine_rules(m)
    levels = dict(
        zip(
            zip(rules["antecedents"].map(sorted).map(tuple), rules["consequents"].map(sorted).map(tuple)),
            zip(rules["lift"], rules["lift_level"]),
        )
    )
    lift, level = levels[(("a",), ("b",))]
    assert lift < 1 and level == "<1"


def test_anl6_rules_from_a_previous_run_are_cleared():
    engine = AprioriAssociationEngine(min_support=0.1, min_confidence=0.1)
    engine.mine_rules(_basket_matrix([[1, 1, 0], [1, 1, 0], [1, 1, 0]]))
    assert engine.get_top_rules()
    engine.min_support = 0.99
    assert engine.mine_rules(_basket_matrix([[1, 0, 0], [0, 1, 0], [0, 0, 1]])).empty
    assert engine.get_top_rules() == []


def _baskets():
    rng = np.random.RandomState(0)
    items = [f"P{i}" for i in range(20)]
    return [list(rng.choice(items, size=6, replace=False)) for _ in range(60)]


def test_anl7_item2vec_training_is_reproducible():
    a = Item2VecRecommender(min_count=1, epochs=3).fit(_baskets())
    b = Item2VecRecommender(min_count=1, epochs=3).fit(_baskets())
    assert np.array_equal(a.get_embedding("P1"), b.get_embedding("P1"))


def test_anl8_item2vec_defaults_follow_settings(monkeypatch):
    monkeypatch.setattr(settings, "ITEM2VEC_MIN_COUNT", 3)
    monkeypatch.setattr(settings, "ITEM2VEC_EPOCHS", 4)
    model = Item2VecRecommender()
    assert (model.min_count, model.epochs, model.vector_size, model.window) == (3, 4, 32, 5)
    assert Item2VecRecommender(min_count=1).min_count == 1


def test_anl8_similarity_pass_covers_every_product():
    model = Item2VecRecommender(min_count=1, epochs=2).fit(_baskets())
    sims = model.get_all_similar_products(top_n=3)
    assert set(sims) == set(model.vocabulary)
    assert all(len(v) == 3 for v in sims.values())


def test_anl10_hits_non_convergence_falls_back_to_uniform_scores(monkeypatch):
    import networkx as nx

    from ml.market_basket.network_graph import ProductNetworkGraph

    def boom(*args, **kwargs):
        raise nx.PowerIterationFailedConvergence(200)

    monkeypatch.setattr(nx, "hits", boom)
    graph = ProductNetworkGraph()
    metrics = graph.build_graph_from_similarity_map({"a": [("b", 0.9)], "b": [("a", 0.8)]})
    assert set(metrics["hub_score"]) == {0.5} and set(metrics["authority_score"]) == {0.5}


def test_compute_cluster_statistics():
    sample_df = pd.DataFrame(
        {
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
            "recommended_action": ["Target lifestyle ads", "Target lifestyle ads", "Target tech deals", "Target tech deals"],
        }
    )

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
