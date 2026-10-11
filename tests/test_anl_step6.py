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
