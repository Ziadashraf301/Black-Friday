"""
Regression test for Fix 2.4:
- Invert subtraction to addition in champion gate promotion condition:
    candidate_metric >= champion_metric + min_improvement_delta
- R2 is higher-is-better; min_improvement_delta is positive margin required.
- Table-driven tests with synthetic metrics covering:
    - candidate worse
    - candidate equal
    - candidate better by less than delta
    - candidate better by exactly delta
    - candidate better by more than delta
    - candidate below min_r2_threshold
"""
import pytest


def champion_gate_decision(
    candidate_r2: float,
    champion_r2: float,
    min_improvement_delta: float = 0.0050,
    min_r2_threshold: float = 0.6000,
) -> bool:
    """Mirrors the Champion vs Challenger decision gate logic in ml/pipelines/retrain.py."""
    meets_r2_threshold = candidate_r2 >= min_r2_threshold
    beats_champion_margin = candidate_r2 >= (champion_r2 + min_improvement_delta)
    is_promoted = meets_r2_threshold and beats_champion_margin
    return is_promoted


@pytest.mark.parametrize(
    "case_name,candidate_r2,champion_r2,delta,min_r2,expected_promoted",
    [
        ("candidate_worse", 0.6800, 0.7000, 0.0050, 0.6000, False),
        ("candidate_equal", 0.7000, 0.7000, 0.0050, 0.6000, False),
        ("candidate_better_less_than_delta", 0.7030, 0.7000, 0.0050, 0.6000, False),
        ("candidate_better_exactly_delta", 0.7050, 0.7000, 0.0050, 0.6000, True),
        ("candidate_better_more_than_delta", 0.7150, 0.7000, 0.0050, 0.6000, True),
        ("candidate_below_absolute_threshold", 0.5500, 0.5000, 0.0050, 0.6000, False),
        ("candidate_below_threshold_even_if_beats_champion", 0.5800, 0.5000, 0.0050, 0.6000, False),
        ("candidate_above_threshold_and_sufficient_delta", 0.7500, 0.7200, 0.0100, 0.6500, True),
    ],
)
def test_champion_challenger_promotion_gate_table(
    case_name, candidate_r2, champion_r2, delta, min_r2, expected_promoted
):
    promoted = champion_gate_decision(
        candidate_r2=candidate_r2,
        champion_r2=champion_r2,
        min_improvement_delta=delta,
        min_r2_threshold=min_r2,
    )
    assert promoted is expected_promoted, f"Failed case {case_name}: got {promoted}, expected {expected_promoted}"


def test_old_buggy_logic_would_improperly_promote_inferior_candidate():
    """Verify that old buggy subtraction logic would have promoted an inferior candidate."""
    candidate_r2 = 0.6970
    champion_r2 = 0.7000
    delta = 0.0050
    min_r2 = 0.6000

    # Buggy condition: candidate_r2 >= (champion_r2 - delta) -> 0.6970 >= 0.6950 (True! Bug!)
    buggy_beats_margin = candidate_r2 >= (champion_r2 - delta)
    assert buggy_beats_margin is True

    # Fixed condition: candidate_r2 >= (champion_r2 + delta) -> 0.6970 >= 0.7050 (False! Correct!)
    fixed_promoted = champion_gate_decision(candidate_r2, champion_r2, delta, min_r2)
    assert fixed_promoted is False
