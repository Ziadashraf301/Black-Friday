"""CORE-14: a missing stay_in_current_city_years must map to a category the model was trained on, never "None"."""

import pytest

from core.db.repositories.user_repo import DEFAULT_STAY_IN_CITY_YEARS, UserRepository

ROW = {
    "gender": "F",
    "age": "26-35",
    "city_category": "A",
    "stay_in_current_city_years": None,
    "marital_status": None,
    "occupation": 4,
}
TRAINED_CATEGORIES = {"0", "1", "2", "3", "4+"}


def test_default_is_a_trained_category():
    assert DEFAULT_STAY_IN_CITY_YEARS in TRAINED_CATEGORIES


@pytest.mark.parametrize("missing", [None, ""])
def test_missing_value_uses_default(missing):
    demo = UserRepository._demographics_from_row({**ROW, "stay_in_current_city_years": missing})
    assert demo["stay_in_current_city_years"] == DEFAULT_STAY_IN_CITY_YEARS
    assert demo["marital_status"] == 0


def test_present_value_is_kept_as_string():
    assert UserRepository._demographics_from_row({**ROW, "stay_in_current_city_years": 4})["stay_in_current_city_years"] == "4"
    assert (
        UserRepository._demographics_from_row({**ROW, "stay_in_current_city_years": "4+"})["stay_in_current_city_years"]
        == "4+"
    )
