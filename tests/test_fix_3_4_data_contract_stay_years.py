"""
Regression test for Fix 3.4:
- Add stay_in_current_city_years validation ("0", "1", "2", "3", "4+")
- First verify valid values accepted by RawTransactionSchema and CleanedTransactionSchema
- Verify invalid values raise pandera SchemaError
"""
import pytest
import pandas as pd
from pandera.errors import SchemaError
from ml.features.data_contract import (
    RawTransactionSchema,
    CleanedTransactionSchema,
    validate_raw_data,
    validate_cleaned_data,
    VALID_STAY_YEARS,
)


@pytest.fixture
def base_raw_row():
    return {
        "user_id": 1000001,
        "product_id": "P0001",
        "gender": "M",
        "age": "26-35",
        "occupation": 10,
        "city_category": "A",
        "stay_in_current_city_years": "2",
        "marital_status": 0,
        "product_category_1": 3,
        "product_category_2": 4.0,
        "product_category_3": 12.0,
        "purchase": 8500.0,
    }


@pytest.fixture
def base_cleaned_row():
    return {
        "user_id": 1000001,
        "product_id": "P0001",
        "gender": "M",
        "age": "26-35",
        "occupation": 10,
        "city_category": "A",
        "stay_in_current_city_years": "2",
        "marital_status": 0,
        "product_category_1": 3,
        "product_category_2": 4,
        "product_category_3": 12,
        "purchase": 8500.0,
        "is_outlier": False,
        "normalized_purchase": 0.40,
        "split": "train",
    }


@pytest.mark.parametrize("stay_val", ["0", "1", "2", "3", "4+"])
def test_valid_stay_years_accepted_in_raw_contract(base_raw_row, stay_val):
    row = dict(base_raw_row)
    row["stay_in_current_city_years"] = stay_val
    df = pd.DataFrame([row])
    validated = validate_raw_data(df)
    assert validated["stay_in_current_city_years"].iloc[0] == stay_val


@pytest.mark.parametrize("stay_val", ["0", "1", "2", "3", "4+"])
def test_valid_stay_years_accepted_in_cleaned_contract(base_cleaned_row, stay_val):
    row = dict(base_cleaned_row)
    row["stay_in_current_city_years"] = stay_val
    df = pd.DataFrame([row])
    validated = validate_cleaned_data(df)
    assert validated["stay_in_current_city_years"].iloc[0] == stay_val


@pytest.mark.parametrize("invalid_val", ["5", "4", "10", "unknown", "-1", "years"])
def test_invalid_stay_years_rejected_in_raw_contract(base_raw_row, invalid_val):
    row = dict(base_raw_row)
    row["stay_in_current_city_years"] = invalid_val
    df = pd.DataFrame([row])
    with pytest.raises(SchemaError):
        validate_raw_data(df)


@pytest.mark.parametrize("invalid_val", ["5", "4", "10", "unknown", "-1", "years"])
def test_invalid_stay_years_rejected_in_cleaned_contract(base_cleaned_row, invalid_val):
    row = dict(base_cleaned_row)
    row["stay_in_current_city_years"] = invalid_val
    df = pd.DataFrame([row])
    with pytest.raises(SchemaError):
        validate_cleaned_data(df)
