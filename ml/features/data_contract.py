import pandas as pd
from core.logging import get_logger
import os
os.environ["DISABLE_PANDERA_IMPORT_WARNING"] = "True"

from pandera.pandas import Column, Check, DataFrameSchema

logger = get_logger(__name__)

VALID_STAY_YEARS = ["0", "1", "2", "3", "4+"]

RawTransactionSchema = DataFrameSchema(
    {
        "user_id": Column(int, Check.greater_than(0), nullable=False),
        "product_id": Column(str, Check.str_startswith("P"), nullable=False),
        "gender": Column(str, Check.isin(["M", "F"]), nullable=False),
        "age": Column(str, Check.isin(["0-17", "18-25", "26-35", "36-45", "46-50", "51-55", "55+"]), nullable=False),
        "occupation": Column(int, Check.in_range(0, 20), nullable=False),
        "city_category": Column(str, Check.isin(["A", "B", "C"]), nullable=False),
        "stay_in_current_city_years": Column(str, Check.isin(VALID_STAY_YEARS), nullable=False),
        "marital_status": Column(int, Check.isin([0, 1]), nullable=False),
        "product_category_1": Column(int, Check.in_range(1, 20), nullable=False),
        "product_category_2": Column(float, Check.in_range(1, 20), nullable=True),
        "product_category_3": Column(float, Check.in_range(1, 20), nullable=True),
        "purchase": Column(float, Check.greater_than(0), nullable=False),
    },
    coerce=True,
    strict=False
)

CleanedTransactionSchema = DataFrameSchema(
    {
        "user_id": Column(int, Check.greater_than(0), nullable=False),
        "product_id": Column(str, nullable=False),
        "gender": Column(str, Check.isin(["M", "F"]), nullable=False),
        "age": Column(str, nullable=False),
        "occupation": Column(int, nullable=False),
        "city_category": Column(str, Check.isin(["A", "B", "C"]), nullable=False),
        "stay_in_current_city_years": Column(str, Check.isin(VALID_STAY_YEARS), nullable=False),
        "marital_status": Column(int, Check.isin([0, 1]), nullable=False),
        "product_category_1": Column(int, Check.in_range(1, 20), nullable=False),
        "product_category_2": Column(int, Check.in_range(1, 20), nullable=False),
        "product_category_3": Column(int, Check.in_range(1, 20), nullable=False),
        "purchase": Column(float, Check.greater_than(0), nullable=False),
        "is_outlier": Column(bool, nullable=False),
        "normalized_purchase": Column(float, Check.in_range(0.0, 1.5), nullable=False),
        "split": Column(str, Check.isin(["train", "test"]), nullable=False)
    },
    coerce=True,
    strict=False
)

def validate_raw_data(df: pd.DataFrame) -> pd.DataFrame:
    """Validates raw DataFrame against Pandera Data Contract."""
    logger.info(f"Validating {len(df):,} raw records against Pandera schema contract...")
    validated_df = RawTransactionSchema.validate(df)
    logger.info("Raw Data Contract validation passed successfully.")
    return validated_df

def validate_cleaned_data(df: pd.DataFrame) -> pd.DataFrame:
    """Validates cleaned DataFrame against Pandera Data Contract."""
    logger.info(f"Validating {len(df):,} cleaned records against Pandera schema contract...")
    validated_df = CleanedTransactionSchema.validate(df)
    logger.info("Cleaned Data Contract validation passed successfully.")
    return validated_df
