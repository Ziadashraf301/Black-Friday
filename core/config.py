from typing import Optional, Any
from pathlib import Path
import urllib.parse
from pydantic import Field, field_validator
from pydantic_settings import BaseSettings, SettingsConfigDict


class Settings(BaseSettings):
    """Central configuration for Black Friday v2 application."""

    model_config = SettingsConfigDict(
        env_file=".env",
        env_file_encoding="utf-8",
        extra="ignore"
    )

    # Project metadata
    PROJECT_NAME: str = "Black-Friday-v2"
    ENVIRONMENT: str = "development"
    LOG_LEVEL: str = "INFO"
    BASE_DIR: Path = Path(__file__).resolve().parent.parent
    LOG_DIR: Path = Path(__file__).resolve().parent.parent / "logs"
    LOG_FILE: str = "app.log"

    # Data Storage & Raw Datasets
    DATA_DIR: Path = Path(__file__).resolve().parent.parent / "data"
    TRAIN_DATA_PATH: str = Field(default="data/train.csv")
    TEST_DATA_PATH: str = Field(default="data/test.csv")

    @property
    def resolved_train_data_path(self) -> Path:
        p = Path(self.TRAIN_DATA_PATH)
        return p if p.is_absolute() else self.BASE_DIR / p

    @property
    def resolved_test_data_path(self) -> Path:
        p = Path(self.TEST_DATA_PATH)
        return p if p.is_absolute() else self.BASE_DIR / p

    # PostgreSQL Analytical Warehouse
    POSTGRES_USER: str = Field(default="postgres")
    POSTGRES_PASSWORD: str = Field(default="postgres_password123")
    POSTGRES_HOST: str = Field(default="localhost")
    POSTGRES_PORT: int = Field(default=5432)
    APP_DB_NAME: str = Field(default="fridayblack")
    MLFLOW_DB_NAME: str = Field(default="mlflow")

    @property
    def database_url(self) -> str:
        user = urllib.parse.quote_plus(self.POSTGRES_USER)
        pwd = urllib.parse.quote_plus(self.POSTGRES_PASSWORD)
        return f"postgresql+psycopg2://{user}:{pwd}@{self.POSTGRES_HOST}:{self.POSTGRES_PORT}/{self.APP_DB_NAME}"

    @property
    def async_database_url(self) -> str:
        user = urllib.parse.quote_plus(self.POSTGRES_USER)
        pwd = urllib.parse.quote_plus(self.POSTGRES_PASSWORD)
        return f"postgresql+asyncpg://{user}:{pwd}@{self.POSTGRES_HOST}:{self.POSTGRES_PORT}/{self.APP_DB_NAME}"

    # MinIO / S3 Storage
    MINIO_HOST: str = Field(default="localhost")
    MINIO_ROOT_USER: str = Field(default="admin")
    MINIO_ROOT_PASSWORD: str = Field(default="password123")
    MINIO_PORT: int = Field(default=9000)
    MINIO_BUCKET_MLFLOW: str = Field(default="mlflow-artifacts")
    S3_ENDPOINT_URL: Optional[str] = Field(default=None)
    MLFLOW_S3_ENDPOINT_URL: Optional[str] = Field(default=None)

    @property
    def s3_endpoint_url(self) -> str:
        if self.MLFLOW_S3_ENDPOINT_URL:
            return self.MLFLOW_S3_ENDPOINT_URL
        if self.S3_ENDPOINT_URL:
            return self.S3_ENDPOINT_URL
        return f"http://{self.MINIO_HOST}:{self.MINIO_PORT}"

    # Redis Cache Configuration (6-Hour Persistent TTL = 21,600 Seconds)
    REDIS_HOST: str = Field(default="localhost")
    REDIS_PORT: int = Field(default=6379)
    REDIS_PASSWORD: Optional[str] = Field(default=None)
    REDIS_DB: int = Field(default=0)
    REDIS_DEFAULT_TTL: int = Field(default=21600)  # 6 hours in seconds

    @property
    def redis_url(self) -> str:
        if self.REDIS_PASSWORD:
            pwd = urllib.parse.quote_plus(self.REDIS_PASSWORD)
            return f"redis://:{pwd}@{self.REDIS_HOST}:{self.REDIS_PORT}/{self.REDIS_DB}"
        return f"redis://{self.REDIS_HOST}:{self.REDIS_PORT}/{self.REDIS_DB}"

    # MLflow Tracking
    MLFLOW_TRACKING_URI: str = Field(default="http://localhost:5000")
    MLFLOW_EXPERIMENT_NAME: str = Field(default="black-friday-sales-prediction")
    MLFLOW_MODEL_NAME: str = Field(default="blackfriday-pricing-regressor")
    MLFLOW_IMPUTER_MODEL_NAME: str = Field(default="blackfriday-missforest-imputer")

    # ML Constants matching R analysis
    PURCHASE_MAX: float = 21399.0
    OUTLIER_THRESHOLD: float = 21400.5
    INR_TO_USD: float = Field(default=80.0)
    RANDOM_SEED: int = Field(default=1234)

    # Customer Segmentation Hyperparameters
    K_CLUSTERS: int = Field(default=10)
    SEGMENTATION_METRIC: str = Field(default="gower")
    SEGMENTATION_LINKAGE: str = Field(default="complete")

    # Market Basket & Item2Vec Hyperparameters
    APRIORI_MIN_SUPPORT: float = Field(default=0.05)
    APRIORI_MIN_CONFIDENCE: float = Field(default=0.40)
    ITEM2VEC_VECTOR_SIZE: int = Field(default=32)
    ITEM2VEC_WINDOW: int = Field(default=5)
    ITEM2VEC_MIN_COUNT: int = Field(default=2)
    ITEM2VEC_EPOCHS: int = Field(default=10)

    # Imputation Configuration (Optional / Overridable via .env)
    IMPUTER_N_ESTIMATORS: int = Field(default=30)
    IMPUTER_MAX_ITER: int = Field(default=5)
    IMPUTER_SAMPLE_SIZE: Optional[int] = Field(default=5000000)
    IMPUTER_MAX_DEPTH: Optional[int] = Field(default=12)
    IMPUTER_MIN_SAMPLES_LEAF: int = Field(default=5)
    IMPUTER_N_JOBS: int = Field(default=-1)

    @field_validator(
        "IMPUTER_SAMPLE_SIZE",
        "IMPUTER_MAX_DEPTH",
        "DT_MAX_DEPTH",
        "RF_MAX_DEPTH",
        mode="before"
    )
    @classmethod
    def parse_optional_integer(cls, v):
        if v == "" or v is None or str(v).lower() in ("none", "null"):
            return None
        return int(v)

    # Model-Agnostic Engine Configuration
    DEFAULT_REGRESSION_MODEL: str = Field(default="random_forest")
    CHAMPION_MIN_R2_THRESHOLD: float = 0.7400
    CHAMPION_MIN_IMPROVEMENT_DELTA: float = 0.0050

    # Decision Tree Hyperparameters
    DT_MAX_DEPTH: Optional[int] = Field(default=18)
    DT_MIN_SAMPLES_LEAF: int = Field(default=5)

    # Random Forest Hyperparameters
    RF_N_ESTIMATORS: int = Field(default=100)
    RF_MAX_FEATURES: Optional[Any] = Field(default=1.0)
    RF_MAX_DEPTH: Optional[int] = Field(default=16)
    RF_MIN_SAMPLES_LEAF: int = Field(default=5)
    RF_N_JOBS: int = Field(default=-1)

    # LightGBM Hyperparameters
    LGBM_N_ESTIMATORS: int = Field(default=350)
    LGBM_LEARNING_RATE: float = Field(default=0.05)
    LGBM_NUM_LEAVES: int = Field(default=127)
    LGBM_MAX_DEPTH: int = Field(default=-1)
    LGBM_SUBSAMPLE: float = Field(default=0.8)
    LGBM_COLSAMPLE_BYTREE: float = Field(default=0.8)
    LGBM_N_JOBS: int = Field(default=-1)

    # Drift-Triggered Retraining Thresholds
    # DRIFT_DATASET_THRESHOLD: fraction of features that must drift before retraining is triggered (0–1)
    DRIFT_DATASET_THRESHOLD: float = 0.30   # retrain if >30% of features drift
    # DRIFT_TARGET_PSI_THRESHOLD: population stability index threshold for target drift
    DRIFT_TARGET_PSI_THRESHOLD: float = 0.20  # PSI > 0.2 = significant target distribution shift

    # API & Serving
    API_PORT: int = 8000
    API_HOST: str = "0.0.0.0"
    API_BASE_URL: str = Field(default="http://localhost:8000")
    REFLEX_PORT: int = 3000
    CORS_ORIGINS: Optional[str] = Field(default=None)

    # Rate Limiting
    RATE_LIMIT_MINUTE: int = Field(default=60)
    RATE_LIMIT_DAY: int = Field(default=1000)

    # JWT Authentication
    SECRET_KEY: str = Field(default="blackfriday-super-secret-key-change-in-production-2024")
    JWT_ALGORITHM: str = Field(default="HS256")
    JWT_EXPIRY_HOURS: int = Field(default=24)

    GEMINI_API_KEY: Optional[str] = Field(default=None)
    TYPESAFE_API_KEY: Optional[str] = Field(default=None)
    LLM_MODEL: str = Field(default="gemini-2.0-flash")


settings = Settings()
