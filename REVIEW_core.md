# Code Review: Session 1 — Core Foundation & Configuration (`core/`)

> **Review Scope**: `core/config.py`, `core/logging.py`, `core/security.py`, `core/cache/redis_client.py`, `core/db/session.py`, `core/db/models/*.py`, `core/db/repositories/*.py`, `core/db/repository.py`, `core/db/migrations/init_semantic_cache.py`.  
> **Repository Root**: `Black Friday/`  
> **Review Date**: 2026-10-06  
> **Output Target**: `REVIEW_core.md`  

---

## 1. Executive Summary

A comprehensive line-by-line audit of all 16 source files in the `core/` package was conducted against the preliminary findings documented in `REVIEW_HANDOFF.md`. 

Key takeaways:
1. **Critical Hidden Bug Discovered**: Python's Method Resolution Order (MRO) in the multiple-inheritance `BlackFridayRepository` causes `UserRepository.create_app_tables()` to override `BaseRepository.create_app_tables()`. Consequently, calling table initialization on startup (`main.py:26`) creates *only* 2 of the 8 required tables (`app_users` and `user_purchases`), silently skipping the raw data warehouse, cleaned tables, segmentation, network metrics, and curated catalog.
2. **Handoff Hypotheses Ground-Truthed**: Out of 7 claims touching `core/` in the handoff, **5 were CONFIRMED** and **2 were REJECTED** (the claim that `AsyncSessionLocal` exists in `session.py` is false; the claim that demographic values are enforced by SQLAlchemy column constraints in `warehouse.py` is false).
3. **Severe Anti-Patterns in Data Access**: `WarehouseRepository` suffers from extreme Single Responsibility Principle (SRP) bloat (594 LOC) combining ETL, vector search, catalog browsing, shopping cart persistence, and semantic caching. High-frequency OLTP paths (reading/saving cart items) execute redundant DDL `CREATE TABLE IF NOT EXISTS` operations on PostgreSQL.
4. **Injection and Performance Hazards**: An unparameterized f-string in `_execute_rrf_query()` exposes the SQL query to SQL injection via the `LIMIT {top_k}` clause. An unindexed `MODE() WITHIN GROUP` query scans 550,000 rows during price estimation. Redis cache pattern deletion uses blocking O(N) `KEYS` in production.

---

## 2. Evaluation of `REVIEW_HANDOFF.md` Hypotheses

### Finding H-1: God Object Anti-Pattern via Multiple Inheritance (SRP & ISP)
- **Location**: `core/db/repository.py:12-30`
- **Severity**: **High**
- **Status**: **CONFIRMED** (and amplified)
- **Problem**: 
  `BlackFridayRepository` uses multiple inheritance across 5 domain repositories:
  ```python
  class BlackFridayRepository(
      WarehouseRepository,
      AnalyticsRepository,
      SegmentationRepository,
      RecommendationRepository,
      UserRepository
  ):
  ```
  This violates SRP by amalgamating unrelated domain responsibilities into one monolithic facade. Furthermore, this causes a catastrophic method resolution collision: `UserRepository.create_app_tables` overrides `BaseRepository.create_app_tables`, preventing `Base.metadata.create_all` from ever executing on startup.
- **Concrete Fix**:
  Transition from multiple inheritance to composition. Provide a unified container or inject domain repositories independently:
  ```python
  # core/db/repository.py
  from core.db.repositories.warehouse_repo import WarehouseRepository
  from core.db.repositories.analytics_repo import AnalyticsRepository
  from core.db.repositories.segmentation_repo import SegmentationRepository
  from core.db.repositories.recommendation_repo import RecommendationRepository
  from core.db.repositories.user_repo import UserRepository
  from core.db.session import get_db_engine

  class BlackFridayRepository:
      """Composition-based repository aggregator."""
      def __init__(self, engine=None):
          self.engine = engine or get_db_engine()
          self.warehouse = WarehouseRepository(engine=self.engine)
          self.analytics = AnalyticsRepository(engine=self.engine)
          self.segmentation = SegmentationRepository(engine=self.engine)
          self.recommendation = RecommendationRepository(engine=self.engine)
          self.users = UserRepository(engine=self.engine)

      def create_all_tables(self):
          from core.db.models.base import Base
          Base.metadata.create_all(bind=self.engine)
  ```

---

### Finding H-2: Encapsulation Breach & Concrete Dependency Coupling
- **Location**: `core/cache/redis_client.py:27` (consumed in `apps/api/core/rate_limiter.py:44-45`)
- **Severity**: **Med**
- **Status**: **CONFIRMED**
- **Problem**: 
  `RedisCacheManager` defines `self._client: Optional[redis.Redis] = None` as a private member, yet callers needing low-level Redis operations (e.g., `RedisRateLimiter`, LangGraph checkpointer) bypass encapsulation and access `cache_manager._client` directly.
- **Concrete Fix**:
  Expose a thread-safe, public `client` property with proper availability guards:
  ```python
  # core/cache/redis_client.py
  @property
  def client(self) -> Optional[redis.Redis]:
      """Public accessor for low-level raw Redis commands (rate limiters, checkpointers)."""
      if not self.is_available:
          return None
      return self._client
  ```

---

### Finding H-3: Dual Logging Framework Fragmentation
- **Location**: `core/logging.py:12-48`
- **Severity**: **Med**
- **Status**: **CONFIRMED**
- **Problem**: 
  `core/logging.py` provides the official centralized logging factory (`get_logger(__name__)`), which configures standard library handlers and formatting. However, multiple modules (e.g. `ai/nodes/cart_node.py:7`, `ai/nodes/bundle_node.py:6`) bypass `core.logging` and import `loguru` directly, splitting logging sinks and adding an unmanaged dependency.
- **Concrete Fix**:
  Enforce usage of `core.logging.get_logger` across all modules:
  ```python
  # In ai/nodes/cart_node.py, bundle_node.py, support_node.py:
  # Replace:
  # from loguru import logger
  # With:
  from core.logging import get_logger
  logger = get_logger(__name__)
  ```

---

### Finding H-4: Schema Duplication Between SQL Models and Validation Contracts
- **Location**: `core/db/models/warehouse.py:15-70` vs `ml/features/data_contract.py:18-65`
- **Severity**: **Low**
- **Status**: **REJECTED**
- **Problem & Reality Check**: 
  The handoff claimed demographic constraints (Gender `M/F`, Age brackets, City `A/B/C`) are defined *both* as SQLAlchemy column constraints and as Pandera schema checks. In reality, `core/db/models/warehouse.py` defines NO `CheckConstraint`s or `Enum` types whatsoever (e.g., `gender = Column(String(1), nullable=False)`). The constraint validation exists *exclusively* in `ml/features/data_contract.py`. However, the underlying issue is that domain literals are unconstrained strings in SQLAlchemy, risking corrupt writes if unvalidated data bypasses Pandera.
- **Concrete Fix**:
  Create shared canonical Enums or Literal types in a domain constants module:
  ```python
  # core/constants.py
  import enum

  class Gender(str, enum.Enum):
      M = "M"
      F = "F"

  class CityCategory(str, enum.Enum):
      A = "A"
      B = "B"
      C = "C"

  # Use in core/db/models/warehouse.py via Enum(Gender) and Pandera checks
  ```

---

### Finding H-5: Async Session Availability Claim in Handoff
- **Location**: `core/db/session.py:10-43` vs `REVIEW_HANDOFF.md:405`
- **Severity**: **Low**
- **Status**: **REJECTED**
- **Problem & Reality Check**: 
  Section 4 of `REVIEW_HANDOFF.md` asserts that `core/db/session.py` provides: *"SQLAlchemy sync session (`SessionLocal`) and async session (`AsyncSessionLocal`)"*. An inspection of `core/db/session.py` proves this is completely false. There is NO `AsyncSessionLocal`, NO `create_async_engine`, and NO async session generator in `session.py`. Only synchronous `create_engine` and `SessionLocal` exist.
- **Concrete Fix**:
  Update documentation to reflect reality. If async operations are required for FastAPI routes, implement `create_async_engine` properly; otherwise, delete unused `async_database_url` in `core/config.py`:
  ```python
  # If async is needed:
  from sqlalchemy.ext.asyncio import create_async_engine, async_sessionmaker, AsyncSession
  async_engine = create_async_engine(settings.async_database_url, pool_pre_ping=True)
  AsyncSessionLocal = async_sessionmaker(async_engine, expire_on_commit=False, class_=AsyncSession)
  ```

---

### Finding H-6: Nested Package Namespace Confusion (`apps/api/core/`)
- **Location**: `core/` vs `apps/api/core/rate_limiter.py`
- **Severity**: **Low**
- **Status**: **CONFIRMED**
- **Problem**: 
  Having a top-level `core/` package and a nested `apps/api/core/` package creates developer confusion and makes imports like `from core.cache import ...` vs `from apps.api.core import ...` error-prone.
- **Concrete Fix**:
  Relocate `apps/api/core/rate_limiter.py` to `core/cache/rate_limiter.py` or `apps/api/security/rate_limiter.py`.

---

### Finding H-7: Absence of Alembic Migrations
- **Location**: `core/db/migrations/init_semantic_cache.py:1-35`
- **Severity**: **Med**
- **Status**: **CONFIRMED**
- **Problem**: 
  Schema creation is fragmented between `Base.metadata.create_all`, raw DDL in `init_semantic_cache.py`, inline `CREATE TABLE IF NOT EXISTS` in repository methods, and `docker/postgres/init_schema.sql`. There is no schema versioning or migration rollback mechanism.
- **Concrete Fix**:
  Initialize Alembic (`alembic init core/db/migrations/alembic`) and manage all schema alterations via versioned revisions.

---

## 3. New Findings Missed by the Handoff

### Finding N-1: Critical MRO Collision in `BlackFridayRepository` Silently Skips 6 Tables on Startup
- **Location**: `core/db/repositories/user_repo.py:15-50` and `core/db/repositories/base.py:32-36` and `core/db/repository.py:12-18`
- **Severity**: **High**
- **Status**: **NEW**
- **Problem**: 
  `BaseRepository.create_app_tables()` defines:
  ```python
  def create_app_tables(self):
      from core.db.models import Base
      Base.metadata.create_all(bind=self.engine)
  ```
  However, `UserRepository` defines a method with the identical name:
  ```python
  def create_app_tables(self):
      ddl = """CREATE TABLE IF NOT EXISTS app_users (...); CREATE TABLE IF NOT EXISTS user_purchases (...);"""
      with self.engine.begin() as conn:
          conn.execute(text(ddl))
  ```
  Because `BlackFridayRepository(WarehouseRepository, ..., UserRepository)` inherits from both, Python's C3 MRO puts `UserRepository` ahead of `BaseRepository`. When `apps/api/main.py:26` executes `BlackFridayRepository().create_app_tables()`, it invokes `UserRepository.create_app_tables()`. As a result, only `app_users` and `user_purchases` are created. The warehouse tables (`raw_black_friday`, `black_friday_cleaned`, `customer_segments`, `product_network_metrics`, `curated_products`) are never initialized on startup!
- **Concrete Fix**:
  Remove the duplicate `create_app_tables` method from `UserRepository` (or rename it to `create_user_tables()`) and ensure `BaseRepository.create_app_tables()` executes `Base.metadata.create_all()`:
  ```python
  # In core/db/repositories/user_repo.py
  # Delete or rename create_app_tables:
  def ensure_user_tables(self):
      """Specific DDL fallback if not using ORM metadata."""
      ...
  ```

---

### Finding N-2: Mandatory `GEMINI_API_KEY` Causes Unhandled Crashes on Import
- **Location**: `core/config.py:170`, `174`
- **Severity**: **High**
- **Status**: **NEW**
- **Problem**: 
  `GEMINI_API_KEY: str` has no default value. In `core/config.py:174`, `settings = Settings()` is instantiated at module top-level. If `GEMINI_API_KEY` is not present in the environment (e.g., in CI pipelines, offline unit test runs, or when running offline data pipelines like `ml.pipelines.ingest`), importing `core.config` (and therefore any of the 40+ dependent files) immediately crashes with:
  `pydantic_core.ValidationError: 1 validation error for Settings: GEMINI_API_KEY: Field required`.
- **Concrete Fix**:
  Provide an empty default string or make it `Optional[str] = Field(default=None)`:
  ```python
  # core/config.py:170
  GEMINI_API_KEY: Optional[str] = Field(default=None, description="Google Gemini API key")
  ```

---

### Finding N-3: SQL Injection Vulnerability via Unparameterized `LIMIT` in RRF Query
- **Location**: `core/db/repositories/warehouse_repo.py:282` and `core/db/repositories/warehouse_repo.py:387`
- **Severity**: **High**
- **Status**: **NEW**
- **Problem**: 
  In `_execute_rrf_query()` and `hybrid_search_products()`, `top_k` is interpolated into raw SQL text via f-strings:
  ```python
  # line 282
  LIMIT {top_k};
  # line 387
  LIMIT {top_k};
  ```
  While `top_k: int = 5` is type-hinted, query callers passing dynamic parameters can inject malicious SQL fragments if input validation is bypassed. Notice that line 227 (`:top_k_candidates`) properly parameterizes candidates, making this direct f-string injection inconsistent and unsafe.
- **Concrete Fix**:
  Bind `top_k` as a parameter:
  ```python
  # core/db/repositories/warehouse_repo.py:282
  # Change:
  # LIMIT {top_k};
  # To:
  LIMIT :top_k;
  # In params dictionary:
  params["top_k"] = int(top_k)
  ```

---

### Finding N-4: Destructive Fallback in `BaseRepository.insert_dataframe` Drops Tables & Custom Types
- **Location**: `core/db/repositories/base.py:96-105`
- **Severity**: **High**
- **Status**: **NEW**
- **Problem**: 
  When streaming COPY fails, `insert_dataframe` falls back to pandas `to_sql`:
  ```python
  except Exception as copy_err:
      logger.warning(f"Fast COPY failed ({copy_err}), falling back to standard to_sql batching...")
      data.to_sql(
          name=table_name,
          con=self.engine,
          if_exists=if_exists,
          index=False,
          chunksize=chunksize,
          method="multi"
      )
  ```
  If `if_exists == "replace"`, `pandas.DataFrame.to_sql` **drops the table** (`DROP TABLE ...`) and recreates it with inferred basic types. This completely obliterates PostgreSQL vector indexes (`hnsw`), GIN text indexes, and native extensions (`Vector(768)`, `TSVECTOR`), irreversibly corrupting the database schema.
- **Concrete Fix**:
  Never pass `if_exists="replace"` to `to_sql`. Perform a table truncate and use `if_exists="append"`:
  ```python
  # core/db/repositories/base.py:96-105
  except Exception as copy_err:
      logger.warning(f"Fast COPY failed ({copy_err}), falling back to standard to_sql batching...")
      if if_exists == "replace":
          with self.engine.begin() as conn:
              conn.execute(text(f"TRUNCATE TABLE {table_name} RESTART IDENTITY;"))
      data.to_sql(
          name=table_name,
          con=self.engine,
          if_exists="append",
          index=False,
          chunksize=chunksize,
          method="multi"
      )
  ```

---

### Finding N-5: High-Frequency DDL Execution in Active OLTP Cart Path
- **Location**: `core/db/repositories/warehouse_repo.py:461-473`, `477`, `503`
- **Severity**: **Med**
- **Status**: **NEW**
- **Problem**: 
  `ensure_user_carts_table()` executes `CREATE TABLE IF NOT EXISTS user_carts (...)`. This method is explicitly called on *every* cart write (`save_user_cart_snapshot:477`) and *every* cart read (`load_user_cart_snapshot:503`). Executing DDL during frequent transactional operations forces PostgreSQL to acquire catalog locks (`AccessExclusiveLock` on `pg_class`), causing lock contention and degrading OLTP throughput.
- **Concrete Fix**:
  Remove `self.ensure_user_carts_table()` from the request paths. Ensure the table is created strictly during application startup or via migrations:
  ```python
  # In save_user_cart_snapshot and load_user_cart_snapshot:
  # REMOVE: self.ensure_user_carts_table()
  ```

---

### Finding N-6: Redundant DDL `Base.metadata.create_all` on Catalog Read Path
- **Location**: `core/db/repositories/warehouse_repo.py:141-143`
- **Severity**: **Med**
- **Status**: **NEW**
- **Problem**: 
  `get_curated_products()` calls `Base.metadata.create_all(bind=self.engine)` on every query. This reflects all tables and issues `CREATE TABLE IF NOT EXISTS` across all registered ORM models before serving the catalog to shoppers, adding latency to a high-traffic endpoint.
- **Concrete Fix**:
  Remove `Base.metadata.create_all` from `get_curated_products()`.

---

### Finding N-7: Dead Code `get_db()` and Absence of Unit-of-Work Pattern
- **Location**: `core/db/session.py:39-43`
- **Severity**: **Med**
- **Status**: **NEW**
- **Problem**: 
  `get_db()` is defined as a FastAPI dependency yielding an ORM session:
  ```python
  def get_db():
      with get_db_session() as session:
          yield session
  ```
  However, zero files across `apps/api/` import or use `get_db()`. Instead, `apps/api/dependencies.py` injects `BlackFridayRepository()`. Because repositories manage their own isolated connections (`self.engine.connect()` / `self.engine.begin()`), there is no cohesive Unit of Work. Multi-step operations (e.g. user creation followed by initial cart initialization) cannot be rolled back atomically.
- **Concrete Fix**:
  Refactor domain repositories to accept a SQLAlchemy `Session` (or provide a Unit of Work context):
  ```python
  # core/db/repositories/base.py
  class BaseRepository:
      def __init__(self, session: Optional[Session] = None, engine=None):
          self.session = session
          self.engine = engine or (session.bind if session else get_db_engine())
  ```

---

### Finding N-8: Full Table Scan with `MODE() WITHIN GROUP` on Unindexed 550k-Row Table
- **Location**: `core/db/repositories/recommendation_repo.py:59-67`
- **Severity**: **Med**
- **Status**: **NEW**
- **Problem**: 
  `get_product_categories(product_id)` runs:
  ```sql
  SELECT
      MODE() WITHIN GROUP (ORDER BY product_category_1) AS product_category_1,
      MODE() WITHIN GROUP (ORDER BY product_category_2) AS product_category_2,
      MODE() WITHIN GROUP (ORDER BY product_category_3) AS product_category_3
  FROM black_friday_cleaned
  WHERE product_id = :pid
  ```
  `black_friday_cleaned` contains ~550,000 rows. The ORM model defines NO index on `product_id`. Running statistical mode calculations on an unindexed table during real-time ONNX price inference causes a slow sequential scan on every request where product category is omitted.
- **Concrete Fix**:
  Add an index on `product_id` in `core/db/models/warehouse.py`, or precompute category modes into `product_network_metrics`:
  ```python
  # core/db/models/warehouse.py:35
  product_id = Column(String(32), nullable=False, index=True)
  ```

---

### Finding N-9: Blocking O(N) `KEYS` Command in Production Redis Cache
- **Location**: `core/cache/redis_client.py:97`
- **Severity**: **Med**
- **Status**: **NEW**
- **Problem**: 
  `delete_pattern(pattern: str)` invokes `self._client.keys(pattern)`. In Redis, `KEYS` is an O(N) synchronous operation that blocks the server event loop. If executed against a cache containing thousands of keys, it halts all other concurrent Redis operations.
- **Concrete Fix**:
  Use `scan_iter` with batched deletions:
  ```python
  # core/cache/redis_client.py:92-102
  def delete_pattern(self, pattern: str) -> int:
      if not self.is_available or self._client is None:
          return 0
      try:
          deleted_count = 0
          keys_to_delete = []
          for key in self._client.scan_iter(match=pattern, count=100):
              keys_to_delete.append(key)
              if len(keys_to_delete) >= 500:
                  deleted_count += self._client.delete(*keys_to_delete)
                  keys_to_delete = []
          if keys_to_delete:
              deleted_count += self._client.delete(*keys_to_delete)
          return deleted_count
      except Exception as err:
          logger.warning(f"Failed to delete pattern '{pattern}' from Redis: {err}")
          return 0
  ```

---

### Finding N-10: Connection Hang on Every Request When Redis Is Down
- **Location**: `core/cache/redis_client.py:50-54`
- **Severity**: **Med**
- **Status**: **NEW**
- **Problem**: 
  ```python
  @property
  def is_available(self) -> bool:
      if not self._is_available or self._client is None:
          self._connect()
      return self._is_available
  ```
  When Redis is down, `self._connect()` attempts to instantiate a client and ping it with a 2.0-second socket connect timeout. Because `is_available` is evaluated on every `get_json` and `set_json` call, an endpoint making 3 cache calls will hang for 6.0 seconds on *every single incoming HTTP request* without a backoff or circuit breaker.
- **Concrete Fix**:
  Introduce a reconnection cooldown timestamp (e.g. 15-30 seconds):
  ```python
  # core/cache/redis_client.py
  import time

  class RedisCacheManager:
      def __init__(self):
          self._last_connect_attempt = 0.0
          self._retry_cooldown = 15.0  # seconds
          ...

      @property
      def is_available(self) -> bool:
          if not self._is_available or self._client is None:
              now = time.monotonic()
              if now - self._last_connect_attempt > self._retry_cooldown:
                  self._last_connect_attempt = now
                  self._connect()
          return self._is_available
  ```

---

### Finding N-11: Unsafe Dynamic Type Coercion in `BaseRepository.insert_dataframe`
- **Location**: `core/db/repositories/base.py:69-73`
- **Severity**: **Med**
- **Status**: **NEW**
- **Problem**: 
  ```python
  for col in data.select_dtypes(include=["float", "float64"]).columns:
      non_null = data[col].dropna()
      if len(non_null) > 0 and (non_null % 1 == 0).all():
          data[col] = data[col].astype("Int64")
  ```
  If a float column in a specific ingestion batch happens to have only whole number values (e.g. `purchase` values ending in `.00` or `rating` values of `4.0` and `5.0`), this logic unpredictably mutates the column to integer type `Int64`, causing dtype instability across chunks and potential schema mismatch errors.
- **Concrete Fix**:
  Rely on explicit schema definitions rather than heuristic runtime type coercion.

---

### Finding N-12: Unsafe Slicing of Passwords at 72 Bytes in Bcrypt
- **Location**: `core/security.py:32`, `41`
- **Severity**: **Low**
- **Status**: **NEW**
- **Problem**: 
  `pw_bytes = plain.encode("utf-8")[:72]` truncates passwords at 72 bytes. Slicing raw bytes can split a multi-byte UTF-8 character, leaving an invalid byte sequence. Furthermore, users supplying long passwords have the trailing portion silently ignored without notification.
- **Concrete Fix**:
  Pre-hash with SHA-256 before bcrypt, standardizing any password length to 32 bytes:
  ```python
  # core/security.py
  import hashlib

  def hash_password(plain: str) -> str:
      if not HAS_BCRYPT:
          raise RuntimeError("bcrypt library is required for password hashing.")
      digest = hashlib.sha256(plain.encode("utf-8")).digest()
      return bcrypt.hashpw(digest, bcrypt.gensalt()).decode("utf-8")

  def verify_password(plain: str, hashed: str) -> bool:
      if not HAS_BCRYPT:
          return False
      digest = hashlib.sha256(plain.encode("utf-8")).digest()
      return bcrypt.checkpw(digest, hashed.encode("utf-8"))
  ```

---

### Finding N-13: In-Memory Cache on Per-Request Repository Instance Is Instantly Discarded
- **Location**: `core/db/repositories/analytics_repo.py:17-18`, `31-32`
- **Severity**: **Low**
- **Status**: **NEW**
- **Problem**: 
  `AnalyticsRepository.get_eda_summary()` attempts to cache query results on `self._cache_eda_summary`. However, FastAPI's `get_repository()` dependency creates a new `BlackFridayRepository` instance on every request. Thus, `self._cache_eda_summary` is created and destroyed on every request, providing zero caching benefit while masquerading as an optimization.
- **Concrete Fix**:
  Use `cache_manager.get_json / set_json` or a class-level/lru cache:
  ```python
  # core/db/repositories/analytics_repo.py
  from core.cache import cache_manager

  def get_eda_summary(self) -> Dict[str, Any]:
      cache_key = "analytics:eda_summary"
      cached = cache_manager.get_json(cache_key)
      if cached:
          return cached
      ...
      cache_manager.set_json(cache_key, res, ttl=3600)
      return res
  ```

---

### Finding N-14: Redundant Field Validators Violate DRY in `core/config.py`
- **Location**: `core/config.py:107-119` and `146-151`
- **Severity**: **Low**
- **Status**: **NEW**
- **Problem**: 
  `parse_optional_imputer_sample_size`, `parse_optional_imputer_max_depth`, and `parse_optional_depth` repeat identical logic across three separate validator definitions.
- **Concrete Fix**:
  Combine them into a single validator:
  ```python
  # core/config.py
  @field_validator(
      "IMPUTER_SAMPLE_SIZE", "IMPUTER_MAX_DEPTH", "DT_MAX_DEPTH", "RF_MAX_DEPTH", 
      mode="before"
  )
  @classmethod
  def parse_optional_int(cls, v):
      if v == "" or v is None or str(v).lower() in ("none", "null"):
          return None
      return int(v)
  ```

---

### Finding N-15: Missing `user_carts` in `VALID_TABLES` Whitelist
- **Location**: `core/db/repositories/base.py:19-27`
- **Severity**: **Low**
- **Status**: **NEW**
- **Problem**: 
  `VALID_TABLES` lists 7 tables but omits `user_carts`. Any attempt to call `truncate_table("user_carts")` or `insert_dataframe("user_carts", ...)` throws an unexpected `ValueError("Unauthorized table...")`.
- **Concrete Fix**:
  Add `"user_carts"` and `"semantic_query_cache"` to `VALID_TABLES`.

---

### Finding N-16: Unescaped Special Characters in Connection URLs
- **Location**: `core/config.py:48-53`, `69-73`
- **Severity**: **Low**
- **Status**: **NEW**
- **Problem**: 
  Database and Redis URLs are constructed via f-strings with raw passwords. If `POSTGRES_PASSWORD` or `REDIS_PASSWORD` contains URL-reserved characters (e.g., `@`, `:`, `/`, `%`), SQLAlchemy and Redis fail to parse the connection URI.
- **Concrete Fix**:
  Use `urllib.parse.quote_plus`:
  ```python
  import urllib.parse
  @property
  def database_url(self) -> str:
      pwd = urllib.parse.quote_plus(self.POSTGRES_PASSWORD)
      return f"postgresql+psycopg2://{self.POSTGRES_USER}:{pwd}@{self.POSTGRES_HOST}:{self.POSTGRES_PORT}/{self.APP_DB_NAME}"
  ```

---

### Finding N-17: Import-Time Side Effect with `os.makedirs`
- **Location**: `core/logging.py:8`
- **Severity**: **Low**
- **Status**: **NEW**
- **Problem**: 
  `os.makedirs(settings.LOG_DIR, exist_ok=True)` executes unconditionally when `core.logging` is imported. In serverless, read-only Docker containers, or environments with restricted filesystem permissions, importing `core.logging` crashes the entire process before execution even begins.
- **Concrete Fix**:
  Wrap directory creation inside `get_logger()` with permission exception handling:
  ```python
  # core/logging.py
  # Defer directory creation to handler initialization:
  try:
      os.makedirs(settings.LOG_DIR, exist_ok=True)
  except OSError:
      pass
  ```

---

## 4. Top 5 Refactors (Ranked by Impact vs Effort)

| Rank | Refactor Focus | Impact | Effort | Rationale |
| :--- | :--- | :--- | :--- | :--- |
| **1** | **Fix MRO Table Creation & Replace Inheritance with Composition** | **CRITICAL** | **LOW** | Eliminates the bug where `main.py` fails to create 6 of 8 warehouse tables on startup, while replacing God-object multiple inheritance with clean composition. |
| **2** | **Make `GEMINI_API_KEY` Optional in `core/config.py`** | **HIGH** | **LOW** | 1-line change that unblocks offline CI, tests, and non-AI batch jobs from crashing on module import. |
| **3** | **Remove Inline DDL from Cart & Catalog Paths** | **HIGH** | **LOW** | Removing `CREATE TABLE IF NOT EXISTS` and `Base.metadata.create_all` from read/write request methods eliminates `AccessExclusiveLock` catalog contention. |
| **4** | **Parameterize `LIMIT` in `WarehouseRepository` RRF Search** | **HIGH** | **LOW** | Parameterizing `:top_k` eliminates SQL injection vulnerability in search relaxation queries. |
| **5** | **Add Reconnect Cooldown & `scan_iter` to `RedisCacheManager`** | **HIGH** | **MED** | Prevents blocking multi-second hangs when Redis is offline and eliminates O(N) blocking `KEYS` command during cache invalidation. |

---

## 5. Dependencies on Other Domains (Check in Later Sessions)

1. **Session 2 (ML Pipeline & Features)**:
   - Verify `ml/features/data_contract.py` against `core/db/models/warehouse.py`: confirm that missing database constraints do not allow unvalidated dirty data into `black_friday_cleaned`.
   - Verify if `ml/pipelines/ingest.py` and `ml/pipelines/seed_curated.py` depend on `BlackFridayRepository.create_app_tables()` or invoke their own DDL scripts.
2. **Session 3 (Backend API & Serving)**:
   - Check `apps/api/dependencies.py`: ensure replacing `BlackFridayRepository` multiple inheritance with composition does not break FastAPI dependency injection in routes (`analytics.py`, `auth.py`, `shopper.py`).
   - Check `apps/api/core/rate_limiter.py:44`: update private `cache_manager._client` reference once public `cache_manager.client` is exposed.
   - Audit `apps/api/routes/shopper.py`: verify if category mode fallback in `recommendation_repo.py` causes latency spikes during ONNX price estimation.
3. **Session 4 (AI Subsystem)**:
   - Check `ai/nodes/cart_node.py:7`, `bundle_node.py:6`, `support_node.py:7`: replace direct `loguru` imports with `from core.logging import get_logger`.
   - Check `ai/services/two_tier_cache_service.py`: verify interaction with `semantic_query_cache` table and pgvector cosine distance lookups.
4. **Session 6 (Docker & CI/CD)**:
   - Verify `docker/postgres/init_schema.sql`: check whether `user_carts` and `semantic_query_cache` tables are pre-initialized in containerized environments.
