"""
Core MLflow Tracking and Observability Client.
Centralizes MLflow configuration, tracking URI, S3/MinIO endpoint management,
client initialization, experiment management, and tracing helpers for the entire platform.
"""
import os
from typing import Optional, Dict, Any, List, Callable

try:
    import mlflow
    import mlflow.langchain
    from mlflow.tracking import MlflowClient
    from mlflow.entities import SpanType
    HAS_MLFLOW = True
except ImportError:
    mlflow = None  # type: ignore
    HAS_MLFLOW = False

    class SpanType:  # type: ignore
        TOOL = "TOOL"
        CHAIN = "CHAIN"
        RETRIEVER = "RETRIEVER"
        AGENT = "AGENT"
        LLM = "LLM"

    class MlflowClient:  # type: ignore
        def __init__(self, *args, **kwargs):
            pass

from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)


def setup_tracking_environment() -> str:
    """Configures environment variables for S3/MinIO artifact storage and tracking URI.
    
    Reads endpoint URL dynamically from core.config settings, eliminating any hardcoded hosts.
    Returns the resolved S3 endpoint URL.
    """
    s3_endpoint = (
        getattr(settings, "MLFLOW_S3_ENDPOINT_URL", None)
        or getattr(settings, "S3_ENDPOINT_URL", None)
        or os.getenv("MLFLOW_S3_ENDPOINT_URL")
        or os.getenv("S3_ENDPOINT_URL")
        or settings.s3_endpoint_url
    )
    os.environ["MLFLOW_S3_ENDPOINT_URL"] = s3_endpoint
    os.environ.setdefault("AWS_ACCESS_KEY_ID", settings.MINIO_ROOT_USER)
    os.environ.setdefault("AWS_SECRET_ACCESS_KEY", settings.MINIO_ROOT_PASSWORD)
    os.environ.setdefault("MLFLOW_DISABLE_AGENT_HINT", "1")
    return s3_endpoint


def get_tracking_uri() -> str:
    """Returns the configured MLflow tracking URI."""
    return getattr(settings, "MLFLOW_TRACKING_URI", "http://localhost:5000")


def get_mlflow_client(tracking_uri: Optional[str] = None) -> Optional[MlflowClient]:
    """Factory creating an authenticated MlflowClient connected to the configured tracking URI."""
    if not HAS_MLFLOW or mlflow is None:
        logger.warning("[TRACKING: CORE] mlflow package not installed. Returning null MlflowClient.")
        return MlflowClient()

    setup_tracking_environment()
    uri = tracking_uri or get_tracking_uri()
    try:
        mlflow.set_tracking_uri(uri)
        return MlflowClient(tracking_uri=uri)
    except Exception as e:
        logger.warning(f"[TRACKING: CORE] Could not initialize MlflowClient at {uri}: {e}")
        return None


def get_or_create_experiment(experiment_name: str, artifact_location: Optional[str] = None) -> Optional[str]:
    """Retrieves an existing experiment ID or creates a new one, and sets it as active."""
    if not HAS_MLFLOW or mlflow is None:
        return None

    setup_tracking_environment()
    uri = get_tracking_uri()
    try:
        mlflow.set_tracking_uri(uri)
        client = MlflowClient(tracking_uri=uri)
        exp = client.get_experiment_by_name(experiment_name)
        if exp is not None:
            exp_id = exp.experiment_id
        else:
            exp_id = client.create_experiment(name=experiment_name, artifact_location=artifact_location)
        mlflow.set_experiment(experiment_name)
        return exp_id
    except Exception as e:
        logger.warning(f"[TRACKING: CORE] Could not get or create experiment '{experiment_name}': {e}")
        try:
            mlflow.set_experiment(experiment_name)
        except Exception:
            pass
        return None


def init_mlflow_tracking(experiment_name: Optional[str] = None) -> None:
    """Initializes tracking environment, tracking URI, and active experiment."""
    if not HAS_MLFLOW or mlflow is None:
        return
    setup_tracking_environment()
    uri = get_tracking_uri()
    exp_name = experiment_name or getattr(settings, "MLFLOW_EXPERIMENT_NAME", "black-friday-sales-prediction")
    try:
        mlflow.set_tracking_uri(uri)
        get_or_create_experiment(exp_name)
    except Exception as e:
        logger.warning(f"[TRACKING: CORE] Failed to initialize MLflow tracking: {e}")


def enable_langchain_autolog(
    log_models: bool = False,
    log_input_examples: bool = False,
    log_traces: bool = True
) -> None:
    """Enables framework autologging for LangChain and LangGraph."""
    if not HAS_MLFLOW or mlflow is None:
        return
    try:
        import mlflow.langchain
        mlflow.langchain.autolog(
            log_models=log_models,
            log_input_examples=log_input_examples,
            log_traces=log_traces,
        )
        logger.info("[TRACKING: CORE] mlflow.langchain.autolog() initialized successfully.")
    except Exception as e:
        logger.debug(f"[TRACKING: CORE] Notice during LangChain autolog initialization: {e}")


def trace(name: Optional[str] = None, span_type: Optional[str] = None):
    """Decorator for tracing function executions with MLflow trace."""
    if HAS_MLFLOW and mlflow is not None and hasattr(mlflow, "trace"):
        kwargs = {}
        if name:
            kwargs["name"] = name
        if span_type:
            kwargs["span_type"] = span_type
        return mlflow.trace(**kwargs)
    else:
        def noop_decorator(func: Callable):
            return func
        return noop_decorator


def update_current_trace(metadata: Optional[Dict[str, Any]] = None, tags: Optional[Dict[str, Any]] = None) -> None:
    """Helper to safely update the active MLflow trace with metadata and tags."""
    if not HAS_MLFLOW or mlflow is None or not hasattr(mlflow, "update_current_trace"):
        return
    try:
        mlflow.update_current_trace(metadata=metadata, tags=tags)
    except Exception:
        pass
