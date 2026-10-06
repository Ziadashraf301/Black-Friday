# TODO-remove: Compatibility shim for apps.api.serving.predictor -> ml.serving.predictor
from ml.serving.predictor import ONNXPredictor  # noqa: F401

__all__ = ["ONNXPredictor"]
