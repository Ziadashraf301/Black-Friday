"""
Core Domain Exceptions (Fix 10.1).
Decoupled domain exception hierarchy independent of FastAPI / HTTP layer.
"""


class AppException(Exception):
    """Base exception for all domain and application errors."""

    def __init__(self, message: str, code: str = "APP_ERROR"):
        super().__init__(message)
        self.message = message
        self.code = code


class NotFoundError(AppException):
    """Raised when a requested resource is not found."""

    def __init__(self, message: str = "Resource not found.", code: str = "NOT_FOUND"):
        super().__init__(message, code=code)


class DataUnavailableError(AppException):
    """Raised when underlying analytical data is not yet populated or unavailable."""

    def __init__(self, message: str = "Data unavailable.", code: str = "DATA_UNAVAILABLE"):
        super().__init__(message, code=code)


class ValidationError(AppException):
    """Raised when request payload or dimension parameter fails validation."""

    def __init__(self, message: str = "Validation failed.", code: str = "VALIDATION_ERROR"):
        super().__init__(message, code=code)


class UnauthorizedError(AppException):
    """Raised when authentication credentials are missing or invalid."""

    def __init__(self, message: str = "Authentication required.", code: str = "UNAUTHORIZED"):
        super().__init__(message, code=code)


class ForbiddenError(AppException):
    """Raised when caller lacks required authorization or permissions."""

    def __init__(self, message: str = "Access forbidden.", code: str = "FORBIDDEN"):
        super().__init__(message, code=code)


class ConflictError(AppException):
    """Raised when an operation conflicts with existing state (e.g. email already exists)."""

    def __init__(self, message: str = "Resource conflict.", code: str = "CONFLICT"):
        super().__init__(message, code=code)


class RateLimitExceededError(AppException):
    """Raised when an operation or user exceeds the rate limit threshold."""

    def __init__(self, message: str = "Rate limit exceeded.", code: str = "RATE_LIMIT_EXCEEDED", headers: dict = None):
        super().__init__(message, code=code)
        self.headers = headers or {}


__all__ = [
    "AppException",
    "NotFoundError",
    "DataUnavailableError",
    "ValidationError",
    "UnauthorizedError",
    "ForbiddenError",
    "ConflictError",
    "RateLimitExceededError",
]
