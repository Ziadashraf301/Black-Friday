from fastapi import FastAPI, Request
from fastapi.responses import JSONResponse
from starlette.exceptions import HTTPException as StarletteHTTPException

from core.exceptions import (
    AppException,
    NotFoundError,
    DataUnavailableError,
    ValidationError,
    UnauthorizedError,
    ForbiddenError,
    ConflictError,
    RateLimitExceededError,
)
from core.logging import get_logger

logger = get_logger(__name__)


async def value_error_handler(request: Request, exc: ValueError) -> JSONResponse:
    return JSONResponse(
        status_code=400,
        content={"detail": str(exc), "error_type": "ValueError"},
    )


async def not_found_handler(request: Request, exc: NotFoundError) -> JSONResponse:
    return JSONResponse(
        status_code=404,
        content={"detail": exc.message, "error_type": exc.code},
    )


async def data_unavailable_handler(request: Request, exc: DataUnavailableError) -> JSONResponse:
    return JSONResponse(
        status_code=404,
        content={"detail": exc.message, "error_type": exc.code},
    )


async def validation_error_handler(request: Request, exc: ValidationError) -> JSONResponse:
    return JSONResponse(
        status_code=400,
        content={"detail": exc.message, "error_type": exc.code},
    )


async def unauthorized_handler(request: Request, exc: UnauthorizedError) -> JSONResponse:
    return JSONResponse(
        status_code=401,
        content={"detail": exc.message, "error_type": exc.code},
        headers={"WWW-Authenticate": "Bearer"},
    )


async def forbidden_handler(request: Request, exc: ForbiddenError) -> JSONResponse:
    return JSONResponse(
        status_code=403,
        content={"detail": exc.message, "error_type": exc.code},
    )


async def conflict_handler(request: Request, exc: ConflictError) -> JSONResponse:
    return JSONResponse(
        status_code=409,
        content={"detail": exc.message, "error_type": exc.code},
    )


async def rate_limit_exceeded_handler(request: Request, exc: RateLimitExceededError) -> JSONResponse:
    return JSONResponse(
        status_code=429,
        content={"detail": exc.message, "error_type": exc.code},
        headers=exc.headers,
    )


async def app_exception_handler(request: Request, exc: AppException) -> JSONResponse:
    return JSONResponse(
        status_code=400,
        content={"detail": exc.message, "error_type": exc.code},
    )


async def global_exception_handler(request: Request, exc: Exception) -> JSONResponse:
    if isinstance(exc, StarletteHTTPException):
        return JSONResponse(
            status_code=exc.status_code,
            content={"detail": exc.detail, "error_type": "HTTPException"},
            headers=getattr(exc, "headers", None),
        )
    logger.error(f"Unhandled server error on {request.url.path}: {exc}", exc_info=True)
    return JSONResponse(
        status_code=500,
        content={"detail": "An internal server error occurred.", "error_type": "InternalServerError"},
    )


def register_exception_handlers(app: FastAPI) -> None:
    """Register all custom and global exception handlers to the FastAPI app."""
    app.add_exception_handler(ValueError, value_error_handler)
    app.add_exception_handler(NotFoundError, not_found_handler)
    app.add_exception_handler(DataUnavailableError, data_unavailable_handler)
    app.add_exception_handler(ValidationError, validation_error_handler)
    app.add_exception_handler(UnauthorizedError, unauthorized_handler)
    app.add_exception_handler(ForbiddenError, forbidden_handler)
    app.add_exception_handler(ConflictError, conflict_handler)
    app.add_exception_handler(RateLimitExceededError, rate_limit_exceeded_handler)
    app.add_exception_handler(AppException, app_exception_handler)
    app.add_exception_handler(Exception, global_exception_handler)
