from contextlib import asynccontextmanager
from typing import List
from fastapi import FastAPI, Request
from fastapi.middleware.cors import CORSMiddleware
from fastapi.middleware.gzip import GZipMiddleware
from fastapi.responses import JSONResponse
from starlette.exceptions import HTTPException as StarletteHTTPException

from apps.api.routes import analytics
from apps.api.routes import auth as auth_router
from apps.api.routes import shopper as shopper_router
from apps.api.routes import bot as bot_router
from apps.api.middleware.security_ban_middleware import SecurityBanMiddleware
from apps.api.services.model_service import model_service
from core.config import settings
from core.exceptions import (
    AppException,
    NotFoundError,
    DataUnavailableError,
    ValidationError,
    UnauthorizedError,
    ForbiddenError,
)
from core.logging import get_logger

logger = get_logger(__name__)


@asynccontextmanager
async def lifespan(app: FastAPI):
    """Application startup and shutdown lifecycle hooks."""
    logger.info("Initializing Black Friday API Services...")

    # 1. Load ONNX models strictly from local storage once on startup
    model_service.load_models()

    # 2. Ensure application database tables exist
    try:
        from core.db.repository import BlackFridayRepository
        BlackFridayRepository().create_app_tables()
    except Exception as e:
        logger.warning(f"App table initialization check (non-fatal): {e}")

    yield

    logger.info("Shutting down API services...")


app = FastAPI(
    title="Black Friday Enterprise API",
    description="Clean production API for Executive Analytics and Personalized Shopper Experience.",
    version="3.0.0",
    lifespan=lifespan,
)

# GZip compression for responses > 1000 bytes
app.add_middleware(GZipMiddleware, minimum_size=1000)

# Explicit CORS origins for credentialed access (Fix 5.1)
default_origins = [
    "http://localhost:3000",
    "http://127.0.0.1:3000",
    "http://localhost:8000",
    "http://127.0.0.1:8000",
    "http://localhost:8001",
    "http://127.0.0.1:8001",
]
configured_origins = [
    o.strip() for o in (getattr(settings, "CORS_ORIGINS", "") or "").split(",") if o.strip()
]
cors_origins = sorted(list(set(default_origins + configured_origins)))

app.add_middleware(
    CORSMiddleware,
    allow_origins=cors_origins,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Security Ban & Strike Lockout Gateway Middleware (Phase 4 - Task P4-07)
app.add_middleware(SecurityBanMiddleware)


# =============================================================================
# Structured Exception Handlers (Fix 5.1 & Fix 10.1)
# =============================================================================
@app.exception_handler(ValueError)
async def value_error_handler(request: Request, exc: ValueError):
    return JSONResponse(
        status_code=400,
        content={"detail": str(exc), "error_type": "ValueError"},
    )


@app.exception_handler(NotFoundError)
async def not_found_handler(request: Request, exc: NotFoundError):
    return JSONResponse(
        status_code=404,
        content={"detail": exc.message, "error_type": exc.code},
    )


@app.exception_handler(DataUnavailableError)
async def data_unavailable_handler(request: Request, exc: DataUnavailableError):
    return JSONResponse(
        status_code=404,
        content={"detail": exc.message, "error_type": exc.code},
    )


@app.exception_handler(ValidationError)
async def validation_error_handler(request: Request, exc: ValidationError):
    return JSONResponse(
        status_code=400,
        content={"detail": exc.message, "error_type": exc.code},
    )


@app.exception_handler(UnauthorizedError)
async def unauthorized_handler(request: Request, exc: UnauthorizedError):
    return JSONResponse(
        status_code=401,
        content={"detail": exc.message, "error_type": exc.code},
        headers={"WWW-Authenticate": "Bearer"},
    )


@app.exception_handler(ForbiddenError)
async def forbidden_handler(request: Request, exc: ForbiddenError):
    return JSONResponse(
        status_code=403,
        content={"detail": exc.message, "error_type": exc.code},
    )


@app.exception_handler(AppException)
async def app_exception_handler(request: Request, exc: AppException):
    return JSONResponse(
        status_code=400,
        content={"detail": exc.message, "error_type": exc.code},
    )


@app.exception_handler(Exception)
async def global_exception_handler(request: Request, exc: Exception):
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


# Mount route controllers
app.include_router(analytics.router)
app.include_router(auth_router.router)
app.include_router(shopper_router.router)
app.include_router(bot_router.router)


@app.get("/health", tags=["Health & Diagnostics"])
def health_check():
    """Healthcheck endpoint reporting service and local model status."""
    return {
        "status": "healthy",
        "service": settings.PROJECT_NAME,
        "environment": settings.ENVIRONMENT,
        "version": "3.0.0",
        "champion_model": model_service.model_name,
        "imputer_loaded": model_service.imputer is not None,
    }


if __name__ == "__main__":
    import uvicorn
    uvicorn.run("apps.api.main:app", host=settings.API_HOST, port=settings.API_PORT, reload=True)
