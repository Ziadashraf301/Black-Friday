from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from contextlib import asynccontextmanager
from apps.api.routes import analytics
from apps.api.routes import auth as auth_router
from apps.api.routes import shopper as shopper_router
from apps.api.services.model_service import model_service
from core.config import settings
from core.logging import get_logger
from fastapi.middleware.gzip import GZipMiddleware
from fastapi.staticfiles import StaticFiles
from pathlib import Path


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


# Mount static product assets directory
static_dir = Path(__file__).resolve().parent / "static" / "products"
if static_dir.exists():
    app.mount("/products", StaticFiles(directory=str(static_dir)), name="products")

# GZip compression for responses > 1000 bytes
app.add_middleware(GZipMiddleware, minimum_size=1000)

# CORS
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Mount thin route controllers
app.include_router(analytics.router)
app.include_router(auth_router.router)
app.include_router(shopper_router.router)


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
