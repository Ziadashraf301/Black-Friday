"""
Shopper routes — catalog browsing, recommendations, tailored pricing, and purchase history.
"""
from typing import List, Dict, Any, Optional
from fastapi import APIRouter, Depends

from apps.api.schemas import (
    CatalogProductItem, BrowseProductResponse,
    ShopperPredictRequest, ShopperPredictResponse,
    ShopperBatchPredictRequest, ShopperBatchPredictResponse,
    CuratedProductItem,
    ShopperPurchaseResponse, PurchaseHistoryResponse
)
from apps.api.dependencies import get_repository
from apps.api.auth import get_current_user
from apps.api.services.shopper_service import shopper_service
from core.db.repository import BlackFridayRepository

router = APIRouter(prefix="/shopper", tags=["Shopper Experience"])


@router.get("/curated-catalog", response_model=List[CuratedProductItem])
def get_curated_catalog():
    """Returns top curated editorial vintage products with Apriori and Item2Vec associations."""
    return shopper_service.get_curated_catalog()


@router.get("/catalog", response_model=List[CatalogProductItem])
def get_product_catalog(
    limit: int = 100,
    repo: BlackFridayRepository = Depends(get_repository),
):
    """Returns top products for catalog browsing."""
    items = shopper_service.get_catalog(limit=limit, repo=repo)
    return [CatalogProductItem(**item) for item in items]


@router.get("/browse/{product_id}", response_model=BrowseProductResponse)
def browse_product(
    product_id: str,
    repo: BlackFridayRepository = Depends(get_repository),
):
    """Returns product details and smart recommendations."""
    detail = shopper_service.browse_product(product_id=product_id, repo=repo)
    return BrowseProductResponse(**detail)


@router.post("/predict-price-batch", response_model=ShopperBatchPredictResponse)
def predict_price_batch(
    request: ShopperBatchPredictRequest,
    current_user: Optional[Dict[str, Any]] = Depends(get_current_user),
    repo: BlackFridayRepository = Depends(get_repository),
):
    """Calculates personalized batch quotes for high-concurrency cart processing."""
    user_id = current_user.get("user_id") if current_user else None
    user_demo = {
        "gender": current_user.get("gender") if current_user else None,
        "age": current_user.get("age") if current_user else None,
        "city_category": current_user.get("city_category") if current_user else None,
        "marital_status": current_user.get("marital_status") if current_user else None,
        "occupation": current_user.get("occupation") if current_user else None,
        "stay_in_current_city_years": current_user.get("stay_in_current_city_years") if current_user else None,
    }
    quotes = shopper_service.estimate_price_batch(
        items=[item.model_dump() for item in request.items],
        repo=repo,
        user_id=user_id,
        user_demographics=user_demo,
    )
    return ShopperBatchPredictResponse(quotes=[ShopperPredictResponse(**q) for q in quotes])


@router.post("/predict-price", response_model=ShopperPredictResponse)
def predict_price_for_shopper(
    request: ShopperPredictRequest,
    current_user: Dict[str, Any] = Depends(get_current_user),
    repo: BlackFridayRepository = Depends(get_repository),
):
    """Calculates personalized purchase price quote for shopper."""
    res = shopper_service.estimate_price(
        product_id=request.product_id,
        cat1=request.product_category_1,
        cat2=request.product_category_2,
        cat3=request.product_category_3,
        repo=repo,
        user_id=current_user.get("user_id"),
        gender=current_user.get("gender"),
        age=current_user.get("age"),
        city_category=current_user.get("city_category"),
        marital_status=current_user.get("marital_status"),
        occupation=current_user.get("occupation"),
        stay_in_current_city_years=current_user.get("stay_in_current_city_years"),
    )
    return ShopperPredictResponse(**res)


@router.post("/purchase", response_model=ShopperPurchaseResponse)
def record_purchase(
    request: ShopperPredictRequest,
    current_user: Dict[str, Any] = Depends(get_current_user),
    repo: BlackFridayRepository = Depends(get_repository),
):
    """Executes price calculation and commits transaction to purchase history."""
    res = shopper_service.process_purchase(
        user_id=current_user["user_id"],
        product_id=request.product_id,
        cat1=request.product_category_1,
        cat2=request.product_category_2,
        cat3=request.product_category_3,
        repo=repo
    )
    return ShopperPurchaseResponse(**res)


@router.get("/history", response_model=PurchaseHistoryResponse)
def get_purchase_history(
    current_user: Dict[str, Any] = Depends(get_current_user),
    repo: BlackFridayRepository = Depends(get_repository),
):
    """Returns recent purchase history for the authenticated shopper."""
    data = shopper_service.get_purchase_history(user_id=current_user["user_id"], repo=repo)
    return PurchaseHistoryResponse(**data)
