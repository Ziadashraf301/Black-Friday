"""
Shopper routes — catalog browsing, recommendations, tailored pricing, and purchase history.
"""
from typing import List, Dict, Any
from fastapi import APIRouter, Depends

from apps.api.schemas import (
    CatalogProductItem, BrowseProductResponse,
    ShopperPredictRequest, ShopperPredictResponse,
    ShopperPurchaseResponse, PurchaseHistoryResponse
)
from apps.api.dependencies import get_repository
from apps.api.auth import get_current_user
from apps.api.services.shopper_service import shopper_service
from core.db.repository import BlackFridayRepository

router = APIRouter(prefix="/shopper", tags=["Shopper Experience"])


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
        repo=repo
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
