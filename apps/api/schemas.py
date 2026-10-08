from pydantic import BaseModel, Field
from typing import List, Dict, Any, Optional

# --- Regression & Serving Schemas ---
class PurchasePredictionRequest(BaseModel):
    product_category_1: int = Field(..., ge=1, le=20, example=3)
    product_category_2: Optional[int] = Field(default=4, ge=1, le=20, example=4)
    product_category_3: Optional[int] = Field(default=12, ge=1, le=20, example=12)
    product_id: str = Field(default="P00069042", example="P00069042")
    model_name: str = Field(default="random_forest", example="random_forest")

class PurchasePredictionResponse(BaseModel):
    model_used: str
    predicted_purchase_usd: float
    normalized_prediction: float
    product_id: str

class BatchPredictionRequest(BaseModel):
    items: List[PurchasePredictionRequest]

class BatchPredictionResponse(BaseModel):
    predictions: List[PurchasePredictionResponse]

class ModelMetricsResponse(BaseModel):
    model_name: str
    test_rmse: float
    test_r2: float
    cv_mean_rmse: float
    cv_mean_r2: float

# --- Customer Segmentation Schemas ---
class CustomerClusterPredictionRequest(BaseModel):
    lifetime_value: float = Field(..., ge=0, example=150000.0)
    frequency: int = Field(..., ge=1, example=25)
    gender: str = Field(..., example="M")
    marital_status: str = Field(..., example="Single")
    age: str = Field(..., example="26-35")

class CustomerPersonaResponse(BaseModel):
    cluster_id: int
    cluster_persona: str
    recommended_action: str
    customer_count: Optional[int] = None
    avg_lifetime_value: Optional[float] = None
    avg_frequency: Optional[float] = None
    avg_aov: Optional[float] = None

# --- Market Basket & Network Schemas ---
class RecommendationRequest(BaseModel):
    product_id: str = Field(..., example="P00110742")
    limit: int = Field(default=5, ge=1, le=20)

class ProductRecommendation(BaseModel):
    recommended_product_id: str
    confidence: float
    lift: float

class RecommendationResponse(BaseModel):
    target_product_id: str
    recommendations: List[ProductRecommendation]

class ProductCentralityResponse(BaseModel):
    product_id: str
    order_count: int
    pagerank_score: float
    hub_score: float
    authority_score: float
    top_associated_product: Optional[str] = None
    highest_lift_rule: Optional[float] = None
    top_bundle_recommendations: Optional[Any] = None
    item2vec_recommendations: Optional[Any] = None

class UnifiedProductRecommendationResponse(BaseModel):
    product_id: str
    order_count: int
    pagerank_score: float
    top_associated_product: Optional[str] = None
    apriori_bundles: List[str] = []
    item2vec_similar: List[str] = []


# --- Analytics & Hypothesis Testing Schemas ---
class EDASummaryResponse(BaseModel):
    total_orders: int
    total_users: int
    total_products: int
    avg_order_value: float
    total_revenue: float

# --- Auth Schemas ---
class SignupRequest(BaseModel):
    name: str = Field(..., min_length=2, max_length=80, example="Ahmed Hassan")
    email: str = Field(..., example="ahmed@example.com")
    password: str = Field(..., min_length=6, example="securepass123")
    gender: str = Field(..., example="M")
    age: str = Field(..., example="26-35")
    city_category: str = Field(..., example="B")
    marital_status: int = Field(..., ge=0, le=1, example=0)
    occupation: int = Field(..., ge=0, le=20, example=4)
    stay_in_current_city_years: str = Field(..., example="2")

class LoginRequest(BaseModel):
    email: str = Field(..., example="ahmed@example.com")
    password: str = Field(..., example="securepass123")

class TokenResponse(BaseModel):
    access_token: str
    token_type: str = "bearer"
    user_id: int
    name: str
    cluster_persona: Optional[str] = None

class UserMeResponse(BaseModel):
    user_id: int
    name: str
    email: str
    gender: Optional[str] = None
    age: Optional[str] = None
    city_category: Optional[str] = None
    marital_status: Optional[int] = None
    occupation: Optional[int] = None
    cluster_id: Optional[int] = None
    cluster_persona: Optional[str] = None
    recommended_action: Optional[str] = None


# --- Shopper Schemas ---
class ShopperPredictRequest(BaseModel):
    product_id: str = Field(..., example="P00110742")
    product_category_1: Optional[int] = Field(default=None, ge=1, le=20, example=1)
    product_category_2: Optional[int] = Field(default=None, ge=1, le=20, example=6)
    product_category_3: Optional[int] = Field(default=None, ge=1, le=20, example=14)
    gender: Optional[str] = Field(default=None, example="M")
    age: Optional[str] = Field(default=None, example="26-35")
    occupation: Optional[int] = Field(default=None, example=4)
    city_category: Optional[str] = Field(default=None, example="B")
    stay_in_current_city_years: Optional[str] = Field(default=None, example="2")
    marital_status: Optional[int] = Field(default=None, example=0)

class ShopperPredictResponse(BaseModel):
    product_id: str
    predicted_usd: float
    catalog_price: Optional[float] = None
    normalized_prediction: float
    model_used: str

class ShopperBatchPredictRequest(BaseModel):
    items: List[ShopperPredictRequest]

class ShopperBatchPredictResponse(BaseModel):
    quotes: List[ShopperPredictResponse]

class BrowseProductResponse(BaseModel):
    product_id: str
    order_count: int
    pagerank_score: float
    hub_score: Optional[float] = None
    authority_score: Optional[float] = None
    top_associated_product: Optional[str] = None
    highest_lift_rule: Optional[float] = None
    apriori_bundles: List[str] = []
    item2vec_similar: List[str] = []

class CatalogProductItem(BaseModel):
    product_id: str
    order_count: int
    pagerank_score: float

class CuratedBundleItem(BaseModel):
    product_id: str
    name: str
    image_url: str
    price: float
    lift: Optional[float] = None
    confidence: Optional[float] = None
    cosine_similarity: Optional[float] = None

class CuratedProductItem(BaseModel):
    product_id: str
    name: str
    tagline: str
    description: str
    category_name: str
    product_category_1: int
    product_category_2: Optional[int] = None
    product_category_3: Optional[int] = None
    gender: str
    brand: str
    style: str
    season: str
    badge: Optional[str] = None
    is_hero: bool = False
    original_price: float
    discounted_price: float
    sizes: List[str] = []
    image_url: str
    order_count: int
    apriori_bundles: List[CuratedBundleItem] = []
    item2vec_similars: List[CuratedBundleItem] = []

class ShopperPurchaseResponse(BaseModel):
    id: int
    product_id: str
    predicted_usd: float
    model_used: str
    purchased_at: str


class ShopperBatchPurchaseItem(BaseModel):
    product_id: str = Field(..., example="P00110742")
    product_category_1: Optional[int] = Field(default=None, ge=1, le=20, example=1)
    product_category_2: Optional[int] = Field(default=None, ge=1, le=20, example=6)
    product_category_3: Optional[int] = Field(default=None, ge=1, le=20, example=14)
    quantity: int = Field(default=1, ge=1, example=1)
    gender: Optional[str] = Field(default=None, example="M")
    age: Optional[str] = Field(default=None, example="26-35")
    occupation: Optional[int] = Field(default=None, example=4)
    city_category: Optional[str] = Field(default=None, example="B")
    stay_in_current_city_years: Optional[str] = Field(default=None, example="2")
    marital_status: Optional[int] = Field(default=None, example=0)


class ShopperBatchPurchaseRequest(BaseModel):
    items: List[ShopperBatchPurchaseItem]


class ShopperBatchPurchaseResponse(BaseModel):
    purchases: List[ShopperPurchaseResponse]
    total_items: int = 0
    total_amount: float = 0.0


class PurchaseHistoryResponse(BaseModel):
    user_id: int
    total_purchases: int
    purchases: List[Dict[str, Any]]


