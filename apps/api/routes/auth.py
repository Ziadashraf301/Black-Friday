"""
Authentication routes — signup, login, and profile.
"""
from typing import Dict, Any
from fastapi import APIRouter, Depends, HTTPException, status

from apps.api.schemas import SignupRequest, LoginRequest, TokenResponse, UserMeResponse
from apps.api.dependencies import get_repository
from apps.api.auth import get_current_user
from apps.api.services.auth_service import auth_service
from core.db.repository import BlackFridayRepository

router = APIRouter(prefix="/auth", tags=["Authentication"])


@router.post("/signup", response_model=TokenResponse, status_code=status.HTTP_201_CREATED)
def signup(request: SignupRequest, repo: BlackFridayRepository = Depends(get_repository)):
    """Registers a new shopper and issues a JWT token."""
    res = auth_service.register_user(request.model_dump(), repo)
    return TokenResponse(**res)


@router.post("/login", response_model=TokenResponse)
def login(request: LoginRequest, repo: BlackFridayRepository = Depends(get_repository)):
    """Authenticates with email and password, returning a JWT token."""
    res = auth_service.authenticate_user(request.email, request.password, repo)
    return TokenResponse(**res)


@router.get("/me", response_model=UserMeResponse)
def get_me(
    current_user: Dict[str, Any] = Depends(get_current_user),
    repo: BlackFridayRepository = Depends(get_repository),
):
    """Returns the authenticated shopper's profile."""
    user = repo.get_user_by_id(current_user["user_id"])
    if not user:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="User not found.")
    return UserMeResponse(**{k: user.get(k) for k in UserMeResponse.model_fields})
