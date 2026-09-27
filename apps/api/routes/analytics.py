"""
Analytics routes — executive KPIs and demographic significance tests.
"""
from typing import List, Dict, Any
from fastapi import APIRouter, Depends

from apps.api.schemas import EDASummaryResponse
from apps.api.dependencies import get_repository
from core.db.repository import BlackFridayRepository
from apps.api.services.analytics_service import analytics_service

router = APIRouter(prefix="/analytics", tags=["Analytics & EDA"])


@router.get("/summary", response_model=EDASummaryResponse)
def get_executive_summary(repo: BlackFridayRepository = Depends(get_repository)):
    """Retrieves executive summary metrics (Total Orders, Users, Products, Revenue, AOV)."""
    data = analytics_service.get_summary(repo)
    return EDASummaryResponse(**data)


@router.get("/demographics/{column_name}")
def get_demographic_breakdown(
    column_name: str,
    repo: BlackFridayRepository = Depends(get_repository),
):
    """Distribution of orders, revenue, and AOV by demographic dimension."""
    return analytics_service.get_demographics(column_name, repo)
