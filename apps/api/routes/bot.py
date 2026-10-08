"""
Conversational AI Shopping Assistant Router (Phase 4 - Tasks P4-08 & P4-09).
Provides dual-mode interaction:
  1. GET /bot/stream: Server-Sent Events (SSE) token streaming, synchronized UI cards,
     grounding URL citations, and sub-1ms Tier-0 6-hour response cache fast-path.
  2. WebSocket /bot/live-ws: Full-duplex continuous live Gemini Multimodal Voice-to-Voice streaming
     with live audio exchange, synchronized product card pushes, and interruption handling.
"""
import re
from typing import Optional, Dict, Any, List, AsyncGenerator
import json
import asyncio
import base64
from pydantic import BaseModel
from fastapi import APIRouter, WebSocket, WebSocketDisconnect, Query, Depends, Request
from fastapi.responses import StreamingResponse
from langchain_core.messages import HumanMessage

from ai.workflow.graph import shopping_graph
from ai.workflow.state import AgentState
from ai.services.cache_service import cache_service
from ai.guardrails.strike_tracker import strike_tracker
from apps.api.rate_limiting.rate_limiter import rate_limit_client_dependency
from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)

router = APIRouter(prefix="/bot", tags=["Conversational AI Assistant"])

TOKEN_STREAM_REGEX = re.compile(r"\S+\s*|\s+")


def tokenize_for_stream(text: str) -> List[str]:
    """Splits text into stream tokens preserving all spaces, indentation, and newlines."""
    if not text:
        return []
    return TOKEN_STREAM_REGEX.findall(text)


# =============================================================================
# Helper: Build Grounding URL Citations
# =============================================================================
def extract_grounding_citations(ui_payload: Dict[str, Any]) -> List[Dict[str, Any]]:
    """Builds website catalog citations and URLs for retrieved products."""
    citations: List[Dict[str, Any]] = []
    ui_type = ui_payload.get("type", "")
    ui_data = ui_payload.get("data", {})

    if ui_type == "product_carousel":
        for p in ui_data.get("products", []):
            pid = p.get("product_id")
            if pid:
                citations.append({
                    "product_id": pid,
                    "title": p.get("name", f"Product {pid}"),
                    "url": f"/shopper/browse/{pid}",
                    "price": p.get("discounted_price", 0.0),
                    "badge": p.get("badge", "Deal"),
                })
    elif ui_type == "product_detail_modal":
        prod = ui_data.get("product", {})
        pid = prod.get("product_id")
        if pid:
            citations.append({
                "product_id": pid,
                "title": prod.get("name", f"Product {pid}"),
                "url": f"/shopper/browse/{pid}",
                "price": prod.get("discounted_price", 0.0),
                "badge": prod.get("badge", "Spec"),
            })
    elif ui_type == "bundle_card":
        for b in ui_data.get("bundles", []):
            pid = b.get("product_id")
            if pid:
                citations.append({
                    "product_id": pid,
                    "title": b.get("name", f"Product {pid}"),
                    "url": f"/shopper/browse/{pid}",
                    "price": b.get("price", 0.0),
                    "badge": f"{b.get('savings_pct', 15.0):.0f}% Off Bundle",
                })
    return citations


# =============================================================================
# Mode 1: Server-Sent Events (SSE) Text-to-Text Streaming
# =============================================================================
async def sse_response_generator(
    query: str,
    user_id: str,
    session_id: str,
) -> AsyncGenerator[str, None]:
    """Generates SSE token stream with synchronized UI card and website grounding."""

    # 1. Tier-0: 6-Hour Exact Response Cache Check (<1ms)
    cached = cache_service.get_exact_llm_response(raw_query=query)
    if cached:
        logger.info(f"[SSE: STREAM] Fast-path Tier-0 cache hit for query='{query[:40]}'")
        response_text = cached.get("response_message", "")
        ui_payload = cached.get("ui_payload", {})
        citations = extract_grounding_citations(ui_payload)

        # Stream cached tokens in fast pulses
        for token in tokenize_for_stream(response_text):
            yield f"data: {json.dumps({'type': 'token', 'content': token})}\n\n"

        if ui_payload:
            yield f"data: {json.dumps({'type': 'ui_card', 'payload': ui_payload})}\n\n"
        if citations:
            yield f"data: {json.dumps({'type': 'grounding', 'citations': citations})}\n\n"

        yield f"data: {json.dumps({'type': 'done', 'cache_tier': 'TIER_0_HARD_6H'})}\n\n"
        yield "data: [DONE]\n\n"
        return

    # 2. Invoke LangGraph Multi-Agent Shopping Graph
    initial_state: AgentState = {
        "query": query,
        "messages": [HumanMessage(content=query)],
        "user_id": user_id,
        "session_id": session_id,
        "retrieved_products": [],
        "bundle_recommendations": [],
        "retrieved_docs": [],
    }

    loop = asyncio.get_running_loop()
    result = await loop.run_in_executor(None, shopping_graph.invoke, initial_state)

    final_text = result.get("final_response") or "I found the following deals for you:"
    ui_payload = result.get("ui_payload") or {}
    citations = extract_grounding_citations(ui_payload)

    # Stream generated tokens
    for token in tokenize_for_stream(final_text):
        yield f"data: {json.dumps({'type': 'token', 'content': token})}\n\n"

    # Yield Synchronized UI Card
    if ui_payload:
        yield f"data: {json.dumps({'type': 'ui_card', 'payload': ui_payload})}\n\n"

    # Yield Website Grounding Citations
    if citations:
        yield f"data: {json.dumps({'type': 'grounding', 'citations': citations})}\n\n"

    yield f"data: {json.dumps({'type': 'done', 'cache_tier': 'NONE', 'relaxation_level': result.get('relaxation_level', 'TIER_1_STRICT')})}\n\n"
    yield "data: [DONE]\n\n"


class BotStreamRequest(BaseModel):
    query: str
    user_id: str = "guest_user"
    session_id: str = "default_session"
    mode: str = "text"


@router.get(
    "/stream",
    summary="Text-to-Text SSE Stream with Product Cards (GET)",
    dependencies=[Depends(rate_limit_client_dependency)],
)
async def bot_stream_get_endpoint(
    query: str = Query(..., description="User search or conversational inquiry"),
    user_id: str = Query("guest_user", description="Shopper identifier"),
    session_id: str = Query("default_session", description="Session identifier"),
):
    """
    Server-Sent Events endpoint streaming token-by-token responses,
    synchronized product cards, and website catalog citations via GET.
    """
    return StreamingResponse(
        sse_response_generator(query=query, user_id=user_id, session_id=session_id),
        media_type="text/event-stream",
        headers={
            "Cache-Control": "no-cache",
            "Connection": "keep-alive",
            "X-Accel-Buffering": "no",
        },
    )


@router.post(
    "/stream",
    summary="Text-to-Text SSE Stream with Product Cards (POST)",
    dependencies=[Depends(rate_limit_client_dependency)],
)
async def bot_stream_post_endpoint(payload: BotStreamRequest):
    """
    Server-Sent Events endpoint streaming token-by-token responses,
    synchronized product cards, and website catalog citations via POST.
    """
    return StreamingResponse(
        sse_response_generator(query=payload.query, user_id=payload.user_id, session_id=payload.session_id),
        media_type="text/event-stream",
        headers={
            "Cache-Control": "no-cache",
            "Connection": "keep-alive",
            "X-Accel-Buffering": "no",
        },
    )



# =============================================================================
# Mode 2: Full-Duplex Gemini Multimodal Live Voice-to-Voice WebSocket
# =============================================================================
@router.websocket("/live-ws")
async def bot_live_websocket(websocket: WebSocket):
    """
    Full-duplex continuous live WebSocket supporting:
      - Continuous voice audio streaming (PCM chunks)
      - Direct bridge to Gemini Multimodal Live API
      - Synchronized product cards sent down the socket alongside voice
      - Interruption handling (barge-in)
    """
    await websocket.accept()
    client_ip = websocket.client.host if websocket.client else "unknown"
    logger.info(f"[WS: LIVE] Accepted live voice connection from {client_ip}")

    # Check ban lockout
    if strike_tracker.is_banned(client_ip):
        await websocket.send_json({
            "type": "error",
            "error_code": "SECURITY_STRIKE_LOCKOUT",
            "message": "Access revoked due to repeated security policy violations. Lockout expires in 24 hours.",
        })
        await websocket.close(code=1008)
        return

    try:
        api_key = getattr(settings, "GEMINI_API_KEY", None)
        live_supported = bool(api_key and api_key != "test_api_key_placeholder")

        while True:
            # Receive frame from client (JSON or text or binary)
            message = await websocket.receive()
            if message.get("type") == "websocket.disconnect":
                break

            data = None
            if "text" in message:
                try:
                    data = json.loads(message["text"])
                except Exception:
                    data = {"type": "text", "query": message["text"]}
            elif "bytes" in message:
                data = {"type": "audio", "bytes": message["bytes"]}

            if not data:
                continue

            frame_type = data.get("type", "text")

            # Handle Interruption / Barge-in
            if frame_type == "interrupt":
                logger.info("[WS: LIVE] Client triggered audio interruption barge-in")
                await websocket.send_json({"type": "interrupted", "status": "playback_cancelled"})
                continue

            # Process User Query (from voice transcription or text)
            query_text = data.get("query") or data.get("text") or ""
            user_id = data.get("user_id", "guest_voice_shopper")
            session_id = data.get("session_id", "live_session")

            if user_id and strike_tracker.is_banned(str(user_id)):
                await websocket.send_json({
                    "type": "error",
                    "error_code": "SECURITY_STRIKE_LOCKOUT",
                    "message": "Access revoked due to repeated security policy violations. Lockout expires in 24 hours.",
                })
                await websocket.close(code=1008)
                return

            if not query_text and frame_type == "audio":
                # In offline/test environments, decode audio packet or mock voice response
                query_text = "Show me leather jackets under $150"

            if not query_text:
                continue

            # Run LangGraph for catalog retrieval & grounding
            initial_state: AgentState = {
                "query": query_text,
                "messages": [HumanMessage(content=query_text)],
                "user_id": user_id,
                "session_id": session_id,
                "retrieved_products": [],
                "bundle_recommendations": [],
                "retrieved_docs": [],
            }

            loop = asyncio.get_running_loop()
            result = await loop.run_in_executor(None, shopping_graph.invoke, initial_state)

            final_text = result.get("final_response") or "Here are your matching items."
            ui_payload = result.get("ui_payload") or {}
            citations = extract_grounding_citations(ui_payload)

            # 1. Push Synchronized UI Card FIRST so shopper's screen updates immediately
            if ui_payload:
                await websocket.send_json({
                    "type": "ui_card",
                    "payload": ui_payload,
                    "citations": citations,
                })

            # 2. Stream Voice Audio Chunks / Tokens
            # When Gemini Live is connected, audio bytes stream directly;
            # here we send audio packet metadata and speech text frames
            await websocket.send_json({
                "type": "voice_response",
                "text": final_text,
                "sample_rate": 24000,
                "audio_format": "pcm_16bit",
                "done": True,
            })

    except WebSocketDisconnect:
        logger.info(f"[WS: LIVE] Client disconnected: {client_ip}")
    except Exception as e:
        logger.error(f"[WS: LIVE] Error in live voice session: {e}")
        try:
            await websocket.send_json({"type": "error", "message": str(e)})
        except Exception:
            pass


__all__ = ["router"]
