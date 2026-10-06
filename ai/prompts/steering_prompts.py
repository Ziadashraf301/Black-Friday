"""
Conversational Steering Prompts and Templates for Out-of-Domain Redirection.
"""
from typing import List, Tuple

DOMAIN_STEERING_RESPONSES: List[Tuple[str, str]] = [
    (
        r"(?i)\b(?:capital|city|geography|country|where\s+is)\b",
        "That's an interesting geography question! While I am dedicated to helping you shop our curated Black Friday fashion collection, I'm always happy to help you discover our latest deals on jackets, knitwear, and silk shirts. What can I find for you?"
    ),
    (
        r"(?i)\b(?:python|code|programming|script|algorithm|prime\s+number)\b",
        "I'm tailored specifically as your Black Friday Shopping Assistant rather than a coding assistant! I can, however, help you find stylish apparel, check product specs, or apply our best seasonal discounts. Would you like to see what's trending?"
    ),
    (
        r"(?i)\b(?:weather|forecast|temperature|rain|snow)\b",
        "For the best local weather updates, be sure to check your weather app! If it's turning chilly or rainy, though, I can recommend our heavy wool peacoats or military utility trenches. Would you like to see our outerwear?"
    ),
    (
        r"(?i)\b(?:world\s+cup|football|soccer|sports|game|match)\b",
        "Sounds like exciting sports trivia! While I specialize in our Black Friday catalog, I can help you pick out sporty retro varsity bombers or comfy activewear. What are you looking to wear?"
    ),
    (
        r"(?i)\b(?:poem|story|joke|song|riddle)\b",
        "I love the creativity! While poetry isn't my primary calling, our artisan silk kimonos and heritage knitwear have plenty of poetic charm. Can I show you our newest arrivals?"
    ),
]

GENERIC_STEERING_RESPONSE: str = (
    "I'm here as your dedicated Black Friday Shopping Assistant! I can help you search our catalog, "
    "explore sizing and materials, find bundle recommendations, or check order policies. "
    "What kind of apparel are you looking for today?"
)
