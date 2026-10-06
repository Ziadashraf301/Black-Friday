"""
Semantic System-1 Entity Extractor Prompt Specifications.
"""
from ai.schemas import PromptVersion, QuestionSpec

# V1 Entity Extractor: Baseline Department, Budget, Size, and Product ID
V1_EXTRACTOR_PROMPT = PromptVersion(
    version="v1.0.0-extractor",
    description="Semantic System-1 Entity Extractor Questions for Department, Budget, Size, and Product ID",
    author="Antigravity MLOps",
    created_at="2026-10-05T10:00:00Z",
    questions={
        "target_department": QuestionSpec(
            question_type="choice",
            instructions="What is the primary apparel department or product category referenced?",
            criteria={
                "Outerwear": "Jackets, coats, bombers, trenches, outerwear, peacoats",
                "Knitwear": "Sweaters, cardigans, mocknecks, wool knits",
                "Tops": "Shirts, blouses, silk kimonos, tunics, polos",
                "Dresses": "Dresses, skirts, pinafores",
                "Bottoms": "Pants, trousers, denim, jeans",
                "Footwear": "Boots, shoes, sneakers, loafers, footwear",
                "None": "No specific category or non-shopping query",
            },
        ),
        "budget_limit": QuestionSpec(
            question_type="choice",
            instructions="What upper price/budget limit is requested by the user, if any?",
            criteria={
                "50": "Under or up to $50",
                "60": "Under or up to $60",
                "80": "Under or up to $80",
                "100": "Under or up to $100",
                "150": "Under or up to $150",
                "200": "Under or up to $200",
                "None": "No budget or price limit mentioned",
            },
        ),
        "apparel_size": QuestionSpec(
            question_type="choice",
            instructions="What apparel, waist, or shoe size is requested by the user?",
            criteria={
                "XS": "Extra Small (XS)",
                "S": "Small (S)",
                "M": "Medium (M)",
                "L": "Large (L)",
                "XL": "Extra Large (XL)",
                "XXL": "Double Extra Large (XXL)",
                "32": "Waist size 32",
                "10": "Shoe size 10",
                "None": "No size specified",
            },
        ),
        "product_mention": QuestionSpec(
            question_type="choice",
            instructions="Does the query reference a specific catalog product or ID?",
            criteria={
                "P00025442": "Artisan Paisley Silk Kimono Shirt or P00025442",
                "P00057642": "Heritage Pleated Pinafore Dress or P00057642",
                "P00110742": "Distressed Saddle Brown Aviator Bomber or P00110742",
                "P00145042": "Maritime Navy Heavy Wool Peacoat or P00145042",
                "P00031042": "Chelsea Leather Boot or P00031042",
                "None": "No specific catalog product code referenced",
            },
        ),
    },
)

# V2 Multi-Class Semantic Category Extractor covering all 17 catalog categories
V2_EXTRACTOR_PROMPT = PromptVersion(
    version="v2.0.0-extractor",
    description="V2 Multi-Class Semantic Category Extractor covering all 17 catalog categories with calibrated softmax probabilities",
    author="Antigravity MLOps",
    created_at="2026-10-05T12:50:00Z",
    questions={
        "target_category": QuestionSpec(
            question_type="choice",
            instructions="What is the primary apparel category or product department referenced in the user query?",
            criteria={
                "Silks & Kimonos": "Silk kimono shirts, silk robes, bohemian tunics, paisley silk blouses",
                "Coats & Trenches": "Heavy wool peacoats, military utility trenches, field explorer parkas, trench coats",
                "Jackets & Outerwear": "Bomber jackets, trucker jackets, retro varsity jackets, canvas parkas",
                "Leather & Outerwear": "Aviator leather bombers, distressed leather jackets, sheepskin outerwear",
                "Jackets & Blazers": "Evening blazers, velvet blazers, formal dinner jackets",
                "Knitwear & Sweaters": "Aran cable knit cardigans, wool sweaters, Nordic Fair Isle knits, mockneck sweaters",
                "Tops & Tunics": "Embroidered velvet tunics, boho peasant blouses, statement tops",
                "Shirts & Blouses": "Collared shirts, button-downs, casual blouses",
                "Shirts & Polos": "Polo shirts, casual short-sleeve shirts",
                "Dresses & Skirts": "Pleated pinafore dresses, midi wrap skirts, evening dresses",
                "Dresses & Jumpsuits": "One-piece jumpsuits, overalls, full-length formal dresses",
                "Pants & Trousers": "High-waist trousers, corduroy pants, tailored slacks, trouser sets",
                "Denim & Jeans": "Raw selvedge denim jeans, relaxed fit denim, vintage wash jeans",
                "Footwear & Boots": "Chelsea leather boots, rugged ankle boots, desert boots, leather footwear",
                "Footwear": "Loafers, sneakers, dress shoes, walking footwear",
                "Activewear & Loungewear": "Sweatpants, hoodies, relaxed loungewear, athletic apparel",
                "Accessories": "Belts, scarves, hats, leather accessories",
                "None": "No specific clothing item or non-shopping query",
            },
        ),
    },
)

__all__ = ["V1_EXTRACTOR_PROMPT", "V2_EXTRACTOR_PROMPT"]
