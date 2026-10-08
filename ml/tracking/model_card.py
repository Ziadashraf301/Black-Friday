import os
import datetime
from typing import Dict, Any, Optional
from ml.models.regression import ALL_FEATURES
from core.logging import get_logger

logger = get_logger(__name__)


class ModelCardGenerator:
    """Generates standardized Model & Data Cards for ML Governance."""

    @classmethod
    def generate_model_card(
        cls,
        model_name: str,
        metrics: Dict[str, Any],
        fairness_metrics: Dict[str, Any],
        output_filepath: Optional[str] = None
    ) -> str:
        """Assembles and writes a comprehensive markdown Model Card."""
        date_str = datetime.datetime.now().strftime("%Y-%m-%d")
        if output_filepath is None:
            safe_name = model_name.lower().replace(" ", "_")
            output_filepath = os.path.join("reports", "model_cards", f"MODEL_CARD_{safe_name}_{date_str}.md")

        features_formatted = ", ".join(f"`{f}`" for f in ALL_FEATURES)

        content = f"""# Model Card: {model_name}

## 1. Model Overview
- **Model Architecture:** {model_name}
- **Task:** Retail Purchase Amount Regression (Normalized USD)
- **Input Features:** {features_formatted}
- **Output:** Normalized purchase price in range `[0, 1]` (scaled by `Purchase_max = 21,399`)
- **Serving Engine:** ONNX Runtime (CPU Execution Provider)
- **Date Created:** {date_str}

## 2. Performance Summary
| Metric | Value |
| :--- | :--- |
| **Test RMSE** | `{metrics.get('test_rmse', 'N/A')}` |
| **Test R² Score** | `{metrics.get('test_r2', 'N/A')}` |
| **10-Fold CV Mean RMSE** | `{metrics.get('cv_mean_rmse', 'N/A')}` |
| **10-Fold CV Mean R²** | `{metrics.get('cv_mean_r2', 'N/A')}` |

## 3. Demographic Subgroup Fairness Evaluation
Evaluating performance parity across customer demographic slices to verify absence of systemic bias.

### Gender Breakdown
"""
        for g, data in fairness_metrics.get("gender_slices", {}).items():
            content += f"- **Gender `{g}`** ({data['sample_size']:,} samples): RMSE = `{data['rmse']}`, R² = `{data['r2']}`\n"

        content += "\n### Age Group Breakdown\n"
        for a, data in fairness_metrics.get("age_slices", {}).items():
            content += f"- **Age `{a}`** ({data['sample_size']:,} samples): RMSE = `{data['rmse']}`, R² = `{data['r2']}`\n"

        content += """
## 4. Intended Use & Limitations
- **Primary Use:** Promotional pricing estimation, what-if product margin analysis, and customer campaign tailoring for Black Friday sales.
- **Outlier Policy:** Excludes top 0.4% transactions (>$21,400.50, predominantly Category 10) to protect linear and tree stability.
- **Ethical Considerations:** Features do not include personally identifiable information (PII). All user IDs are masked integers.
"""
        os.makedirs(os.path.dirname(output_filepath) if os.path.dirname(output_filepath) else ".", exist_ok=True)
        with open(output_filepath, "w") as f:
            f.write(content)

        logger.info(f"Model Card successfully generated at: {output_filepath}")
        return output_filepath
