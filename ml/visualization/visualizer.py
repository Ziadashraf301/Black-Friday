import os
import matplotlib
matplotlib.use("Agg")  # Non-interactive backend for server/CLI execution
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import pandas as pd
from typing import Dict, Any, Optional, List
from core.logging import get_logger

logger = get_logger(__name__)


class Visualizer:
    """Centralized visualization and chart generator for all pipelines, models, and analytics."""

    @staticmethod
    def generate_preprocessing_charts(
        raw_df: pd.DataFrame,
        cleaned_df: pd.DataFrame,
        outlier_bounds: Dict[str, float],
        output_dir: str = "reports/preprocessing"
    ) -> Dict[str, str]:
        """Generates visual diagnostic charts for outlier detection and target normalization across Train and Test splits."""
        os.makedirs(output_dir, exist_ok=True)
        chart_paths = {}

        has_split = "split" in cleaned_df.columns

        # 1. Outlier Distribution Plot (Histogram + Boxplot with IQR bounds)
        if has_split:
            fig, axes = plt.subplots(2, 2, figsize=(14, 8), sharex=True)
            for col_idx, split_name in enumerate(["train", "test"]):
                split_data = cleaned_df[cleaned_df["split"] == split_name]["purchase"]
                color = "#4299e1" if split_name == "train" else "#805ad5"

                sns.boxplot(x=split_data, ax=axes[0, col_idx], color=color, fliersize=3)
                axes[0, col_idx].axvline(
                    outlier_bounds["upper"],
                    color="#e53e3e",
                    linestyle="--",
                    linewidth=2,
                    label=f"IQR Upper ({outlier_bounds['upper']:.1f})"
                )
                axes[0, col_idx].set_title(f"{split_name.title()} Split: Purchase Boxplot", fontsize=12, fontweight="bold")
                axes[0, col_idx].legend(loc="upper right")

                sns.histplot(split_data, ax=axes[1, col_idx], kde=True, color=color, bins=50)
                axes[1, col_idx].axvline(outlier_bounds["upper"], color="#e53e3e", linestyle="--", linewidth=2, label="Outlier Boundary")
                axes[1, col_idx].set_xlabel("Purchase Amount ($ USD)", fontsize=11)
                axes[1, col_idx].set_ylabel("Count", fontsize=11)
                axes[1, col_idx].legend(loc="upper right")
        else:
            fig, (ax_box, ax_hist) = plt.subplots(
                2, 1, figsize=(10, 8), sharex=True, gridspec_kw={"height_ratios": [0.3, 0.7]}
            )
            sns.boxplot(x=raw_df["purchase"], ax=ax_box, color="#4299e1", fliersize=3)
            ax_box.axvline(
                outlier_bounds["upper"],
                color="#e53e3e",
                linestyle="--",
                linewidth=2,
                label=f"IQR Upper ({outlier_bounds['upper']:.1f})"
            )
            ax_box.set(xlabel="")
            ax_box.set_title("Purchase Distribution & IQR Outlier Threshold", fontsize=14, fontweight="bold")
            ax_box.legend(loc="upper right")

            sns.histplot(raw_df["purchase"], ax=ax_hist, kde=True, color="#3182ce", bins=50)
            ax_hist.axvline(outlier_bounds["upper"], color="#e53e3e", linestyle="--", linewidth=2, label="Outlier Boundary")
            ax_hist.set_xlabel("Purchase Amount ($ USD)", fontsize=12)
            ax_hist.set_ylabel("Transaction Count", fontsize=12)
            ax_hist.legend(loc="upper right")

        plt.tight_layout()
        outlier_chart_path = os.path.join(output_dir, "outlier_distribution.png")
        fig.savefig(outlier_chart_path, dpi=150)
        plt.close(fig)
        chart_paths["outlier_distribution_plot"] = outlier_chart_path

        # 2. Normalization Comparison Plot (Train vs Test)
        if has_split:
            fig, axes = plt.subplots(2, 2, figsize=(14, 8))
            for row_idx, split_name in enumerate(["train", "test"]):
                non_outliers = cleaned_df[(cleaned_df["split"] == split_name) & (~cleaned_df["is_outlier"])]
                c1 = "#48bb78" if split_name == "train" else "#38a169"
                c2 = "#38b2ac" if split_name == "train" else "#319795"

                sns.histplot(non_outliers["purchase"], ax=axes[row_idx, 0], kde=True, color=c1, bins=40)
                axes[row_idx, 0].set_title(f"{split_name.title()} Raw Purchase (Non-Outliers USD)", fontsize=11, fontweight="bold")
                axes[row_idx, 0].set_xlabel("Purchase ($)")

                sns.histplot(non_outliers["normalized_purchase"], ax=axes[row_idx, 1], kde=True, color=c2, bins=40)
                axes[row_idx, 1].set_title(f"{split_name.title()} Normalized Purchase [0, 1]", fontsize=11, fontweight="bold")
                axes[row_idx, 1].set_xlabel("Normalized Target")
        else:
            fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 5))
            sns.histplot(cleaned_df[~cleaned_df["is_outlier"]]["purchase"], ax=ax1, kde=True, color="#48bb78", bins=40)
            ax1.set_title("Raw Purchase (Non-Outliers in USD)", fontsize=12, fontweight="bold")
            ax1.set_xlabel("Purchase ($)")

            sns.histplot(cleaned_df[~cleaned_df["is_outlier"]]["normalized_purchase"], ax=ax2, kde=True, color="#38b2ac", bins=40)
            ax2.set_title("Normalized Purchase [0, 1] (Max Scaled)", fontsize=12, fontweight="bold")
            ax2.set_xlabel("Normalized Target")

        plt.tight_layout()
        norm_chart_path = os.path.join(output_dir, "normalization_distribution.png")
        fig.savefig(norm_chart_path, dpi=150)
        plt.close(fig)
        chart_paths["normalization_distribution_plot"] = norm_chart_path

        logger.info(f"Preprocessing diagnostic charts saved to: {output_dir}")
        return chart_paths

    @staticmethod
    def generate_imputation_charts(
        eval_data: Dict[str, Any],
        output_dir: str = "reports/preprocessing"
    ) -> Dict[str, str]:
        """Generates visual diagnostic charts comparing true holdout ground-truth vs imputed categories,
        producing both individual per-category charts and an overall evaluation summary."""
        os.makedirs(output_dir, exist_ok=True)
        chart_paths = {}

        # 1. Standalone Category 2 Chart
        if "y_true_cat2" in eval_data and "y_pred_cat2" in eval_data:
            fig_cat2, ax_cat2 = plt.subplots(figsize=(8, 5))
            bins = np.arange(1, 20) - 0.5
            ax_cat2.hist(eval_data["y_true_cat2"], bins=bins, alpha=0.6, label="Ground Truth", color="#3182ce", density=True)
            ax_cat2.hist(eval_data["y_pred_cat2"], bins=bins, alpha=0.6, label="Imputed (MissForest)", color="#e53e3e", density=True)
            ax_cat2.set_title("Product Category 2: True vs. Imputed (Test Holdout)", fontsize=13, fontweight="bold")
            ax_cat2.set_xlabel("Category ID")
            ax_cat2.set_ylabel("Density")
            ax_cat2.legend()
            plt.tight_layout()
            cat2_path = os.path.join(output_dir, "imputation_cat2_distribution.png")
            fig_cat2.savefig(cat2_path, dpi=150)
            plt.close(fig_cat2)
            chart_paths["imputation_category_2_plot"] = cat2_path

        # 2. Standalone Category 3 Chart
        if "y_true_cat3" in eval_data and "y_pred_cat3" in eval_data:
            fig_cat3, ax_cat3 = plt.subplots(figsize=(8, 5))
            bins = np.arange(1, 20) - 0.5
            ax_cat3.hist(eval_data["y_true_cat3"], bins=bins, alpha=0.6, label="Ground Truth", color="#38a169", density=True)
            ax_cat3.hist(eval_data["y_pred_cat3"], bins=bins, alpha=0.6, label="Imputed (MissForest)", color="#dd6b20", density=True)
            ax_cat3.set_title("Product Category 3: True vs. Imputed (Test Holdout)", fontsize=13, fontweight="bold")
            ax_cat3.set_xlabel("Category ID")
            ax_cat3.set_ylabel("Density")
            ax_cat3.legend()
            plt.tight_layout()
            cat3_path = os.path.join(output_dir, "imputation_cat3_distribution.png")
            fig_cat3.savefig(cat3_path, dpi=150)
            plt.close(fig_cat3)
            chart_paths["imputation_category_3_plot"] = cat3_path

        # 3. Overall 3-Panel Imputation Diagnostic Summary
        fig, axes = plt.subplots(1, 3, figsize=(18, 5))

        # Subplot 1: Category 2
        if "y_true_cat2" in eval_data and "y_pred_cat2" in eval_data:
            ax = axes[0]
            bins = np.arange(1, 20) - 0.5
            ax.hist(eval_data["y_true_cat2"], bins=bins, alpha=0.6, label="Ground Truth", color="#3182ce", density=True)
            ax.hist(eval_data["y_pred_cat2"], bins=bins, alpha=0.6, label="Imputed (MissForest)", color="#e53e3e", density=True)
            ax.set_title("Product Category 2: True vs. Imputed", fontsize=12, fontweight="bold")
            ax.set_xlabel("Category ID")
            ax.set_ylabel("Density")
            ax.legend()

        # Subplot 2: Category 3
        if "y_true_cat3" in eval_data and "y_pred_cat3" in eval_data:
            ax = axes[1]
            bins = np.arange(1, 20) - 0.5
            ax.hist(eval_data["y_true_cat3"], bins=bins, alpha=0.6, label="Ground Truth", color="#38a169", density=True)
            ax.hist(eval_data["y_pred_cat3"], bins=bins, alpha=0.6, label="Imputed (MissForest)", color="#dd6b20", density=True)
            ax.set_title("Product Category 3: True vs. Imputed", fontsize=12, fontweight="bold")
            ax.set_xlabel("Category ID")
            ax.legend()

        # Subplot 3: Imputation Metrics Bar Summary
        metrics = eval_data.get("metrics", {})
        ax = axes[2]
        labels = ["Cat2 Acc", "Cat3 Acc", "Mean Acc", "Cat2 NRMSE", "Cat3 NRMSE"]
        vals = [
            metrics.get("test_cat2_accuracy", 0.0),
            metrics.get("test_cat3_accuracy", 0.0),
            metrics.get("test_mean_accuracy", 0.0),
            metrics.get("test_cat2_nrmse", 0.0),
            metrics.get("test_cat3_nrmse", 0.0),
        ]
        colors = ["#3182ce", "#38a169", "#805ad5", "#e53e3e", "#dd6b20"]
        bars = ax.bar(labels, vals, color=colors, width=0.6)
        ax.set_title("MissForest Holdout Accuracy & NRMSE", fontsize=12, fontweight="bold")
        ax.set_ylim(0, 1.1)
        for bar in bars:
            height = bar.get_height()
            ax.annotate(
                f"{height:.3f}",
                xy=(bar.get_x() + bar.get_width() / 2, height),
                xytext=(0, 3),
                textcoords="offset points",
                ha="center",
                va="bottom",
                fontsize=9
            )

        plt.tight_layout()
        summary_chart_path = os.path.join(output_dir, "imputation_evaluation.png")
        fig.savefig(summary_chart_path, dpi=150)
        plt.close(fig)
        chart_paths["imputation_evaluation_plot"] = summary_chart_path

        logger.info(f"Imputation evaluation charts saved to: {output_dir}")
        return chart_paths

    @staticmethod
    def generate_model_evaluation_charts(
        y_true: np.ndarray,
        y_pred: np.ndarray,
        model_name: str,
        output_dir: str = "reports/models"
    ) -> Dict[str, str]:
        """Generates Actual vs Predicted scatter and Residual distribution plots."""
        os.makedirs(output_dir, exist_ok=True)
        artifacts = {}

        # Sample for cleaner scatter visualization
        sample_size = min(500, len(y_true))
        idx = np.random.choice(len(y_true), sample_size, replace=False)
        y_sample = y_true[idx]
        pred_sample = y_pred[idx]
        residuals = y_true - y_pred

        # 1. Actual vs Predicted Scatter Plot
        fig, ax = plt.subplots(figsize=(7, 6))
        ax.scatter(y_sample, pred_sample, alpha=0.5, color="#3182ce", edgecolors="none", s=25)
        min_val = min(y_sample.min(), pred_sample.min())
        max_val = max(y_sample.max(), pred_sample.max())
        ax.plot([min_val, max_val], [min_val, max_val], "r--", linewidth=2, label="Ideal 45° Line")
        ax.set_title(f"Actual vs Predicted — {model_name.replace('_', ' ').title()}", fontsize=13, fontweight="bold")
        ax.set_xlabel("Actual Normalized Purchase", fontsize=11)
        ax.set_ylabel("Predicted Normalized Purchase", fontsize=11)
        ax.legend()
        plt.tight_layout()

        pred_chart_path = os.path.join(output_dir, f"actual_vs_pred_{model_name}.png")
        fig.savefig(pred_chart_path, dpi=150)
        plt.close(fig)
        artifacts["actual_vs_predicted_chart"] = pred_chart_path

        # 2. Residuals Distribution Plot
        fig, ax = plt.subplots(figsize=(7, 5))
        sns.histplot(residuals, ax=ax, kde=True, color="#e53e3e", bins=40)
        ax.axvline(0, color="black", linestyle="--", linewidth=1.5)
        ax.set_title(f"Prediction Residuals — {model_name.replace('_', ' ').title()}", fontsize=13, fontweight="bold")
        ax.set_xlabel("Residual (Actual - Predicted)", fontsize=11)
        ax.set_ylabel("Frequency", fontsize=11)
        plt.tight_layout()

        res_chart_path = os.path.join(output_dir, f"residuals_{model_name}.png")
        fig.savefig(res_chart_path, dpi=150)
        plt.close(fig)
        artifacts["residuals_distribution_chart"] = res_chart_path

        logger.info(f"Model evaluation charts saved for '{model_name}' to: {output_dir}")
        return artifacts

    @staticmethod
    def generate_benchmark_comparison_chart(
        metadata: Dict[str, Any],
        output_path: str = "reports/models/model_benchmark_comparison.png"
    ) -> str:
        """Generates cross-model benchmark bar chart comparing R2 and RMSE across all trained models."""
        os.makedirs(os.path.dirname(output_path), exist_ok=True)
        model_names = list(metadata.keys())
        r2_scores = [metadata[m]["metrics"].get("test_r2", 0.0) for m in model_names]
        rmse_scores = [metadata[m]["metrics"].get("test_rmse", 0.0) for m in model_names]

        x = np.arange(len(model_names))
        width = 0.35

        fig, ax1 = plt.subplots(figsize=(9, 5))
        ax2 = ax1.twinx()

        ax1.bar(x - width/2, r2_scores, width, label='Test R² (higher is better)', color='#3182ce')
        ax2.bar(x + width/2, rmse_scores, width, label='Test RMSE (lower is better)', color='#e53e3e')

        ax1.set_ylabel('R² Score', color='#3182ce', fontsize=12, fontweight="bold")
        ax2.set_ylabel('RMSE', color='#e53e3e', fontsize=12, fontweight="bold")
        ax1.set_xticks(x)
        ax1.set_xticklabels([m.replace('_', ' ').title() for m in model_names], fontsize=11)
        ax1.set_title('Model Performance Benchmark Comparison', fontsize=14, fontweight="bold")

        ax1.set_ylim(0, max(max(r2_scores) * 1.25, 1.0))
        ax2.set_ylim(0, max(max(rmse_scores) * 1.25, 0.5))

        plt.tight_layout()
        fig.savefig(output_path, dpi=150)
        plt.close(fig)
        logger.info(f"Benchmark comparison chart saved to: {output_path}")
        return output_path

    @staticmethod
    def generate_shap_summary_plot(
        shap_values: Any,
        eval_features: pd.DataFrame,
        feature_names: Optional[List[str]] = None,
        output_dir: str = "reports/shap"
    ) -> Optional[str]:
        """Generates and saves SHAP summary beeswarm plot as PNG artifact."""
        try:
            import shap
        except ImportError:
            logger.warning("SHAP library is not installed.")
            return None

        if shap_values is None:
            return None

        os.makedirs(output_dir, exist_ok=True)
        out_path = os.path.join(output_dir, "shap_summary_plot.png")

        try:
            # If an explainer instance was passed instead of values, compute shap_values
            if hasattr(shap_values, "shap_values"):
                shap_values = shap_values.shap_values(eval_features)

            # Validate feature_names length matches transformed feature matrix columns
            n_cols = eval_features.shape[1] if hasattr(eval_features, "shape") and len(eval_features.shape) > 1 else None
            if feature_names is not None and n_cols is not None and len(feature_names) != n_cols:
                logger.info(f"SHAP feature_names count ({len(feature_names)}) != transformed matrix columns ({n_cols}); using auto indexing.")
                feature_names = None

            plt.figure(figsize=(10, 6))
            shap.summary_plot(shap_values, eval_features, feature_names=feature_names, show=False)
            plt.tight_layout()
            plt.savefig(out_path, dpi=300)
            plt.close()
            logger.info(f"SHAP summary plot saved to: {out_path}")
            return out_path
        except Exception as e:
            logger.error(f"Failed to generate SHAP summary plot: {e}", exc_info=True)
            return None

    @staticmethod
    def generate_segmentation_charts(
        segmented_df: pd.DataFrame,
        stats_df: Optional[pd.DataFrame] = None,
        output_dir: str = "reports/segmentation"
    ) -> Dict[str, str]:
        """Generates visual persona distribution, LTV profiling, and cluster statistics table charts for Customer Segmentation."""
        os.makedirs(output_dir, exist_ok=True)
        charts = {}

        if "cluster_persona" not in segmented_df.columns:
            return charts

        # 1. Persona Distribution Bar Chart
        fig, ax = plt.subplots(figsize=(10, 6))
        persona_counts = segmented_df["cluster_persona"].value_counts(ascending=True)
        colors = plt.cm.viridis(np.linspace(0.2, 0.8, len(persona_counts)))

        bars = ax.barh(persona_counts.index, persona_counts.values, color=colors)
        ax.set_title("Customer Personas Distribution (Gower Hierarchical Clustering)", fontsize=13, fontweight="bold")
        ax.set_xlabel("Number of Customers", fontsize=11)

        for bar in bars:
            width = bar.get_width()
            pct = (width / max(len(segmented_df), 1)) * 100
            ax.text(width + max(persona_counts.values)*0.01, bar.get_y() + bar.get_height()/2, f"{int(width):,} ({pct:.1f}%)", va="center", fontsize=9)

        plt.tight_layout()
        dist_path = os.path.join(output_dir, "customer_personas_distribution.png")
        fig.savefig(dist_path, dpi=150)
        plt.close(fig)
        charts["persona_distribution_chart"] = dist_path

        # 2. Persona Spending Profiling (LTV & Frequency)
        if "lifetime_value" in segmented_df.columns and "frequency" in segmented_df.columns:
            profile_df = segmented_df.groupby("cluster_persona")[["lifetime_value", "frequency"]].mean()
            fig, ax1 = plt.subplots(figsize=(11, 6))
            ax2 = ax1.twinx()

            x = np.arange(len(profile_df))
            width = 0.35

            ax1.bar(x - width/2, profile_df["lifetime_value"], width, label="Mean LTV ($)", color="#4299e1")
            ax2.bar(x + width/2, profile_df["frequency"], width, label="Mean Orders Frequency", color="#ed8936")

            ax1.set_ylabel("Mean Lifetime Value ($)", color="#4299e1", fontsize=11, fontweight="bold")
            ax2.set_ylabel("Mean Order Frequency", color="#ed8936", fontsize=11, fontweight="bold")
            ax1.set_xticks(x)
            ax1.set_xticklabels(profile_df.index, rotation=35, ha="right", fontsize=9)
            ax1.set_title("Customer Personas: Spending LTV & Order Frequency Profiling", fontsize=13, fontweight="bold")

            plt.tight_layout()
            prof_path = os.path.join(output_dir, "customer_personas_profiling.png")
            fig.savefig(prof_path, dpi=150)
            plt.close(fig)
            charts["persona_profiling_chart"] = prof_path

        # 3. Cluster Statistics Summary Table Chart
        if stats_df is not None and not stats_df.empty:
            fig, ax = plt.subplots(figsize=(16, max(5, len(stats_df) * 0.6 + 1.5)))
            ax.axis("off")

            display_cols = [c for c in stats_df.columns if c not in ["recommended_action"]]
            headers = [c.replace("_", " ").title() for c in display_cols]
            cell_text = stats_df[display_cols].astype(str).values.tolist()

            table = ax.table(
                cellText=cell_text,
                colLabels=headers,
                loc="center",
                cellLoc="center"
            )
            table.auto_set_font_size(False)
            table.set_fontsize(8)
            table.scale(1.1, 1.5)

            for (r, c), cell in table.get_celld().items():
                if r == 0:
                    cell.set_facecolor("#3182ce")
                    cell.set_text_props(color="white", weight="bold", fontsize=8.5)
                else:
                    cell.set_facecolor("#f7fafc" if r % 2 == 0 else "#ffffff")

            ax.set_title("Cluster Statistics per Persona (Mean Numeric Features & Mode Categorical Features)", fontsize=13, fontweight="bold", pad=15)
            plt.tight_layout()
            table_chart_path = os.path.join(output_dir, "cluster_statistics_table.png")
            fig.savefig(table_chart_path, dpi=200, bbox_inches="tight")
            plt.close(fig)
            charts["cluster_statistics_table"] = table_chart_path

        logger.info(f"Customer segmentation diagnostic charts saved to: {output_dir}")
        return charts


    @staticmethod
    def generate_market_basket_charts(
        rules_df: pd.DataFrame,
        metrics_df: pd.DataFrame,
        item2vec: Any = None,
        output_dir: str = "reports/market_basket"
    ) -> Dict[str, str]:
        """Generates Apriori association rules scatter plot, Product Network centrality leaders chart, and Item2Vec embedding similarity distribution."""
        os.makedirs(output_dir, exist_ok=True)
        charts = {}

        # 1. Apriori Association Rules Scatter Plot
        if rules_df is not None and not rules_df.empty:
            fig, ax = plt.subplots(figsize=(8, 6))
            scatter = ax.scatter(
                rules_df["support"],
                rules_df["confidence"],
                c=rules_df["lift"],
                cmap="YlOrRd",
                alpha=0.75,
                s=60,
                edgecolors="none"
            )
            cbar = plt.colorbar(scatter, ax=ax)
            cbar.set_label("Lift Factor", fontsize=11)
            ax.set_title("Mined Apriori Association Rules (Support vs Confidence)", fontsize=13, fontweight="bold")
            ax.set_xlabel("Support", fontsize=11)
            ax.set_ylabel("Confidence", fontsize=11)
            plt.tight_layout()

            rules_chart_path = os.path.join(output_dir, "apriori_rules_scatter.png")
            fig.savefig(rules_chart_path, dpi=150)
            plt.close(fig)
            charts["apriori_rules_chart"] = rules_chart_path

        # 2. Product Network Centrality Leaders Chart (PageRank, Hubs, Authorities)
        pr_col = "pagerank_score" if "pagerank_score" in metrics_df.columns else ("pagerank" if "pagerank" in metrics_df.columns else None)
        hub_col = "hub_score" if "hub_score" in metrics_df.columns else ("hubs" if "hubs" in metrics_df.columns else None)
        auth_col = "authority_score" if "authority_score" in metrics_df.columns else ("authorities" if "authorities" in metrics_df.columns else None)

        if metrics_df is not None and not metrics_df.empty and pr_col:
            top_nodes = metrics_df.sort_values(by=pr_col, ascending=False).head(10)
            fig, ax = plt.subplots(figsize=(10, 5))
            bars = ax.bar(top_nodes["product_id"].astype(str), top_nodes[pr_col], color="#319795")
            ax.set_title("Top 10 Product Network Centrality Leaders (PageRank)", fontsize=13, fontweight="bold")
            ax.set_xlabel("Product ID", fontsize=11)
            ax.set_ylabel("PageRank Score", fontsize=11)
            plt.xticks(rotation=45, ha="right")

            for bar in bars:
                height = bar.get_height()
                ax.annotate(f"{height:.4f}", xy=(bar.get_x() + bar.get_width()/2, height),
                            xytext=(0, 3), textcoords="offset points", ha="center", va="bottom", fontsize=8)

            plt.tight_layout()
            net_chart_path = os.path.join(output_dir, "product_network_centrality.png")
            fig.savefig(net_chart_path, dpi=150)
            plt.close(fig)
            charts["product_centrality_chart"] = net_chart_path

        # 3. HITS Hubs & Authority Leaders Chart
        if metrics_df is not None and not metrics_df.empty and hub_col and auth_col:
            top_hubs = metrics_df.sort_values(by=hub_col, ascending=False).head(8)
            top_auths = metrics_df.sort_values(by=auth_col, ascending=False).head(8)

            fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 5))
            ax1.bar(top_hubs["product_id"].astype(str), top_hubs[hub_col], color="#dd6b20")
            ax1.set_title("Top Product Hubs (Connectors)", fontsize=12, fontweight="bold")
            ax1.set_xlabel("Product ID")
            ax1.set_ylabel("Hub Score")
            ax1.tick_params(axis="x", rotation=45)

            ax2.bar(top_auths["product_id"].astype(str), top_auths[auth_col], color="#3182ce")
            ax2.set_title("Top Product Authorities (Key Destinations)", fontsize=12, fontweight="bold")
            ax2.set_xlabel("Product ID")
            ax2.set_ylabel("Authority Score")
            ax2.tick_params(axis="x", rotation=45)

            plt.tight_layout()
            hits_chart_path = os.path.join(output_dir, "product_network_hubs_authorities.png")
            fig.savefig(hits_chart_path, dpi=150)
            plt.close(fig)
            charts["product_hubs_authorities_chart"] = hits_chart_path

        # 4. Item2Vec Cosine Similarity Distribution Chart
        if item2vec is not None and hasattr(item2vec, "get_all_similar_products") and hasattr(item2vec, "vocabulary"):
            sim_scores = []
            for pid in item2vec.vocabulary[:200]:  # Sample first 200 items for speed
                sims = item2vec.get_similar_products(pid, top_n=5)
                sim_scores.extend([s for _, s in sims])

            if sim_scores:
                fig, ax = plt.subplots(figsize=(8, 5))
                sns.histplot(sim_scores, ax=ax, kde=True, color="#805ad5", bins=30)
                mean_sim = float(np.mean(sim_scores))
                ax.axvline(mean_sim, color="#e53e3e", linestyle="--", linewidth=2, label=f"Mean Similarity ({mean_sim:.3f})")
                ax.set_title("Item2Vec Top-5 Cosine Similarity Distribution", fontsize=13, fontweight="bold")
                ax.set_xlabel("Cosine Similarity", fontsize=11)
                ax.set_ylabel("Frequency", fontsize=11)
                ax.legend()
                plt.tight_layout()

                i2v_chart_path = os.path.join(output_dir, "item2vec_similarity_distribution.png")
                fig.savefig(i2v_chart_path, dpi=150)
                plt.close(fig)
                charts["item2vec_similarity_chart"] = i2v_chart_path

        logger.info(f"Market basket & product network diagnostic charts saved to: {output_dir}")
        return charts


