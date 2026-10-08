# Fix Report: Work Package 4 (ML Quick Bug Fixes & Evaluation Layering)

> **Work Package**: WP4  
> **Status**: COMPLETED  
> **Branch**: `review-fixes`  
> **Date**: 2026-10-08  

---

## 1. Summary of Remediated Findings

| Fix ID | Domain & Component | Status | Summary of Remediation |
| :--- | :--- | :--- | :--- |
| **Fix 2.4** | `ml/pipelines/retrain.py` | **DONE** | Inverted gate from subtraction to addition: `candidate_r2 >= (champion_r2 + min_improvement_delta)`. Confirmed metric is R² (higher is better) and `min_improvement_delta` is positive threshold. Updated rejection reason. |
| **Fix 2.5** | `ml/models/__init__.py` | **DONE** | Exported `ModelRegistry` (and `ModelEvaluator`) in `ml/models/__init__.py` and `__all__`. |
| **Fix 3.4** | `ml/features/data_contract.py` | **DONE** | Added `stay_in_current_city_years` validation column (`Check.isin(["0", "1", "2", "3", "4+"])`) to both `RawTransactionSchema` and `CleanedTransactionSchema`. Real data produces strings `"0"`, `"1"`, `"2"`, `"3"`, `"4+"`. |
| **Fix 3.5** | `ml/pipelines/train.py` | **DONE** | Wired CLI `--model` arguments through `ModelRegistry.resolve_name()`. Unknown models cleanly raise `ValueError` specifying available models. |
| **Fix 5.5** | `ml/pipelines/monitor.py` | **DONE** | Ground-truth check inspects normalized evaluation dataframe (`curr_eval`) and raw `current_batch_df["purchase"]`. Sets `has_target = True` when labels exist to trigger supervised continuous retraining. |
| **Fix 8.6** | `tests/test_features.py` | **DONE** | Replaced generic `pytest.raises(Exception)` with explicit `pytest.raises(SchemaError)` from Pandera. |
| **Fix 10.3** | `ml/tracking/model_card.py` | **DONE** | Dynamically derived all 10 model features from `ml.models.regression.ALL_FEATURES` in the model card generator template markdown. |
| **Fix 10.4** | `ml/pipelines/cron_scheduler.py` | **DONE** | Removed inaccurate docstrings claiming 6-hour cache flushes/invalidation; documented distributed task queue transition note (Celery / ARQ / APScheduler) for production HA deployments. |
| **Fix 4.2 (Dedup)** | `ml/models/regression.py` | **DONE** | Extracted duplicate `ColumnTransformer` from `DecisionTreeModel`, `RandomForestModel`, and `LightGBMModel` into reusable factory `create_tree_preprocessor()` with identical behavior. (Occupation reclassification deferred to WP11 as instructed). |
| **Extra (Layering)** | `ml/models/metrics.py` & `evaluation/` | **DONE** | Extracted core metric computation and `ModelEvaluator` into `ml/models/metrics.py`. Repointed `ml/pipelines/{preprocess,train,retrain}.py`. `evaluation/ml/evaluate.py` imports from `ml.models.metrics` for CLI/reporting. Emptied `ALLOW_LISTED_VIOLATIONS` in `tests/test_architecture.py`. |

---

## 2. Verification Evidence

All 64 WP4-related unit, regression, and architectural boundary tests passed:

```
tests/test_architecture.py::test_dependency_rules_and_mlflow_isolation PASSED [  1%]
tests/test_fix_2_4_champion_gate.py::test_champion_challenger_promotion_gate_table[candidate_worse-0.68-0.7-0.005-0.6-False] PASSED [  3%]
tests/test_fix_2_4_champion_gate.py::test_champion_challenger_promotion_gate_table[candidate_equal-0.7-0.7-0.005-0.6-False] PASSED [  4%]
tests/test_fix_2_4_champion_gate.py::test_champion_challenger_promotion_gate_table[candidate_better_less_than_delta-0.703-0.7-0.005-0.6-False] PASSED [  6%]
tests/test_fix_2_4_champion_gate.py::test_champion_challenger_promotion_gate_table[candidate_better_exactly_delta-0.705-0.7-0.005-0.6-True] PASSED [  7%]
tests/test_fix_2_4_champion_gate.py::test_champion_challenger_promotion_gate_table[candidate_better_more_than_delta-0.715-0.7-0.005-0.6-True] PASSED [  9%]
tests/test_fix_2_4_champion_gate.py::test_champion_challenger_promotion_gate_table[candidate_below_absolute_threshold-0.55-0.5-0.005-0.6-False] PASSED [ 10%]
tests/test_fix_2_4_champion_gate.py::test_champion_challenger_promotion_gate_table[candidate_below_threshold_even_if_beats_champion-0.58-0.5-0.005-0.6-False] PASSED [ 12%]
tests/test_fix_2_4_champion_gate.py::test_champion_challenger_promotion_gate_table[candidate_above_threshold_and_sufficient_delta-0.75-0.72-0.01-0.65-True] PASSED [ 14%]
tests/test_fix_2_4_champion_gate.py::test_old_buggy_logic_would_improperly_promote_inferior_candidate PASSED [ 15%]
tests/test_fix_2_5_model_registry_export.py::test_model_registry_imported_from_ml_models PASSED [ 17%]
tests/test_fix_2_5_model_registry_export.py::test_model_registry_in_ml_models_all PASSED [ 18%]
tests/test_fix_3_4_data_contract_stay_years.py::test_valid_stay_years_accepted_in_raw_contract[0] PASSED [ 20%]
tests/test_fix_3_4_data_contract_stay_years.py::test_valid_stay_years_accepted_in_raw_contract[1] PASSED [ 21%]
tests/test_fix_3_4_data_contract_stay_years.py::test_valid_stay_years_accepted_in_raw_contract[2] PASSED [ 23%]
tests/test_fix_3_4_data_contract_stay_years.py::test_valid_stay_years_accepted_in_raw_contract[3] PASSED [ 25%]
tests/test_fix_3_4_data_contract_stay_years.py::test_valid_stay_years_accepted_in_raw_contract[4+] PASSED [ 26%]
tests/test_fix_3_4_data_contract_stay_years.py::test_valid_stay_years_accepted_in_cleaned_contract[0] PASSED [ 28%]
tests/test_fix_3_4_data_contract_stay_years.py::test_valid_stay_years_accepted_in_cleaned_contract[1] PASSED [ 29%]
tests/test_fix_3_4_data_contract_stay_years.py::test_valid_stay_years_accepted_in_cleaned_contract[2] PASSED [ 31%]
tests/test_fix_3_4_data_contract_stay_years.py::test_valid_stay_years_accepted_in_cleaned_contract[3] PASSED [ 32%]
tests/test_fix_3_4_data_contract_stay_years.py::test_valid_stay_years_accepted_in_cleaned_contract[4+] PASSED [ 34%]
tests/test_fix_3_4_data_contract_stay_years.py::test_invalid_stay_years_rejected_in_raw_contract[5] PASSED [ 35%]
tests/test_fix_3_4_data_contract_stay_years.py::test_invalid_stay_years_rejected_in_raw_contract[4] PASSED [ 37%]
tests/test_fix_3_4_data_contract_stay_years.py::test_invalid_stay_years_rejected_in_raw_contract[10] PASSED [ 39%]
tests/test_fix_3_4_data_contract_stay_years.py::test_invalid_stay_years_rejected_in_raw_contract[unknown] PASSED [ 40%]
tests/test_fix_3_4_data_contract_stay_years.py::test_invalid_stay_years_rejected_in_raw_contract[-1] PASSED [ 42%]
tests/test_fix_3_4_data_contract_stay_years.py::test_invalid_stay_years_rejected_in_raw_contract[years] PASSED [ 43%]
tests/test_fix_3_4_data_contract_stay_years.py::test_invalid_stay_years_rejected_in_cleaned_contract[5] PASSED [ 45%]
tests/test_fix_3_4_data_contract_stay_years.py::test_invalid_stay_years_rejected_in_cleaned_contract[4] PASSED [ 46%]
tests/test_fix_3_4_data_contract_stay_years.py::test_invalid_stay_years_rejected_in_cleaned_contract[10] PASSED [ 48%]
tests/test_fix_3_4_data_contract_stay_years.py::test_invalid_stay_years_rejected_in_cleaned_contract[unknown] PASSED [ 50%]
tests/test_fix_3_4_data_contract_stay_years.py::test_invalid_stay_years_rejected_in_cleaned_contract[-1] PASSED [ 51%]
tests/test_fix_3_4_data_contract_stay_years.py::test_invalid_stay_years_rejected_in_cleaned_contract[years] PASSED [ 53%]
tests/test_fix_3_5_train_aliases.py::test_model_alias_resolution[lgbm-lightgbm] PASSED [ 54%]
tests/test_fix_3_5_train_aliases.py::test_model_alias_resolution[rf-random_forest] PASSED [ 56%]
tests/test_fix_3_5_train_aliases.py::test_model_alias_resolution[dt-decision_tree] PASSED [ 57%]
tests/test_fix_3_5_train_aliases.py::test_model_alias_resolution[lr-linear_regression] PASSED [ 59%]
tests/test_fix_3_5_train_aliases.py::test_model_alias_resolution[LGBM-lightgbm] PASSED [ 60%]
tests/test_fix_3_5_train_aliases.py::test_model_alias_resolution[RF-random_forest] PASSED [ 62%]
tests/test_fix_3_5_train_aliases.py::test_model_alias_resolution[DT-decision_tree] PASSED [ 64%]
tests/test_fix_3_5_train_aliases.py::test_model_alias_resolution[LR-linear_regression] PASSED [ 65%]
tests/test_fix_3_5_train_aliases.py::test_model_alias_resolution[lightgbm-lightgbm] PASSED [ 67%]
tests/test_fix_3_5_train_aliases.py::test_model_alias_resolution[random_forest-random_forest] PASSED [ 68%]
tests/test_fix_3_5_train_aliases.py::test_model_alias_resolution[decision_tree-decision_tree] PASSED [ 70%]
tests/test_fix_3_5_train_aliases.py::test_model_alias_resolution[linear_regression-linear_regression] PASSED [ 71%]
tests/test_fix_3_5_train_aliases.py::test_unknown_model_name_raises_clean_error PASSED [ 73%]
tests/test_fix_3_5_train_aliases.py::test_train_pipeline_model_resolution PASSED [ 75%]
tests/test_fix_4_2_preprocessor_dedup.py::test_create_tree_preprocessor_returns_columntransformer PASSED [ 76%]
tests/test_fix_4_2_preprocessor_dedup.py::test_tree_models_use_identical_preprocessor_configuration PASSED [ 78%]
tests/test_fix_5_5_monitor_target.py::test_batch_containing_purchase_sets_has_target_true PASSED [ 79%]
tests/test_fix_5_5_monitor_target.py::test_batch_containing_normalized_purchase_sets_has_target_true PASSED [ 81%]
tests/test_fix_5_5_monitor_target.py::test_batch_without_target_sets_has_target_false PASSED [ 82%]
tests/test_fix_5_5_monitor_target.py::test_batch_with_all_nan_purchase_sets_has_target_false PASSED [ 84%]
tests/test_features.py::test_preprocessor_outlier_bounds PASSED          [ 85%]
tests/test_features.py::test_preprocessor_normalization PASSED           [ 87%]
tests/test_features.py::test_customer_feature_extractor PASSED           [ 89%]
tests/test_features.py::test_cleaned_data_contract_with_split PASSED     [ 90%]
tests/test_features.py::test_onnx_imputer_export_and_inference PASSED    [ 92%]
tests/test_fix_10_3_model_card_features.py::test_model_card_generator_documents_all_10_features PASSED [ 93%]
tests/test_evaluation_layering.py::test_evaluate_holdout_identical_outputs PASSED [ 95%]
tests/test_evaluation_layering.py::test_evaluate_imputation_identical_outputs PASSED [ 96%]
tests/test_evaluation_layering.py::test_evaluate_hypothesis_welch_identical_outputs PASSED [ 98%]
tests/test_evaluation_layering.py::test_model_evaluator_class_parity PASSED [100%]
======================= 64 passed, 3 warnings in 46.42s =======================
```

CLI Help Verifications:
- `python -m ml.pipelines.train --help`: Exit 0 (shows `--model` flag).
- `python -m ml.pipelines.retrain --help`: Exit 0 (shows `--new-data`, `--old-data`, `--min-improvement`, `--min-r2`).
- `python -m ml.pipelines.monitor --help`: Exit 0 (shows `--batch`, `--reference`, `--threshold`, `--no-retrain`).

---

## 3. Files Changed / Created

### New Files Created
- `ml/models/metrics.py` (decoupled metric computation and `ModelEvaluator`)
- `tests/test_fix_2_4_champion_gate.py`
- `tests/test_fix_2_5_model_registry_export.py`
- `tests/test_fix_3_4_data_contract_stay_years.py`
- `tests/test_fix_3_5_train_aliases.py`
- `tests/test_fix_4_2_preprocessor_dedup.py`
- `tests/test_fix_5_5_monitor_target.py`
- `tests/test_fix_10_3_model_card_features.py`
- `tests/test_evaluation_layering.py`

### Modified Files
- `ml/pipelines/retrain.py` (champion gate formula fixed, imports updated)
- `ml/models/__init__.py` (exported `ModelRegistry` and `ModelEvaluator`)
- `ml/features/data_contract.py` (added `stay_in_current_city_years` validation)
- `ml/pipelines/train.py` (alias resolution & error handling, imports updated)
- `ml/pipelines/monitor.py` (ground-truth target detection in batch/evaluation frame)
- `ml/pipelines/preprocess.py` (updated import to `ml.models.metrics`)
- `ml/models/regression.py` (`create_tree_preprocessor` factory extracted)
- `ml/tracking/model_card.py` (derived all 10 features from `ALL_FEATURES`)
- `ml/pipelines/cron_scheduler.py` (docstring clean-up)
- `evaluation/ml/evaluate.py` (imports metrics from `ml.models.metrics`, maintains CLI)
- `tests/test_architecture.py` (removed 3 allow-list entries; `ALLOW_LISTED_VIOLATIONS = set()`)
- `tests/test_features.py` (asserts `SchemaError`)
- `RESTRUCTURE_MAP.md` (documented `ml/models/metrics.py`)

---

## 4. Decisions
- **Metric Direction (2.4)**: Metric is R² (higher is better). Promotion requirement is `candidate_r2 >= champion_r2 + min_improvement_delta`.
- **Stay Years Validation (3.4)**: Validated values against real dataset (`data/train.csv` and `data/test.csv`), confirming strings `"0"`, `"1"`, `"2"`, `"3"`, `"4+"`.
- **Deduplication vs Retraining (4.2)**: As instructed, only the `ColumnTransformer` deduplication was implemented. Occupation categorical reclassification was deferred to WP11.
- **Evaluation Layering**: Relocating metric calculations to `ml/models/metrics.py` allowed completely emptying `ALLOW_LISTED_VIOLATIONS` in `tests/test_architecture.py`.

---

## 5. Noticed but Not Fixed
- `shap` plotting deprecation warnings (`set_bad`, `set_over`, `set_under`) when rendering SHAP summary plots in tests.
- LangGraph agent search/details test assertions in `tests/test_phase3_langgraph_agent.py` are part of the baseline failures documented in `baseline_tests.txt`.
