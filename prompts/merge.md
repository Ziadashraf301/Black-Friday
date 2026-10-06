Read all REVIEW_*.md files in the project root. Merge them into FIX_PLAN.md.
Rules:
- Deduplicate findings reported by more than one review into a single fix, listing which reports mention it.
- Drop findings that were REJECTED, or that another review shows are already resolved.
- Group fixes into ordered batches (Batch 1 = foundation that others depend on). Within a batch, only include fixes that touch different files.
- For each fix give: files touched, one-line description, severity, and any verification needed before editing (for example, check the metric direction before changing a comparison).
- Mark fixes that need a human decision or retraining.
- Do NOT modify any code or any file except FIX_PLAN.md.
