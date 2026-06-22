---
name: Coding style and conventions
description: R coding patterns and conventions used in this codebase
type: feedback
---

Use the dot.case naming style for function arguments (e.g., `bathy.style`, `land.col`, `expand.factor`) consistent with the existing API — do not switch to snake_case for new arguments.

**Why:** All existing public functions use dot.case for multi-word arguments. Changing would break the API.

**How to apply:** Any new arguments added to basemap(), qmap(), transform_coord(), etc. should follow dot.case.

---

Internal helper functions are exported (`@export`) but marked `@keywords internal`. Follow this pattern for new internal helpers.

**Why:** Advanced users sometimes call internal functions directly; exporting avoids NAMESPACE warnings while the `@keywords internal` tag keeps them out of the main reference index.

---

Documentation uses roxygen2 with `@inheritParams basemap` extensively to avoid repeating shared parameter docs. Use this for any new function that shares basemap parameters.

---

The `verbose` argument defaults to `FALSE` in most functions (e.g., `basemap()`, `transform_coord()`). New functions with potentially chatty output should follow this pattern.

---

Test parameters for interactive debugging are left as comments at the top of function definitions (e.g., `# limits = c(160, -160, 60, 80); bathymetry = TRUE`). This is intentional and not noise to be removed.
