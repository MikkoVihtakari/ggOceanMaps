---
name: no-wrap-prose
description: Author preference — do not hard-wrap prose paragraphs in Rmd/Quarto/Markdown
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 35650802-957c-4fb7-adee-b30bd3742d17
---

When editing `.Rmd`/`.qmd`/`.md` files, write each prose paragraph as a single unwrapped line (no hard line breaks at ~80 cols). The user relies on editor soft-wrap (VS Code / Positron) and finds hard-wrapped paragraphs hard to read because they don't follow the window width.

**Why:** Hard wraps are fixed to a column width that doesn't match the reader's window.

**How to apply:** One line per paragraph in prose. Code chunks are unaffected (normal R style/line lengths still apply). Applies to all docs going forward, not just one file. See [[build-vignettes-locally]] for the render workflow.
