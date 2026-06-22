---
name: feedback-no-personal-info-in-docs
description: Do not include the user's local paths, machine-specific filenames, or other personal info in committed/published docs (AGENTS.md, README, vignettes, cookbook). Generic placeholders only.
metadata:
  type: feedback
---

Do not include the user's local paths, machine-specific filenames, or other personal info in committed/published docs (e.g. AGENTS.md, README, vignettes, cookbook recipes). Use generic placeholders.

**Why:** User explicitly flagged this after `~/Downloads/GEBCO_2025.nc` was used as an example userpath in AGENTS.md. That string echoed their actual local setup, which doesn't belong in a public-facing file checked into git. The author attribution line on vignettes (`Mikko Vihtakari (Institute of Marine Research)`) and the canonical project URLs (`github.com/MikkoVihtakari/ggOceanMaps`) are package metadata, not personal info in this sense — those are fine.

**How to apply:**
- For example file paths, use generic placeholders: `path/to/your/bathymetry.nc`, `~/path/to/data`, `<your-data-dir>`.
- For example coordinates, use round numbers around well-known regions (`c(-20, 30, 50, 70)` for Europe is fine), not the user's actual research sites.
- Before committing any public-facing doc, scan for: home directory paths, specific filenames the user has mentioned, dataset versions tied to a particular machine.
- Personal info ≠ authorship/canonical URLs in package metadata. Those are public and expected.

Related: [[plan-v2.4.0]] (the AGENTS.md added in Phase 1.D triggered this).
