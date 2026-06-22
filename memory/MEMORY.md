# Memory index

This folder is the project's persistent memory. Each file holds one topic; this
index is the one-line-per-entry table of contents — skim it, then open what you need.

**Do not put personal information in these files.** This folder is public on
GitHub. No absolute local paths (`/Users/<name>/...`), machine-specific
filenames, dataset versions tied to one computer, or contact details beyond the
maintainer's public package metadata. Use placeholders: `~/...`,
`path/to/your/file.nc`, `<your-data-folder>`. See
[feedback_no_personal_info_in_docs.md](feedback_no_personal_info_in_docs.md).

## Project

- [Project overview](project_overview.md) — package purpose, version, dependencies, release channels
- [Architecture](architecture.md) — `basemap()` pipeline, projection logic, shapefile registry, eval/parse pattern
- [v3.0.0 master plan](plan_v2.4.0.md) — living plan/record for the major update (WCS bathymetry, vector_land, docs revision, CRAN). Filename kept from the v2.4.0 working name.
- [cran-comments](cran-comments.md) — CRAN submission notes (test environments + R CMD check results)

## Who I work with

- [User profile](user_profile.md) — the package author/maintainer (role, expertise)
- [Coding style](feedback_style.md) — R patterns and conventions used in this codebase
- [No personal info in docs](feedback_no_personal_info_in_docs.md) — keep local paths/filenames out of committed/published files
- [No-wrap prose](no-wrap-prose.md) — don't hard-wrap paragraphs in Rmd/Quarto/Markdown; one line per paragraph

## How-to / reference

- [Building vignettes locally](build-vignettes-locally.md) — pandoc path + env vars to render pkgdown articles via Rscript
- [Large-data figure hosting](large-data-figure-hosting.md) — pre-rendered vignette figures live in the ggOceanMapsLargeData repo, embedded by URL
