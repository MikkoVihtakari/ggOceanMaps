---
name: plan-v2.4.0
description: Master plan and research notes for the v2.4.0 major update — covers WMS/WCS bathymetry, vector_land, EMODnet/BSBD support, Jules PR triage, docs revision, AI-agent friendliness, and CRAN resubmission. Living document — update as items are addressed.
metadata:
  type: project
---

# ggOceanMaps v2.4.0 master plan

Status: **RELEASE RE-SCOPED TO 3.0.0 (2026-06-19).** "Bringing ggOceanMaps to
the AI age." Version bumped to 3.0.0; R CMD check --as-cran clean (0 errors / 0
warnings / 1 transient NOTE = bathymetry.html 404 that resolves on site deploy).
All work on branch `v2.4.0-dev` (branch name unchanged; release is 3.0.0). PR
#67 still draft. **Awaiting the maintainer's review + Codex code review; then
THEY push/merge to master and submit the tarball to CRAN — do NOT merge or
submit.**

3.0.0 work done this pass (all committed on v2.4.0-dev):
- Version 3.0.0 in DESCRIPTION + AI-age sentence; NEWS.md reworked with 3.0.0
  entry (New features / Documentation & AI support / Bug fixes / Testing /
  Internal).
- New `vignettes/new-features-v3.Rmd` wired into the pkgdown News menu.
- User manual (`ggOceanMaps.Rmd`) slimmed 663->490 lines: overview + links;
  advanced recipes (scale bar, ocean colour, graticule order, second fill
  scale, polar labels) moved into the Cookbook (polar-labels rewritten base-R
  to drop undeclared dplyr).
- Dropped the AeN workshop deck from the site (navbar in `_pkgdown.yml`, the
  58 built docs/*.html, and the README link).
- Repo/CRAN leanness: deleted stray root `figure/`; pruned orphan README figs;
  `.Rbuildignore` now excludes `man/figures` + `figure/`, cleaned dup DS_Store.
  Tarball = 3.1 MB, only R/data/man/tests, NO figures/vignettes/docs.
- Code review: added grDevices, stats, tools to Imports (used via :: ); dropped
  vignette-only Suggests (scatterpie, cowplot, ggnewscale, scales) — fixes the
  --as-cran ERROR. Kept ggspatial (examples).
- Tests: FAIL 0 / PASS 150. 3 rotate/antimeridian vdiffr baselines re-accepted
  (matched the earlier antimeridian fix; only rotate cases changed).
- Added `cran-comments.md`.
- README + full pkgdown site recompiled (0 errors/0 warnings); pushed.
  All 3.0.0 work is on origin/v2.4.0-dev (latest `2aaa431`). **Done — awaiting
  maintainer review + Codex review before merge/CRAN.**

**Key decisions to FLAG at review** (see report):
1. Vignettes are excluded from the CRAN tarball via `.Rbuildignore` `^vignettes$`
   (website-only; they need large downloads). Pre-existing; confirm intended.
2. `NEWS.md` is also `.Rbuildignore`d (no changelog in tarball / `news()`).
   Pre-existing; confirm intended.
3. The bathymetry.html 404 NOTE clears once the rebuilt site is deployed
   (on merge to master / GitHub Pages).

### >>> VS CODE HANDOFF (start here) <<<

This task continues in **Claude Code inside VS Code on the maintainer's machine**,
where (unlike the earlier sandbox) **pandoc IS available** and the configured
`getOption("ggOceanMaps.datapath")` (`~/Documents/ggOceanMapsLargeData`, a
OneDrive path) **IS readable** — so the *full* pkgdown site (incl. large-data
maps) can actually render and the maintainer reviews it to verify the release.

**Immediate next step — rebuild + commit the site:**
1. `git pull` on `v2.4.0-dev` (the bug fix `b8b1eec` changed land clipping, so the
   committed `docs/` is now stale).
2. Rebuild: `pkgdown::build_site(lazy = FALSE)`. It downloads ~16 MB of
   large-data .rda from ggOceanMapsLargeData on first run (now works
   non-interactively — see the menu() fix logged below). Build should finish
   with **zero errors and zero warnings**; if not, fix whatever broke.
3. Commit the regenerated `docs/` (the `b8b1eec` commit deliberately committed
   only the code fix + snapshots, **not** the docs rebuild). Then push.

**Then finish the remaining doc revisions (the "revise webpage documentation"
task):**
- **Depth-bins example (TODO below)** — add an example showing how to manually
  adjust bathymetry depth bins. Best home: `vignettes/bathymetry.Rmd` or
  `vignettes/cookbook.Rmd`. Two angles: (a) custom `depths=` vector in
  `raster_bathymetry()`; (b) re-mapping the discrete fill on an existing
  basemap via `scale_fill_manual()` / `scale_fill_viridis_d()`.
- **Walk the rebuilt site** and fix anything that reads wrong/stale. The
  maintainer's review IS the acceptance test.
- **Known open flags** (carry into the review):
  - Vignette build paths now read `GGOCEANMAPS_DATAPATH` / `GGOCEANMAPS_USERPATH`
    from `~/.Renviron` (replaced the old hardcoded personal paths). Set these in
    `~/.Renviron` so the high-res / user-raster example chunks render locally.
  - AeN workshop slides navbar link is still an external broken URL — deferred
    to Phase 4 (workshop deck → vignette).

**Do NOT** merge PR #67 or do anything CRAN-related (Phase 5, gated on the
maintainer's explicit go-ahead after they review the new features).

Working commands (`CLAUDE.md` has the full list): `devtools::load_all()`,
`devtools::document()`, `devtools::test()`, `pkgdown::build_site()`.

### Phase re-org (2026-05-29, user request)

The release tail was split so the user can review new features before anything
ships. **Current focus: finish Phase 3 so the pkgdown site recompiles cleanly** —
the user verifies the release by walking the rebuilt webpage ("the quickest way
for me to make sure everything works as intended").

- **Phase 3 (NOW)** — docs/site work so the pkgdown webpage recompiles cleanly.
  Includes the existing 3.C–3.H items **plus** a new vignette
  **"Adding graphical elements"** (ocean currents + scatterpies — see Phase 3.J).
- **Phase 4 (deferred)** — workshop deck as a vignette (was 3.F). Larger, separate
  task. Strip AeN references, update content for 2.4.
- **Phase 5 (deferred, gated on user review)** — PR #67 **merge** + **CRAN
  submission**. The user will "carefully review the new features" before merge.
  CRAN must wait. Includes dry-runs, `urlchecker::url_check()`, win-builder, rhub.

Workshop deck (old 3.F) → Phase 4. CRAN submit (old 3.I) + PR #67 merge → Phase 5.

**Phase 2 deferred items** (documented but not implemented):
- BSBD helper — needs user input on which download format BSBD produces; no existing dev code found.
- ~~Global WCS source~~ — **DONE** (ETOPO1 added in commit aaa2b8e, see Phase 2.F below).
- GEBCO WCS — not implemented; ETOPO1 covers the global need. Could add later if GEBCO endpoint verified.

## Where to pick up next

**Branch state**: `v2.4.0-dev` at commit `b8b1eec` (pushed to origin). PR #67 still draft. CI green on all 5 platforms. See the VS Code handoff block at the top for the active task.

**Immediate goal**: run Phase 3 so the pkgdown webpage recompiles cleanly. The
user reviews the rebuilt site to confirm everything works.

**Phase 3 status: COMPLETE (3.A–3.E, 3.G, 3.H, 3.J all done, pushed to v2.4.0-dev as of 5ea239d).**
Awaiting the user's pkgdown site rebuild + review. Done items:
- **3.C — DONE** (5ea239d): `ggOceanMaps.Rmd` — env-var datapath/userpath (no personal paths committed) + replaced retired-package "Custom shapefiles" section (and its `data(bs_shapes, package="ggOceanMapsData")` build-breaker) with a pointer to customising-shapefiles.html / bathymetry.html.
- **3.D — DONE** (8378b4a): rewrote premade-maps.Rmd + premade-shapefiles.Rmd to current reality (archived retired BarentsSea/IBCAO/GEBCO/EMODnet recipes in HTML comments); navbar entries uncommented.
- **3.E — DONE** (5e9914f): new customising-shapefiles.Rmd (clip_shapefile, raster_bathymetry→vector_bathymetry/vector_land, geonorge_bathymetry).
- **3.G — DONE (nothing to do repo-side)**: `.Rproj.user/` already gitignored & untracked; no stray untracked files; `docs/` regeneration IS the user's webpage recompile (needs pandoc, unavailable in sandbox).
- **3.H — DONE** (5ecc2e9): GDAL `+init=epsg:` deprecation fixed in test-basemap-smoke.R. The 6 latin1→UTF-8 warnings come from attribute strings baked into ggOceanMapsLargeData .rda files (separate repo) — out of scope here.
- **3.J — DONE** (cba8b2e): new adding-graphical-elements.Rmd (ocean-current arrows via transform_coord+xspline; scatterpie::geom_scatterpie pies). scatterpie added to DESCRIPTION Suggests.

**Open flags for the user (report at review):**
- Env-var datapath change to their local build: set `GGOCEANMAPS_DATAPATH` / `GGOCEANMAPS_USERPATH` in `~/.Renviron` to render high-res / user-raster examples locally (replaces the old hardcoded `~/Documents/...` + `~/Downloads/...` paths).
- AeN workshop slides navbar link still external & broken — folded into Phase 4 (workshop deck → vignette).
- Verification: all 9 vignettes parse-check OK; _pkgdown.yml valid; every navbar article href resolves to a vignette file. Final confirmation is the user's `pkgdown::build_site()`.

**Phase 4 (deferred)** — workshop deck as vignette (was 3.F): port from `Meetings/2021_01 ggOceanMaps workshop/ggOceanMaps lecture/ggOceanMaps_workshop.Rmd`. Strip AeN references. Update content for 2.4.

**Phase 5 (deferred, gated on user review)** — PR #67 merge + CRAN submission:
- Bump nothing (already 2.4.0); spell-check README/NEWS; `urlchecker::url_check()`; win-builder; rhub; final `--as-cran`.
- **User reviews new features first, then merges PR #67. Do not merge or submit without explicit go-ahead.**

**TODO — unwrap hard-wrapped vignette prose** (user reminded 2026-06-20: no hard
wrapping in Rmd; one line per paragraph). Already-unwrapped: README.Rmd,
ggOceanMaps.Rmd, new-features-v3.Rmd, bathymetry citation list. **Still wrapped
(authored this release, pending):** `bathymetry.Rmd`, `cookbook.Rmd`,
`premade-maps.Rmd`, `premade-shapefiles.Rmd`. Reflow prose to single lines while
preserving code fences, tables, and list items; then rebuild site.

**Park notes**:
- BSBD helper still needs user input on file format.
- All Phase 3 docs items below 3.B are independent of each other — can be tackled in any order.
- After 3.H clean-up, do a final `R CMD check --as-cran` in non-interactive state (use the test recipe in the 2026-05-28 entry below).
- **TODO (docs)**: Add an example showing how to manually adjust bathymetry depth bins — e.g. overriding the default `depths` vector in `raster_bathymetry()` / the `bathy.style` discrete scale, or using `scale_fill_manual()` / `scale_fill_discrete()` on the resulting depth-bin factor. Probably fits best in `vignettes/bathymetry.Rmd` or the cookbook.

## Progress log

**2026-06-20 — Codex review landed + two follow-up basemap fixes**

*Codex code review (commit `76a0d60`, applied by the maintainer):* hardened
`wcs_bathymetry()` — server-side `downsample` (the WCS server reduces transfer;
the returned bathy carries `attr(., "downsampled") = TRUE` so basemap does not
downsample again), thorough input validation (limits/source/coverage/force/
verbose/downsample/timeout/max_area_deg2/tile_size_deg/cache_dir), validated
temp files + atomic cache writes, auto-replacement of invalid cached TIFFs
(`.is_valid_tiff`), and multipart-MIME splitting on the full boundary. `basemap.R`
gained `plot.downsample` (0 when already downsampled, else `downsample`), used by
`map_cmd` `geom_stars`. Added `tests/testthat/test-basemap-crop.R`; expanded WCS
tests. Roxygen examples use `options(old_options)` with placeholder paths.

*Two bugs the maintainer then found, both fixed (in `basemap_data_crop`):*

1. **Rotated antimeridian maps drew no land** (issue #44 regression): e.g.
   `basemap(c(100,-120,-12,-57), rotate=TRUE)`, `c(40,-70,-37,40)`, `c(0,-120,
   -12,-57)`. Rotated **DD** maps use a geographic CRS `+proj=longlat +lon_0=<mid>`
   (NOT projected — `st_is_longlat` is TRUE). A landmass crossing the new
   antimeridian (lon_0 ± 180) — even off-screen Antarctica — gets "unwrapped"
   into a >180° ring on `st_transform`, and ggplot2 then skips the whole land
   layer. Fix: before transforming, cut a thin slit (`st_difference` with a
   ~2e-5°-wide bbox at the seam, s2 off) from land/glacier/(longlat sf bathy);
   and route antimeridian-risk land clipping through the map CRS (broadened the
   `if(antimeridian_risk ...)` branch to drop the `!st_is_longlat` gate). The
   old WGS84 buffer pre-clip was replaced by this seam-cut.
2. **Default bathymetry was downsampled to ~256 cells**: the warp target used
   `stars::st_as_stars(st_bbox(clip_limits))`'s coarse default, so
   `basemap(60, bathymetry=TRUE)` came out 255×255 vs the 7200×3600 source.
   Fix: new local `warp_to_clip()` builds a target grid with **square** cells
   sized to reproduce the source cell-count over the clip region (densify clip
   before measuring extent in the source CRS; one cell size for both axes so
   polar aspect isn't distorted; cap 4000/dim). Now ~1287×1287 for the Arctic.

Snapshots updated: `bathy-rbb`, `bathy-rcb`, `antimeridian-low-lat-bathy-rotate`
(higher-res / corrected land). `issue-44`, `antimeridian-160`, `data-antimeridian-
rect` now MATCH their committed baselines again — confirms the fix restores the
known-good behaviour. Regression tests added to `test-basemap-crop.R`. All on
`v2.4.0-dev`.

**2026-06-16 — basemap() land-clipping bug fixed (commit `b8b1eec`)**
- Symptom: `basemap(c(-20, 30, 50, 70))` clipped off northern land (e.g. Norway
  above ~67.5°N) even though limits asked for 70°N.
- Root cause: in `basemap_data_crop()` (`R/basemap_data.R`), the non-custom-CRS
  branch built the land clip boundary from `dd_clip_boundary()`, which projects
  the dd limits to the map CRS (EPSG:3995 here), takes the **bbox** (a 5-point
  rectangle), then back-projects those 5 corners to WGS84 for `clip_shapefile()`.
  With only 5 points, projection curvature warps the WGS84 clip polygon — its
  ymax came out ~67.5° instead of 70°, so land was cut early.
- Fix: **densify** the projected bbox (`smoothr::densify(..., n = 100)`) before
  back-projecting — matches what the custom-CRS branch already did. Guards:
  only densify when `clip_limits` is a projected (non-longlat) CRS, and **skip**
  for antimeridian-crossing (`limits[1] > limits[2]` or lon span > 180°) and
  rotated maps (densifying near ±180° → GEOS TopologyExceptions). Without the
  guards, 4 antimeridian/rotate smoke tests threw TopologyExceptions.
- Verified: `c(-20,30,50,70)` land WGS84 ymax now 72.5°; all antimeridian/rotate
  smoke cases OK. 6 basemap-snapshot SVGs re-accepted (improved land coverage).
  Full suite: **148 PASS / 0 FAIL** (6 warns = the known largedata latin1→UTF-8;
  1 skip = empty WCS test).

**2026-06-16 — pkgdown build fixed for clean rebuild (commit `f6028be`)**
- The maintainer ran `pkgdown::build_site(lazy = FALSE)`; fixed three classes of
  problem so it completes with **0 errors / 0 warnings**:
  1. **`_pkgdown.yml`**: added `url:` (sitrep) and `wcs_bathymetry` to the
     reference index (it was missing → fatal "topic not in index" error).
  2. **`R/load_map_data.R`**: both `utils::menu()` prompts now guarded with
     `if(!interactive())` → in non-interactive/CI/pkgdown builds they
     auto-proceed (create datapath dir / download files) instead of erroring
     "menu() cannot be used non-interactively". Added `recursive = TRUE` to
     `dir.create()`.
  3. **Vignette/README accessibility + metadata**: set the `bathy.style="rcb"`
     demo chunk in `ggOceanMaps.Rmd` to `eval=FALSE` (it pulled the 85 MB
     `dd_rbathy_cont.rda`); fixed 3 `VignetteIndexEntry` title mismatches
     (`ggOceanMaps.Rmd`, `new-features.Rmd`, `poster.Rmd`); added alt-text to
     4 hosted PNGs in `premade-maps.Rmd`, the `<img>` in `poster.Rmd`, and the
     README demo figure.
- The committed `docs/` from this build is now **stale** (predates `b8b1eec`) —
  the VS Code session must rebuild + re-commit (see handoff block).

**2026-05-29 — Phase 3.J done (new "Adding graphical elements" vignette)**

- New `vignettes/adding-graphical-elements.Rmd` wired into `_pkgdown.yml`
  Articles menu (after Bathymetry, before Cookbook). Two sections:
  - **Ocean currents** — (a) a synthetic u/v velocity field as a
    `geom_segment()` quiver (eval=TRUE, renders without downloads);
    (b) schematic "Figure 1" arrows adapted from the **Barents-Sea-currents**
    repo: read `tabular/barents_currents.csv` (cols `id,long,lat,order,group,
    size,type`) from the raw GitHub URL, `transform_coord(..., proj.out=3995)`,
    smooth per-group with `graphics::xspline(shape=-0.6, draw=FALSE)`, plot
    with `geom_path()` + `arrow()` + `scale_colour_manual(Arctic=blue,
    Atlantic=red)`. **Gotcha solved**: `xspline()` needs an open device even
    with `draw=FALSE` → recipe opens `grDevices::pdf(NULL)` + `plot.new()`
    inside the per-arrow helper. Verified live: 669 CSV rows → 33250 smoothed.
    NOTE: old code used `basemap("barentssea")` which **no longer exists**;
    recipe now uses `basemap(c(0,50,68,83), shapefiles="Arctic")`.
  - **Scatter pies** — web research (2026-05-29) confirms
    `scatterpie::geom_scatterpie()` is the modern standard (works w/ ggplot2
    4.0). Recipes for DD map (r in degrees) and projected map
    (`transform_coord()` centres, r in metres), plus `geom_scatterpie_legend()`
    and a `ggnewscale::new_scale_fill()` note for bathymetry+pies. scatterpie
    chunks eval=FALSE (pkg optional). `scatterpie` added to DESCRIPTION Suggests.
- All 11 R chunks parse-checked via `knitr::purl` (pandoc-free; sandbox has no
  pandoc). The eval=TRUE quiver chunk verified to return a ggOceanMaps gg.
- NEWS.md updated (bathymetry + adding-graphical-elements vignettes).

**FINDING — premade-maps.Rmd is stale (blocks 3.D as written)**: `shapefile_list()`
now only has ArcticStereographic, AntarcticStereographic, DecimalDegree,
Svalbard, Europe. `premade-maps.Rmd` documents BarentsSea / IBCAO / GEBCO /
EMODnet premade maps that **no longer exist**, and `premade-shapefiles.Rmd`
uses retired `rgeos::gBuffer` / `sp::spTransform` / `convert_crs()` in its
(eval=FALSE) build recipes. Wiring them into the navbar as-is would document
non-existent functionality. **Needs user decision** before 3.D: rewrite to
current reality vs. drop vs. keep as historical. Left commented in `_pkgdown.yml`.


**2026-05-22 — Phase 1.A done (item 6, Jules PR triage)**
- #58 squash-merged → testthat infra + LS() tests, `Config/testthat/edition: 3`.
- #63 closed as superseded; its quiet() test content reapplied as a follow-up commit on v2.4.0-dev.
- Cleanup PRs squash-merged: #56 (deprecated ggplot2 shim), #59 (stale comment), #60 (legacy raster comments), #61 (97-line commented alt grid impl), #62 (1.3.7-era parallel comments). 131 lines of dead code removed total.
- Perf PRs squash-merged: #57 (`split()`-based grid line generation), #55 (clip in native CRS before transform).
- Verification: 15/15 testthat pass; basemap() smoke tests pass for polar N/S, Europe DD, antimeridian (rotate=TRUE), and bathymetry-on cases.

**2026-05-22 — Phase 1.B done (item 12, testthat + vdiffr)**
- Source: regression corpus at `ggOceanMaps development/tests/ggOceanMaps-function-tests.Rmd` (user-maintained, ~50 cases collected from GH issues over time).
- Two-tier approach:
  - **Smoke tests** (run everywhere): `tests/testthat/test-basemap-smoke.R` — 32 cases asserting `expect_s3_class(basemap(...), "gg")`. Catches "code stopped running" regressions.
  - **Visual snapshots** (vdiffr, `skip_on_cran() + skip_on_ci()`): `tests/testthat/test-basemap-snapshots.R` — 16 SVG baselines for polar / square / antimeridian / projected / crs / data-arg / bathymetry-style cases. Catches "code runs but wrong map" regressions.
- Unit tests added: `test-transform_coord.R`, `test-auto_limits.R`, `test-guess_coordinate_columns.R`. Plus existing `test-internal_functions.R` (LS, quiet).
- Helper `helper-skip.R` with `skip_if_no_largedata()`, `skip_if_no_userpath()`, `skip_unless_visual()`.
- Suggests: added `vdiffr (>= 1.0.0)` and `sp`.
- `.Rbuildignore`: excluded `tests/testthat/_snaps/basemap-snapshots/` so the CRAN tarball stays small (visual tests skip on CRAN anyway).
- Result: **81 tests pass locally**, 0 failures.
- **Baseline trust**: snapshots verified-good by reference to user's rendered corpus HTML at `ggOceanMaps development/tests/ggOceanMaps-function-tests_20260210.html` (dated 2026-02-10, pre-Jules-PR merges, all cases passed visually). Since the merged Jules PRs were either pure-deletion cleanups or computed-equivalent perf optimizations (`split()`, native-CRS clip), no visual drift is expected. Future drift will be caught automatically by vdiffr.

**2026-05-22 — Phase 1.C done (item 3, `vector_land()`)**
- Moved from `ggOceanMaps development/functions in dev/vector_land.R` into `R/vector_land.R`.
- Filled in land-cell selection: handles both `is.na()` mode (when `depths = NULL` or `estimate.land = FALSE`) and factor-level `"land"` mode (when `estimate.land = TRUE`).
- Removed `dplyr::recode_factor` (was depth-relabeling code, not relevant to land extraction) and `output.sf = FALSE` branch.
- Returns sf polygon layer with single `land = TRUE` attribute column — slots into `basemap(shapefiles = list(land = ..., ...))`.
- Added to pkgdown reference under "Create bathymetries and shapefiles".
- 6 testthat cases against a 20×20 synthetic stars raster, including end-to-end basemap() render.
- Total suite: **92/92 pass**, 0 failures.

**2026-05-22 — Phase 1.D done (item 1, agent friendliness)**
- `AGENTS.md` at repo root, user-agent-facing (complements CLAUDE.md which is dev-agent-facing). 8 MWE snippets + 7-item pitfalls list (limits order, projected-map transform, antimeridian, bathy data paths, no manual `coord_sf()`, shapefiles vs shape, first-call latency).
- `vignettes/cookbook.Rmd` scaffold organized by user-question. 5 recipes filled in (regions, polar, antimeridian, auto-fit-to-data, point overlays, projected-map overlays, qmap, ggsave); rest are stubs to fill alongside phase-2 work (WCS bathy, custom shapefiles, ocean currents).
- Wired cookbook into pkgdown navbar Articles menu.
- Added `^AGENTS\.md$` to `.Rbuildignore`.

## Phase 1 — DONE 2026-05-22

All four substreams (A: PR triage, B: testthat+vdiffr, C: vector_land, D: AGENTS.md+cookbook) complete. Branch `v2.4.0-dev` at commit 0081847. Tracking PR #67 (draft).

## Phase 2 progress

---

## Phase 2 progress log

**2026-05-22 — Phase 2.A done (EMODnet WCS feasibility spike)**

Endpoint is alive and usable from R with minimal tooling:

| Property | Value |
|---|---|
| URL | `https://ows.emodnet-bathymetry.eu/wcs` |
| Protocol | WCS 2.0.1 |
| Default coverage | `emodnet__mean` (also versioned: `_2022`, `_2020`, `_2018`, `_2016`) |
| Native CRS | EPSG:4326 (matches ggOceanMaps default — no reprojection needed for DD maps) |
| Resolution | ~0.00104° ≈ 115 m |
| Native format | GeoTIFF — opens cleanly with `stars::read_stars()` |
| Subset syntax | `SUBSET=Lat(min,max)&SUBSET=Long(min,max)` (lat-first per axisLabels) |
| Latency | 1°×1° → 4 MB, 2 s download. 10°×10° at native res would be ~400 MB. |
| End-to-end | Verified: GeoTIFF → `raster_bathymetry()` → bathyRaster → `basemap(shapefiles = list(bathy = ...))` renders. |

Implementation decisions from spike:
- **No ows4R dependency needed.** Plain `utils::download.file()` or `httr::GET()` + `stars::read_stars()` is sufficient.
- **Use Imports = utils** (already there); optionally add `httr` to Suggests for richer error handling, but plain `download.file()` with `mode="wb"` works.
- **Default downsample needed** for bboxes >~5°² to keep responses under 10 MB.
- **Cache layout**: filename hash of `(source, bbox, downsample, coverage)` under `getOption("ggOceanMaps.datapath")`.
- **Style integration**: register `wcs_emodnet_*` / `wcs_gebco_*` in `define_bathy_style()`, route to `bathy_rc` rendering (same path as `raster_continuous_*`). No new `map_cmd()` entry needed.

**2026-05-22 — Phase 2.B + 2.C done (WCS bathymetry — EMODnet)**

Three commits on `v2.4.0-dev`:

1. **[05305d4](https://github.com/MikkoVihtakari/ggOceanMaps/commit/05305d4) — standalone `wcs_bathymetry()`**
   Exported helper. Returns a `bathyRaster` from any registered WCS source (currently EMODnet). File-based cache under `getOption("ggOceanMaps.datapath")`. `max_area_deg2` guard (default 10). Live + offline-skip tests.

2. **[473fe44](https://github.com/MikkoVihtakari/ggOceanMaps/commit/473fe44) — bathy.style integration**
   `bathy.style = "wcs_emodnet_blues"` (abbrev `"wemb"`) and `"wcs_emodnet_grays"` (`"wemg"`) now route through `basemap()`. Touched `define_bathy_style()`, two spots in `basemap.R`, and the bathy injection step in `basemap_data.R`. Polar maps and non-DD limits error cleanly.

3. **[e3348bc](https://github.com/MikkoVihtakari/ggOceanMaps/commit/e3348bc) — docs**
   `basemap()` roxygen, AGENTS.md style chart, cookbook recipe, NEWS.md 2.4 entry.

Total: **111 tests pass** (incl. 8 new WCS tests), 0 failures. **GEBCO source not implemented** — registry leaves room for it but no endpoint verified yet. Punt to a follow-up if/when needed.

**2026-05-28 — R CMD check fixes — PR #67 CI now green (commit 36c5f0c)**

PR #67's R-CMD-check workflow was failing on all five platforms (macOS,
Ubuntu devel/release/oldrel-1, Windows). Reproduced locally with
`devtools::check(args = '--as-cran')` running with `interactive() == FALSE`
and `ggOceanMaps.datapath = tempdir()` (simulates CI). Status went from
**1 ERROR, 3 WARNINGs, 1 NOTE** → **0/0/0 = OK**.

Fixes:

1. **ERROR — `menu() cannot be used non-interactively`** (7 smoke + 1 snapshot
   test). Root cause: `.onLoad` sets `ggOceanMaps.datapath = tempdir()`
   silently if the user hasn't configured one, so existing
   `skip_if_no_largedata()` (which only checked `dir.exists()`) didn't
   trigger and `basemap()` tried to prompt for a download.
   - `tests/testthat/helper-skip.R`: helper now also skips when the path
     is `tempdir()` AND `interactive() == FALSE`.
   - Added `skip_if_no_largedata()` to four tests that needed it but
     weren't calling it (`test-basemap-smoke.R` lines 99, 106, 116;
     `test-basemap-snapshots.R` line 84).
2. **WARNING — non-ASCII chars in `R/wcs_bathymetry.R`** (°, —, ×, ≈, →, −).
   - Roxygen comments: replaced with ASCII (`deg`, `--`, `x`, `~`).
   - User-visible error strings: `°` and `≈` escapes (render
     identically as ° and ≈ in UTF-8 sessions).
3. **WARNING — Rd cross-reference `\link[stars]{stars}`** broken (no `stars`
   function in the stars package). Changed to `\link[stars]{read_stars}`.
4. **WARNING — unstated dependency on `withr`** in tests
   (`withr::with_options` in `test-wcs_bathymetry.R`). Added `withr` to
   `Suggests`.
5. **NOTE — invalid URL `../articles/bathymetry.html`** in `man/basemap.Rd`
   (pkgdown-relative path doesn't resolve in Rd). Replaced with canonical
   `https://mikkovihtakari.github.io/ggOceanMaps/articles/bathymetry.html`.

**Test recipe for CI-like local check**:
```r
.GlobalEnv$.ggOceanMapsenv <- NULL
options(ggOceanMaps.datapath = tempdir())  # what .onLoad does without user config
options(ggOceanMaps.userpath = NULL)
devtools::check(args = c("--as-cran", "--no-manual"), error_on = "never")
```
Run from a non-interactive Rscript so `interactive()` is FALSE.

**Live CI result** (PR #67, run 26579493071):
- macOS release: pass 4m58s
- Ubuntu devel: pass 6m12s
- Ubuntu oldrel-1: pass 3m54s
- Ubuntu release: pass 4m4s
- Windows release: pass 3m58s

**2026-05-28 — Phase 3.A + 3.B done (bathy.style table + bathymetry vignette — commit bd545d0)**

- **3.A**: replaced the long itemized `bathy.style` prose list in
  `R/basemap.R` (was 13 bullet points) with a 6-column table (alias,
  abbreviation, geometry, data source, needs, notes). One row per blues
  style; greyscale variants described in a paragraph below the table.
  Cuts the section roughly in half and is easier to scan.
- **3.B**: new `vignettes/bathymetry.Rmd` walking through all five
  bathymetry sources in order of setup cost (shipped → ggOceanMapsLargeData
  → user raster → live WCS → build-your-own). Opens with a "Quick decision
  guide" table. Examples are drawn from the regression corpus
  (`ggOceanMaps-function-tests.Rmd`), so the article doubles as
  documentation-as-test for the new WCS features.
- `vignettes/ggOceanMaps.Rmd`: trimmed the ~80-line bathymetry section to
  a 5-item summary + link to the new vignette. No more duplicated style
  descriptions.
- `_pkgdown.yml`: wired bathymetry article into the Articles navbar menu,
  positioned first.

**2026-05-27 — Phase 2.E done (cookbook stubs + Phase 2 wrap-up)**

All five cookbook stubs filled in `vignettes/cookbook.Rmd`:
- High-res bathymetry via ggOceanMapsLargeData auto-download + `ggOceanMaps.datapath`
- GEBCO/ETOPO via `ggOceanMaps.userpath` (quick) and `raster_bathymetry()` + `basemap()` (manual)
- Full `raster_bathymetry()` → `vector_bathymetry()` + `vector_land()` pipeline with `save()`/reuse tip
- `clip_shapefile()` recipe
- Ocean current vectors: `geom_segment()` pattern for both DD and projected maps
- ICES and Norwegian fishery zone overlays (corrected column names: `Area_Full`, `SubArea`, `main_area`, `sub_area`)
Fixed stale `max_area_deg2 = 10` → `50` in EMODnet recipe. **123 tests pass, 0 fail.** Commit 47122fd.

**2026-05-27 — Phase 2.D done (WCS coverage-bug fixes)**

Two user-reported bugs fixed in `R/wcs_bathymetry.R`:

1. **Out-of-coverage silent failure** (`basemap(c(110, 120, -20, 30), bathy.style = "wemb")`):
   - Added `extent = c(-36, 43, 15, 90)` to the EMODnet registry entry (approximate European-waters bounds).
   - Added extent check in `wcs_bathymetry()` that fires **before** the area guard (better error message: "entirely outside EMODnet coverage … use GEBCO/ETOPO locally instead").
   - All three bbox examples (Indonesian waters, Pacific, Southern Ocean) now get a clear, actionable error message without any network call.

2. **GeoTIFF validation** for cases where the server returns an XML error document instead of a TIFF (e.g. boundary edge cases not caught by the extent check):
   - Added `.is_valid_tiff()` helper that reads 4 magic bytes (supports both little-endian and big-endian TIFF/BigTIFF).
   - Fires in `wcs_fetch_tile()` after the download size check; deletes the bad cached file and stops with a descriptive message.

3. Fixed pre-existing `conditionMessage()` bug — was being called on an integer `0L` (success return from `download.file`) when the file-size check failed.

Tests added (`test-wcs_bathymetry.R`): 3 new out-of-coverage tests + 1 "inside coverage doesn't error on extent check" + 1 `.is_valid_tiff()` test (little-endian, big-endian, XML). **Total suite: 24 tests pass (5 network-gated skips).**

**GEBCO/ETOPO WCS research (2026-05-27)**

Both offer usable endpoints:

| Source | URL / pattern | Resolution | Notes |
|---|---|---|---|
| **GEBCO** (BODC) | WCS through https://www.gebco.net/data_and_products/webservices/ | ~460 m (15″) | WCS 1.1 via GeoServer; WCS 2.0.1 endpoint URL needs live testing before hard-coding |
| **ETOPO 2022** (NOAA NCEI) | `https://gis.ngdc.noaa.gov/arcgis/services/DEM_mosaics/ETOPO_2022_v1_60s_surface/ImageServer/WCSServer` | ~1.85 km (1′) | ArcGIS Server WCS 2.0.1; auth-free; different axis/subset syntax than OGC GeoServer |
| **NCEI global mosaic** | `https://gis.ngdc.noaa.gov/arcgis/services/DEM_mosaics/DEM_global_mosaic_1arc/ImageServer/WCSServer` | 1″/30″ patchwork | Same ArcGIS WCS pattern |

Recommended strategy: add GEBCO as `wcs_gebco` source (CC0, gold-standard bathymetry, global) once the exact WCS 2.0.1 endpoint is verified against a test bbox. ArcGIS WCS needs axis-order and RANGESUBSET investigation before implementation. **Deferred to a future session.**

**Pre-existing minor issue noted**: `basemap_data_crop()` triggers a `range()` warning ("no non-missing arguments to min/max") inside `raster_bathymetry()` at `R/raster_bathymetry.R:227`. Not introduced by Phase 2 work; visible in test warnings. ~~Worth fixing as a small follow-up.~~ **Fixed in [ed1580c](https://github.com/MikkoVihtakari/ggOceanMaps/commit/ed1580c)**: `raster_bathymetry()` now detects whether input is raw (negative depths + positive land) or already-processed (positive depths + NA land), only inverts sign in the raw case, and guards `range()` against an all-NA raster. WCS injection now stores the full bathyRaster object so it routes through the bathyRaster branch in `basemap_data_crop()` instead of being re-processed.

**Other remaining warnings** (pre-existing, not Phase 2): 6 UTF-8 encoding warnings on polar/projected/Arctic-shapefile cases (latin1 → UTF-8 conversion in stars/sf), 1 GDAL deprecation warning for `+init=epsg:XXXX` syntax in test-basemap-smoke.R sp input case. Not blocking but worth cleaning up before CRAN submission in Phase 3.

## Research findings (captured before discussion)

### Key code locations relevant to the plan
- `bathy.style` parsing: `define_bathy_style()` at [R/internal_functions.R:240](../R/internal_functions.R:240). Strings parsed as `type_resolution_color` (e.g. `raster_binned_blues`); returns a tag (`bathy_rb`, `bathy_rc`, …) dispatched inside `map_cmd()`.
- `basemap()` runtime assembly: layers are concatenated as a string and run via `eval(parse(text = layers))` ([R/basemap.R](../R/basemap.R)). New bathymetry sources slot in by adding a tag + a `map_cmd()` branch.
- `vector_land()` stub: [`ggOceanMaps development/functions in dev/vector_land.R`](../../ggOceanMaps%20development/functions%20in%20dev/vector_land.R). Validation, drop.crumbs, smoothing chain is written; the "pick the land" step (line 36) is empty. Uses `dplyr::recode_factor` (line 107) — dplyr is not an Imports dependency; replace with base R / forcats before merging.
- Geonorge pattern (analog for BSBD helper): [R/geonorge_bathymetry.R](../R/geonorge_bathymetry.R) — reads `.gml` via `sf::st_read`, extracts depth col, returns sf. Use as template.
- pkgdown navbar with commented-out article links: [_pkgdown.yml](../_pkgdown.yml) — `articles/premade-maps.html` and `articles/premade-shapefiles.html` are present-but-commented; source `.Rmd` files exist (151 + 323 lines). Workshop link points to broken external URL `https://aen-r-workshop.github.io/4-ggOceanMaps/`.
- Workshop source: [`Meetings/2021_01 ggOceanMaps workshop/ggOceanMaps lecture/ggOceanMaps_workshop.Rmd`](../../../Meetings/2021_01%20ggOceanMaps%20workshop/ggOceanMaps%20lecture/ggOceanMaps_workshop.Rmd).
- Tests: no `tests/testthat/` folder exists yet. Jules PRs #58 and #63 would establish it.

### Jules PR inventory (as of 2026-02-20, all by @MikkoVihtakari via Jules bot)
| PR | Title | Δ | Files |
|---|---|---|---|
| #55 | Optimize basemap_data_crop by clipping in native CRS | +5/-2 | R/basemap_data.R |
| #56 | Remove deprecated ggplot2 code in basemap.R | +0/-6 | R/basemap.R |
| #57 | Optimize Grid Line Generation (split/lapply) | +6/-6 | R/basemap_data.R |
| #58 | Add tests for LS | +39/-1 | DESCRIPTION, tests/testthat.R, tests/testthat/test-internal_functions.R |
| #59 | Remove commented-out code in basemap_data.R | +0/-1 | R/basemap_data.R |
| #60 | Remove legacy raster code comments in raster_bathymetry.R | +0/-9 | R/raster_bathymetry.R |
| #61 | Remove commented-out code in basemap_data.R (larger) | +0/-97 | R/basemap_data.R |
| #62 | Remove unused parallel code in dist2land.R | +0/-18 | R/dist2land.R |
| #63 | Add tests for quiet function | +18/-1 | DESCRIPTION, tests/testthat.R, tests/testthat/test-internal_functions.R |

### WMS/WCS feasibility for R
- `sf::gdal_utils()` and `terra::rast()` both consume remote OGC services via GDAL's WMS/WCS drivers.
- `ows4R` is a dedicated R client for WMS/WMTS/WCS/WFS.
- **WCS, not WMS**, is the right primitive for ggOceanMaps: WMS returns pre-rendered RGB tiles (background imagery only); WCS returns numeric depth values, which can be recolored, contoured, and reused for `get_depth()`.
- Best sources (need URL verification at implementation time, services move):
  - **EMODnet Bathymetry** — WCS endpoint, ~115 m European waters, ~1/4 arc-sec global mosaics. Primary recommendation.
  - **GEBCO** — WMS/WCS, global, lower res. Backup / fallback.
- CRAN constraints: examples must not require internet; tests must `skip_on_cran()` + `skip_if_offline()`. Use small bundled fixture for tests. Client should respect timeout and degrade gracefully.

---

## Item-by-item plan

### 1) Make ggOceanMaps more useful for AI agents
Highest-leverage, lowest-cost item. Almost all gains from docs hygiene, not code.
- Add `AGENTS.md` (or extend `CLAUDE.md`) with: one-paragraph summary, minimal working example per scenario (point, transect, polar, bathymetry, custom shapefile), explicit "common pitfalls" list (DD limits order, `transform_coord` for projected layers, `datapath`/`userpath` options, antimeridian rotation).
- New `vignettes/cookbook.Rmd` — atomic copy-pasteable "How do I…?" recipes. LLMs retrieve by similarity; short atomic beats elaborate prose.
- Tighten roxygen `@examples` so every exported fn has an example that runs in <2 s without downloads.
- Keep canonical names stable (no renames in 2.4) — renames are LLM poison because old names are in training data.
- Skip machine-readable manifest; cookbook + AGENTS.md is enough.

### 2) WMS / WCS bathymetry on demand
- Path: **WCS**, not WMS. See research findings above.
- Implement as a new bathymetry *type*, not just a new style. Add `wcs_<source>_<color>` family (`wcs_emodnet_blues`, `wcs_gebco_blues`, …) that:
  1. Read map limits from `basemap_data` step.
  2. Issue WCS `GetCoverage` for bbox/CRS at resolution capped by `downsample` and a max area guard.
  3. Cache result under `getOption("ggOceanMaps.datapath")` keyed by `hash(bbox + source + resolution)` so repeat plots are free.
  4. Feed `stars` object into existing `bathy_rc` rendering path.
- Also expose a thin wrapper `wcs_bathymetry()` callable on its own for custom workflows.
- Why a subsystem, not a style switch: download/cache logic + error handling (offline, server down, bbox too big) + CRAN policy makes it separable.

**Decision (2026-05-22)**: opt-in alongside file-based. Default in 2.4.0 stays the current default (`raster_binned_blues` / built-in low-res) — changing the default would cause confusion. New WCS options must be well documented.

### 3) Finish `vector_land()`
- Move file from dev → `R/vector_land.R`.
- Fill in the "pick the land" step at line 36: mask cells where depth ≥ 0 (or NA above sea level — depends on `raster_bathymetry()` output convention).
- Replace `dplyr::recode_factor` (line 107) with base R or forcats. Don't add dplyr to Imports for this.
- Pair with `raster_bathymetry()` in docs, add a "build your own land/bathymetry" cookbook recipe.
- Add testthat test against tiny bundled raster.

### 4) EMODnet bathymetry
Covered by item 2 (EMODnet is the primary WCS source). User-facing question is live-WCS vs. user-supplied tile. Recommendation: support both, document EMODnet specifically in cookbook. `raster_user_*` already handles the file-based case via `ggOceanMaps.userpath`.

### 5) Baltic Sea Bathymetry Database (BSBD)
- Distributed by HELCOM/Sjöfartsverket. Access typically via download (registration). **No confirmed stable open OGC endpoint** — needs verifying before promising live download.
- Safer plan: provide a helper analogous to `geonorge_bathymetry()` that ingests a downloaded BSBD product into a `shapefiles = list(bathy = ...)`-ready object. Cookbook section. Defer until item 2 pattern is in place.

### 6) Triage Jules PRs (#55–#63)
| PR | Verdict | Rationale |
|---|---|---|
| #55 native-CRS clip | Review carefully, likely merge | Real perf win, but reverses a transform order — regression-test antimeridian rotation |
| #56 deprecated ggplot2 | Likely merge | Pure deletion; diff-check it's only dead branches |
| #57 grid line perf | Merge after benchmark | Plausible win, author didn't benchmark — verify equivalence on polar map |
| #58 LS tests | Merge | Establishes `tests/testthat/` infrastructure |
| #59 dead-code cleanup | Merge (squash with #61) | Pure deletion |
| #60 raster_bathymetry cleanup | Merge | Pure deletion |
| #61 larger dead-code cleanup | Merge (squash with #59) | Pure deletion |
| #62 unused parallel code | Read first | Could be scaffolding for future feature; if truly dead, merge |
| #63 quiet tests | Merge | Same as #58 |

Order: test-infra PRs (#58, #63) → cleanups in a batch → perf PRs (#55, #57) last with real benchmarks.

**Decision (2026-05-22)**: handle all 9 in one pass as I see best. Follow the recommended order (test-infra → cleanups → perf with benchmarks).

### 7) Articles for customizing / premade shapefiles / premade maps
- `_pkgdown.yml` has two article links commented out; `.Rmd` files already exist (151 + 323 lines). This is **wire-up + content review**, not from scratch.
- Steps: `pkgdown::build_articles()`, fix any errors, uncomment navbar links.
- Add new `vignettes/customising-shapefiles.Rmd` covering `clip_shapefile()`, `raster_bathymetry()` → `vector_bathymetry()` → (new) `vector_land()`, and `geonorge_bathymetry()`.

### 8) Ocean currents example
- Source: https://github.com/MikkoVihtakari/Barents-Sea-currents
- Right form: a cookbook recipe + short user-manual section ("Adding vector fields"). Not a standalone article — overkill for one example.
- Pattern: load NetCDF → downsample to regular grid → `geom_segment(aes(xend, yend))` arrows over `basemap()`. Mention `transform_coord()` for projections.

### 9) 2021 workshop deck
- Broken navbar link points to `https://aen-r-workshop.github.io/4-ggOceanMaps/`.
- Source: [`ggOceanMaps lecture/ggOceanMaps_workshop.Rmd`](../../../Meetings/2021_01%20ggOceanMaps%20workshop/ggOceanMaps%20lecture/ggOceanMaps_workshop.Rmd).
- **Decision (2026-05-22)**: host as a vignette. Remove AEN workshop references entirely. Fully update content for 2.4. Do this towards the end of the release cycle (phase 3).

### 10) Manual + documentation revision
- `vignettes/ggOceanMaps.Rmd` is one long file. Bathymetry section currently describes `type_resolution_color` parsing as a wall of text.
- Restructure bathymetry section as a decision tree: "Need bathymetry? → built-in binned blues / high-res download / your own raster (userpath) / live (new WCS) / custom shapefile."
- Split manual into: (a) Quickstart, (b) Bathymetry choices, (c) Custom shapefiles, (d) Projections, (e) Citations. Keep `ggOceanMaps.Rmd` as index.
- Sweep roxygen: stale parameter descriptions, missing `@return`, examples that download or fail offline, `@family` consistency.

### 11) Repo cleanup
- Candidates: `.Rproj.user`, regenerate `docs/` cleanly, prune sibling dev directories (`ggOceanMaps development/scrap`, `old functions`) — confirm these aren't tracked in repo first.
- Triage `.github/` workflows; R-CMD-check is the must-keep.
- Decide on `README.Rmd` vs `README.md` (keep both, regenerate `.md`).

### 12) Modern AI-based tests
Interpretation needs confirmation. Two plausible meanings:
- **(a)** AI-generated comprehensive testthat suite (snapshot tests for ggplot output via `vdiffr`, unit tests for helpers, integration tests for `basemap()` cases). With `skip_on_cran()` for heavy ones.
- **(b)** Tests verifying ggOceanMaps behaves well when called by an LLM (golden-path examples agents commonly produce).

**Decision (2026-05-22)**: option (a). Build a comprehensive testthat suite with `vdiffr` snapshot tests for ggplot output, unit tests for helpers, integration tests for `basemap()` cases. `skip_on_cran()` on heavy ones. Option (b) gets handled implicitly via the cookbook recipes in item 1.

---

## Suggested sequencing (three phases, each releasable as 2.4.0.900x dev versions)

**Phase 1 — Foundations (low risk, enables everything else)**
- Triage Jules PRs (item 6): test infra (#58, #63) first, then cleanups
- Set up `tests/testthat/` properly, add `vdiffr` (item 12)
- Finish `vector_land()` (item 3)
- AGENTS.md / cookbook scaffold (item 1)

**Phase 2 — New capabilities**
- WCS bathymetry subsystem + EMODnet style (items 2, 4)
- BSBD helper, file-based (item 5)
- Ocean currents recipe (item 8)
- `vector_land()` cookbook recipe (item 3 follow-up)

**Phase 3 — Docs & release**
- **Bathymetry vignette** (new — user request 2026-05-27):
  Create `vignettes/bathymetry.Rmd` as a dedicated Article. Sections:
  1. Shipped raster (`dd_rbathy`, no setup needed)
  2. High-res from ggOceanMapsLargeData (auto-download)
  3. Locally downloaded GEBCO/ETOPO (user-supplied NetCDF → `raster_bathymetry()` → `vector_bathymetry()`)
  4. Live WCS from EMODnet (`bathy.style = "wemb"`, coverage limits, citation requirement)
  5. Vector contours from any raster source
  Source: extract and rewrite the bathymetry section from `vignettes/ggOceanMaps.Rmd`; use GEBCO file path placeholder `path/to/your/bathymetry.nc` (no personal paths).
  Wire into `_pkgdown.yml` Articles.
- **Improve `bathy.style` docs in `basemap.Rd`**: replace the wall-of-text prose list with a structured table (columns: style alias, abbreviation, description, data needed). Add a "Choosing bathymetry" subsection with decision flow. 
- Manual restructure (item 10)
- Wire up + finish articles (item 7)
- Refresh workshop deck and re-link (item 9)
- Repo cleanup (item 11)
- CRAN dry-runs, bump to 2.4.0, submit

---

## Resolved decisions (2026-05-22)

1. **Item 12** → option (a): AI-generated comprehensive testthat + vdiffr suite, `skip_on_cran()` on heavy ones.
2. **Item 2** → WCS is opt-in alongside file-based. Default in 2.4.0 stays current default. Document new options well.
3. **Item 9** → host workshop as vignette, remove AEN references, fully update content for 2.4. Phase 3 timing.
4. **Item 6** → triage all 9 Jules PRs in one pass following the recommended order.

## How to use this document
- This is the long-form planning record. Update as items are addressed (strike through, add status notes, link to commits/PRs).
- All four design questions resolved 2026-05-22 — see "Resolved decisions" section. No blockers on starting any phase.
- Related memory: [[project_overview]], [[architecture]], [[feedback_style]].
