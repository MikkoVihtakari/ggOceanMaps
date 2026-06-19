# cran-comments

## Submission

This is a major update (3.0.0) of ggOceanMaps, which is already on CRAN.

It adds on-demand WCS bathymetry (`wcs_bathymetry()`), a `vector_land()` helper
for build-your-own shapefiles, a comprehensive automated test suite, several
clipping bug fixes, and an expanded documentation website. The package backend
remains `sf`/`stars`.

## Test environments

- local macOS, R release
- (please also note CI runs R CMD check on macOS, Windows, and Ubuntu
  (devel/release/oldrel-1) via GitHub Actions)

## R CMD check results

0 errors | 0 warnings | 1 note

The note is from `checking CRAN incoming feasibility` and reports the
maintainer plus one URL in `man/basemap.Rd`:

```
Found the following (possibly) invalid URLs:
  URL: https://mikkovihtakari.github.io/ggOceanMaps/articles/bathymetry.html
    Status: 404
```

This is a newly added article on the package's pkgdown website. The URL becomes
valid once the updated website is deployed (at the time of submission). The
page is part of this release and the link is correct.

## Notes for reviewers

- The package downloads higher-resolution map data on demand from the
  `ggOceanMapsLargeData` GitHub repository to a user-configured cache directory;
  examples that need these data are wrapped in `\dontrun{}` / `\donttest{}`.
- Vignettes are built for the website only and are excluded from the source
  tarball (they require large external downloads to render).
