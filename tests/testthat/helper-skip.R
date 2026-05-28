skip_if_no_largedata <- function() {
  path <- getOption("ggOceanMaps.datapath", "")
  if (!nzchar(path) || !dir.exists(path)) {
    testthat::skip("ggOceanMapsLargeData download path is not configured")
  }
  # .onLoad sets datapath = tempdir() if the user hasn't configured one.
  # In non-interactive sessions (CI, R CMD check) that path exists but is
  # empty and the download prompt cannot be answered, so skip.
  td <- normalizePath(tempdir(), mustWork = FALSE)
  np <- normalizePath(path, mustWork = FALSE)
  if (!interactive() && identical(np, td)) {
    testthat::skip(
      "ggOceanMaps.datapath is tempdir (not user-configured); non-interactive session cannot prompt for download"
    )
  }
}

skip_if_no_userpath <- function() {
  path <- getOption("ggOceanMaps.userpath", "")
  testthat::skip_if(
    !nzchar(path) || !file.exists(path),
    "ggOceanMaps.userpath is not set or file is missing"
  )
}

skip_unless_visual <- function() {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  testthat::skip_if_not_installed("vdiffr")
}
