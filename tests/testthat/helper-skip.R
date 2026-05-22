skip_if_no_largedata <- function() {
  path <- getOption("ggOceanMaps.datapath", "")
  testthat::skip_if(
    !nzchar(path) || !dir.exists(path),
    "ggOceanMapsLargeData download path is not configured"
  )
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
