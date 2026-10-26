# Run after pkgdown builds the site; fail before uploading a stale version.
args <- commandArgs(trailingOnly = TRUE)
site <- if (length(args)) args[[1]] else "docs"
metadata <- read.dcf("DESCRIPTION")
version <- unname(metadata[1, "Version"])
index <- file.path(site, "index.html")
if (!file.exists(index)) stop("Missing site homepage: ", index)
html <- xml2::read_html(index)
node <- xml2::xml_find_first(html, "//nav//small[contains(@class, 'nav-text')]")
actual <- trimws(xml2::xml_text(node))
if (is.na(actual) || !identical(actual, version)) {
  stop("Website version ", actual, " differs from DESCRIPTION version ", version)
}
text <- xml2::xml_text(html)
if (grepl("Setting data download folder to a temporary folder", text, fixed = TRUE) ||
    grepl("(/var/folders/|/tmp/|/home/runner/)[^[:space:]]*Rtmp", text)) {
  stop("Package startup diagnostics or temporary paths leaked into the homepage.")
}
claims <- regmatches(text, gregexpr("R package version [0-9]+\\.[0-9]+\\.[0-9]+", text))[[1]]
if (length(claims) && any(sub("R package version ", "", claims, fixed = TRUE) != version)) {
  stop("The homepage contains a stale package-version statement or citation.")
}
message("Website version verified: ", metadata[1, "Package"], " ", version)
