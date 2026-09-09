#' @title Raster bathymetry layer for basemap
#' @description An internal replacement for \code{\link[stars]{geom_stars}} used
#'   to plot raster bathymetries in \code{\link{basemap}}.
#' @param x A \code{stars} object containing the bathymetry raster. See
#'   \code{\link[stars]{st_as_stars}}.
#' @param downsample Integer passed to \code{\link[stars]{st_downsample}}. 0
#'   (default) does not downsample.
#' @param show.legend Logical indicating whether the fill legend should be shown.
#' @param alpha Transparency of the raster (0-1).
#' @details This is an internal function which is automatically run by
#'   \code{\link{basemap}}. Common users do not need to worry about these
#'   details.
#'
#'   \code{stars::geom_stars()} turns the entire raster into a tibble and, when
#'   called with \code{na.action = na.omit}, keeps a named index of every
#'   dropped cell in the \code{"na.action"} attribute. For the million-cell
#'   rasters \code{basemap()} routinely produces, that attribute is larger than
#'   the plotted data itself. This function instead extracts only the cells that
#'   contain data, and hands them to \code{GeomBathyRaster}, a
#'   \code{\link[ggplot2]{GeomRaster}} variant that neither materialises the
#'   \code{xmin}/\code{xmax}/\code{ymin}/\code{ymax} columns (four doubles per
#'   cell, needed only for the extent of the raster as a whole) nor warns about
#'   uneven pixel intervals when the raster contains entirely empty columns or
#'   rows (issue #69). Rasters that are not regular grids are passed on to
#'   \code{stars::geom_stars()}.
#' @return A ggplot2 layer.
#' @importFrom grid rasterGrob
#' @keywords internal
#' @author Mikko Vihtakari
#' @seealso \code{\link{basemap}}

bathy_raster_layer <- function(x, downsample = 0, show.legend = TRUE, alpha = 1) {

  if(any(downsample > 0)) x <- stars::st_downsample(x, downsample)

  d <- stars::st_dimensions(x)

  # Anything but a plain two-dimensional regular grid (curvilinear,
  # rectilinear, extra dimensions such as band or time) is left to stars, which
  # picks a suitable geom for it.
  if(!is_regular_2d_grid(x)) {
    return(
      stars::geom_stars(data = x, na.action = stats::na.omit,
                        show.legend = show.legend, alpha = alpha)
    )
  }

  xy <- attr(d, "raster")$dimensions
  vals <- x[[1]]

  if(inherits(vals, "units")) vals <- units::drop_units(vals)

  # Cell centres along each axis, and the (row, column) of every cell that
  # contains data. Indices are derived arithmetically instead of expanding the
  # full coordinate grid, which would allocate two doubles for every empty cell.
  x_vals <- stars::st_get_dimension_values(x, xy[1], center = TRUE)
  y_vals <- stars::st_get_dimension_values(x, xy[2], center = TRUE)

  keep <- which(!is.na(vals))
  nx <- length(x_vals)

  out <- data.frame(
    x = x_vals[((keep - 1L) %% nx) + 1L],
    y = y_vals[((keep - 1L) %/% nx) + 1L],
    fill = vals[keep]
  )

  ggplot2::layer(
    data = out,
    mapping = ggplot2::aes(x = .data$x, y = .data$y, fill = .data$fill),
    stat = "identity",
    position = "identity",
    geom = GeomBathyRaster,
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = list(alpha = alpha)
  )
}

#' @rdname bathy_raster_layer
#' @format NULL
#' @usage NULL
#' @keywords internal
GeomBathyRaster <- ggplot2::ggproto(
  "GeomBathyRaster", ggplot2::GeomRaster,
  non_missing_aes = "fill",

  # GeomRaster expands the data with xmin/xmax/ymin/ymax, four doubles per
  # cell, although draw_panel() only ever uses their overall range. It also
  # warns when the coordinates are unevenly spaced, which happens whenever the
  # raster contains an entirely empty column or row: those cells are dropped
  # before plotting, leaving a gap in the coordinate sequence. The cells are
  # still drawn in the right place because draw_panel() positions them by
  # integer index, so the warning is spurious here (issue #69).
  setup_data = function(data, params) {
    data
  },

  draw_panel = function(self, data, panel_params, coord, interpolate = FALSE,
                        hjust = 0.5, vjust = 0.5) {

    # The cell size is deliberately measured from the plotted coordinates
    # rather than taken from the raster's `delta`: the two differ by a
    # floating-point margin that is enough to shift the outermost cells by a
    # whole column when the cell positions are truncated to integers below.
    w <- cell_size(data$x)
    h <- cell_size(data$y)

    if(!coord$is_linear()) {
      # Non-linear coordinates cannot be drawn as a single raster. Draw the
      # cells as rectangles instead, which is what ggplot2::GeomRaster does in
      # this situation, adding the corner columns it would otherwise have
      # computed in setup_data().
      data$xmin <- data$x - w * (1 - hjust)
      data$xmax <- data$x + w * hjust
      data$ymin <- data$y - h * (1 - vjust)
      data$ymax <- data$y + h * vjust
      data$linewidth <- 0.3
      data$colour <- data$fill
      return(ggplot2::GeomRect$draw_panel(data, panel_params, coord))
    }

    x_pos <- as.integer((data$x - min(data$x)) / w)
    y_pos <- as.integer((data$y - min(data$y)) / h)

    nrow <- max(y_pos) + 1
    ncol <- max(x_pos) + 1

    raster <- matrix(NA_character_, nrow = nrow, ncol = ncol)
    raster[cbind(nrow - y_pos, x_pos + 1)] <-
      ggplot2::alpha(data$fill, data$alpha)

    # Only the extent of the raster as a whole has to pass through the
    # coordinate system, so transform its two corners instead of every cell.
    corners <- data.frame(
      x = c(min(data$x) - w * (1 - hjust), max(data$x) + w * hjust),
      y = c(min(data$y) - h * (1 - vjust), max(data$y) + h * vjust)
    )
    corners <- coord$transform(corners, panel_params)

    grid::rasterGrob(
      raster,
      x = mean(corners$x), y = mean(corners$y),
      width = diff(range(corners$x)), height = diff(range(corners$y)),
      default.units = "native", interpolate = interpolate
    )
  }
)

## Cell size along one axis: the smallest distance between two neighbouring
## cell centres. Mirrors ggplot2::resolution(x, zero = FALSE) without warning
## when the spacing is uneven.
cell_size <- function(x) {
  d <- diff(sort(unique(as.numeric(x))))
  if(length(d) == 0) 1 else min(d)
}

## TRUE when x is a plain two-dimensional regular grid, i.e. a raster whose
## cells are evenly spaced along both axes and which carries no further
## dimensions. stars has no exported predicate for this.
is_regular_2d_grid <- function(x) {
  d <- stars::st_dimensions(x)
  r <- attr(d, "raster")

  if(is.null(r) || isTRUE(r$curvilinear)) return(FALSE)
  if(length(dim(x)) != 2 || length(x) != 1) return(FALSE)
  # The x dimension must come first in the array, since bathy_raster_layer()
  # derives cell positions from the linear index of the array.
  if(!identical(unname(r$dimensions), names(d))) return(FALSE)

  all(vapply(r$dimensions, function(i) {
    is.null(d[[i]]$values) && !is.na(d[[i]]$delta)
  }, logical(1)))
}
