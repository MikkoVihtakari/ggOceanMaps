#' @title A ggplot2 theme for maps
#' @description A ggplot2 theme for maps.
#' @param grid.col Character code specifying the color of grid lines. Use \code{NA} to remove the grid lines.
#' @param grid.size Numeric value specifying the width of grid lines.
#' @param ... additional arguments passed to \code{\link[ggplot2]{ggtheme}}.
#' @import ggplot2
#' @return A ggplot2 theme layer.
#' @family customize shapefiles
#' @export

theme_map <- function(..., grid.col, grid.size) {
    theme_bw(...) %+replace%
      theme(panel.background = element_blank(),
      panel.border = element_rect(fill = NA, colour = "black", size = 0.2),
      panel.grid = element_line(colour = grid.col, linewidth = grid.size),
      plot.background = element_blank(),
      axis.ticks.x = element_line(colour = "black", size = 0.2),
      axis.ticks.y = element_line(colour = "black", size = 0.2),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      panel.ontop = TRUE
      )
  }
