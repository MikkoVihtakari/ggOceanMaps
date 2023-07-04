#' @title Return map elements for basemap
#' @description An internal function to make \code{\link{basemap}} code more readable
#' @param command basemap layer to be added
#' @param alternative logical to return alternative formmatting in certain cases. Used to reduce \code{if}-\code{else} statements in \code{\link{basemap}}.
#' @details This is an internal function, which is automatically run by the \code{\link{basemap}} function. Common users do not need to worry about these details. Basemap elements can added together using this function, \code{\link[base]{parse}} and \code{\link[base]{eval}}.
#' @return A character string containing a ggplot2 plotting command. Use \code{eval(parse(text=...))} to plot the string. 
#' @examples ## An example for utm map without glaciers or bathymetry
#' \dontrun{eval(parse(text=paste(map_cmd("base"), map_cmd("land_utm"),
#' map_cmd("grid_utm"), map_cmd("defs_utm"), sep = "+")))}
#' @keywords internal
#' @export
#' @author Mikko Vihtakari
#' @seealso \code{\link{basemap}}

map_cmd <- function(command, alternative = FALSE) {
  out <- switch(command,
    base = '
      ggplot2::ggplot()
    ',
    bathy_rb = '
    stars::geom_stars(data = X$shapefiles$bathy$raster, na.action = na.omit,
      downsample = downsample, show.legend = bathy.legend, alpha = bathy.alpha) 
    ',
    bathy_rc = '
    stars::geom_stars(data = X$shapefiles$bathy$raster, downsample = downsample,
      show.legend = bathy.legend, alpha = bathy.alpha)
    ',
    bathy_rbb_scale = '
    ggplot2::scale_fill_manual(
      name = "Depth (m)", 
      values = colorRampPalette(c("#F7FBFF", "#DEEBF7", "#9ECAE1", "#4292C6", "#08306B"))(nlevels(X$shapefiles$bathy$raster[[1]])),
      guide =
        if(bathy.legend) {
            guide_legend(order = 1, override.aes = list(colour = NA))
          } else {
            "none"
        })
    ',
    bathy_rbg_scale = '
    scale_fill_grey("Depth (m)", start = 1, end = 0.5, guide =
        if(bathy.legend) {
          guide_legend(order = 1, override.aes = list(colour = NA))
        } else {
          "none"
        })
      ',
    bathy_rcb_scale = '
    ggplot2::scale_fill_gradientn(
        name = "Depth (m)", 
        limits = c(0,NA), 
        values = c(0,0.01,0.05,0.25,0.75,1),
        colors = colorRampPalette(c("#F7FBFF", "#DEEBF7", "#9ECAE1", "#4292C6", "#08306B"))(10),
        na.value = "white"
        )
    ',
    bathy_rcg_scale = '
    scale_fill_distiller(
        name = "Depth (m)", 
        limits = c(0,NA), 
        type = "seq",
        direction = 1,
        palette = "Greys",
        na.value = "white"
        )
      ',
    # bathy_rbb = 
    #   'stars::geom_stars(data = X$shapefiles$bathy$raster, na.action = na.omit,
    #   show.legend = bathy.legend, alpha = bathy.alpha) +
    #   ggplot2::scale_fill_manual(
    #     name = "Depth (m)", 
    #     values = colorRampPalette(c("#F7FBFF", "#DEEBF7", "#9ECAE1", "#4292C6", "#08306B"))(nlevels(X$shapefiles$bathy$raster[[1]])),
    #     guide =
    #       if(bathy.legend) {
    #         guide_legend(order = 1, override.aes = list(colour = NA))
    #       } else {
    #         "none"
    #       })
    # ',
    # bathy_rcb = 
    #   'stars::geom_stars(data = X$shapefiles$bathy$raster,
    #   show.legend = bathy.legend, alpha = bathy.alpha) +
    #   ggplot2::scale_fill_gradientn(
    #     name = "Depth (m)", limits = c(0,NA), 
    #     values = c(0,0.01,0.05,0.25,0.75,1),
    #     colors = colorRampPalette(c("#F7FBFF", "#DEEBF7", "#9ECAE1", "#4292C6", "#08306B"))(10),
    #     na.value = "white"
    #     )
    # ',
    # bathy_rbg = '
    #   stars::geom_stars(data = X$shapefiles$bathy$raster, na.action = na.omit,
    #   show.legend = bathy.legend, alpha = bathy.alpha) +
    #   scale_fill_grey("Depth (m)", start = 1, end = 0.5, guide =
    #     if(bathy.legend) {
    #       guide_legend(order = 1, override.aes = list(colour = NA))
    #     } else {
    #       "none"
    #     })
    #   ',
    bathy_pb = '
      ggplot2::geom_sf(data = X$shapefiles$bathy, ggplot2::aes(fill = depth), 
      show.legend = bathy.legend, color = bathy.border.col, 
      size = bathy.size, alpha = bathy.alpha) +
      ggplot2::scale_fill_manual(name = "Depth (m)", 
      values = colorRampPalette(c("#F7FBFF", "#DEEBF7", "#9ECAE1", "#4292C6", "#08306B"))(nlevels(X$shapefiles$bathy$depth)), 
      guide =
        if(bathy.legend) {
          guide_legend(order = 1, override.aes = list(colour = NA))
        } else {
          "none"
        })
    ',
    bathy_pg = '
      ggplot2::geom_sf(data = X$shapefiles$bathy, ggplot2::aes(fill = depth), 
      show.legend = bathy.legend, color = bathy.border.col, size = bathy.size, 
      alpha = bathy.alpha) +
      scale_fill_grey("Depth (m)", start = 1, end = 0.5, guide =
        if(bathy.legend) {
          guide_legend(order = 1, override.aes = list(colour = NA))
        } else {
          "none"
        })
      ',
    bathy_cg = '
      ggplot2::geom_sf(data = X$shapefiles$bathy, fill = NA, 
      color = bathy.border.col, size = bathy.size)
    ',
    bathy_cb = '
      ggplot2::geom_sf(data = X$shapefiles$bathy, ggplot2::aes(color = depth),
      show.legend = bathy.legend, fill = NA, size = land.size) +
      ggplot2::scale_color_manual(name = "Depth (m)",
      values = colorRampPalette(c("#DEEBF7", "#9ECAE1", "#4292C6", "#08306B", "#76536F"))(nlevels(X$shapefiles$bathy$depth)))
    ',
    land = '
      ggplot2::geom_sf(data = X$shapefiles$land, fill = land.col, 
      color = land.border.col, size = land.size)
    ',
    glacier = '
      ggplot2::geom_sf(data = X$shapefiles$glacier, fill = gla.col, 
      color = gla.border.col, size = gla.size)
    ',
    defs_rect = '
      ggplot2::scale_y_continuous(breaks = X$map.grid$lat.breaks, expand = c(0,0.1)) +
      ggplot2::scale_x_continuous(breaks = X$map.grid$lon.breaks, expand = c(0,0.1)) +
      ggplot2::labs(y = "Latitude (decimal degrees)", x = "Longitude (decimal degrees)") + 
      ggplot2::coord_sf(xlim = X$map.limits[1:2], ylim = X$map.limits[3:4], expand = FALSE,
               crs = sf::st_crs(X$proj), default = TRUE) +
      theme_map(base_size = base_size, grid.col = grid.col, grid.size = grid.size) +
      ggplot2::theme(legend.position = legend.position,
      legend.margin=margin(t = 0.2, b = 0, unit = "cm")
      )
    ',
    defs_rect_proj = '
      ggplot2::labs(y = "Latitude (meters)", x = "Longitude (meters)") + 
      ggplot2::coord_sf(xlim = X$map.limits[1:2], ylim = X$map.limits[3:4], expand = FALSE,
               crs = sf::st_crs(X$proj), datum = sf::st_crs(X$proj), default = TRUE) +
      theme_map(base_size = base_size, grid.col = grid.col, grid.size = grid.size) +
      ggplot2::theme(legend.position = legend.position,
      legend.margin=margin(t = 0.2, b = 0, unit = "cm")
      )
    ',
    defs_polar = '
    {
    if(!is.na(grid.col)) ggplot2::geom_sf(data = X$map.grid$lon.grid.lines, color = grid.col, linewidth = grid.size)
    } + {
    if(!is.na(grid.col)) ggplot2::geom_sf(data = X$map.grid$lat.grid.lines, color = grid.col, linewidth = grid.size)
    } +
    ggplot2::geom_sf(data = X$map.grid$lat.limit.line, color = land.border.col, size = land.size) +
    ggplot2::coord_sf(xlim = X$map.limits[1:2], ylim = X$map.limits[3:4], expand = FALSE,
             crs = sf::st_crs(X$proj), default = TRUE) +
    ggplot2::theme_void(base_size = base_size) +
    ggplot2::theme(legend.position = legend.position,
          legend.margin = margin(t = 0.2, b = 0, r = 0.2, unit = "cm")
    )
    ',
    defs_polar_proj = '
    ggplot2::labs(y = "Latitude (meters)", x = "Longitude (meters)") +
    ggplot2::geom_sf(data = X$map.grid$lat.limit.line, color = land.border.col, size = land.size) +
    ggplot2::coord_sf(xlim = X$map.limits[1:2], ylim = X$map.limits[3:4], expand = FALSE,
             crs = sf::st_crs(X$proj), datum = sf::st_crs(X$proj), default = TRUE) +
    theme_map(base_size = base_size, grid.col = grid.col, grid.size = grid.size) +
    ggplot2::theme(legend.position = legend.position,
          legend.margin = margin(t = 0.2, b = 0, r = 0.2, unit = "cm")
    )
    ',
    # Scrap stuff. Delete when used
    #
    # labels_polar = '
    #   geom_text(data = X$Grid$lon, aes(x = label.offset*lon.end, y = label.offset*lat.end, angle = angle, label = paste(label, "^o", sep = "")), size = FS(label.font), parse = TRUE) +
    #   geom_text(data = X$Grid$lat.breaks, aes(x = lon.utm, y = lat.utm, label = paste(label, "^o", sep = "")), hjust = 0, vjust = 0, size = FS(label.font), parse = TRUE)
    # ',
    # labels_polar_limits = '
    #   scale_y_continuous(labels = formatterUTMkm) +
    #   scale_x_continuous(labels = formatterUTMkm) +
    #   labs(x = "Longitude (km)", y = "Latitude (km)")
    # ',
    # defs_polar_limits = '
    #   coord_fixed(xlim = c(X$Grid$boundaries$lon.utm[1], X$Grid$boundaries$lon.utm[2]), ylim = c(X$Grid$boundaries$lat.utm[1], X$Grid$boundaries$lat.utm[2]), expand = FALSE) +
    #   theme_map(base_size = base_size) + theme(legend.key.height = unit(0.4, "cm"))
    # ',
    # remove_labels = '
    #   theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())
    # ',
    stop(paste("map command", command, "not found."))
  )

  trimws(gsub("\n", " ", out))
}

