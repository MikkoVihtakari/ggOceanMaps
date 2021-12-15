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
#' @import ggplot2
#' @author Mikko Vihtakari
#' @seealso \code{\link{basemap}}

map_cmd <- function(command, alternative = FALSE) {
  out <- switch(command,
    base = '
      ggplot2::ggplot()
    ',
    bathy_pb = '
      ggspatial::layer_spatial(data = X$shapefiles$bathy, aes(fill = depth), show.legend = bathy.legend, color = bathy.border.col, size = bathy.size) +
      scale_fill_manual(name = "Depth (m)", values = colorRampPalette(c("#F7FBFF", "#DEEBF7", "#9ECAE1", "#4292C6", "#08306B"))(nlevels(X$shapefiles$bathy@data$depth)), guide =
        if(bathy.legend) {
          guide_legend(order = 1, override.aes = list(colour = NA))
        } else {
          "none"
        })
    ',
    bathy_pg = '
      ggspatial::layer_spatial(data = X$shapefiles$bathy, aes(fill = depth), show.legend = bathy.legend, color = bathy.border.col, size = bathy.size) +
      scale_fill_grey("Depth (m)", start = 1, end = 0.5, guide =
        if(bathy.legend) {
          guide_legend(order = 1, override.aes = list(colour = NA))
        } else {
          "none"
        })
      ',
    bathy_cg = '
      ggspatial::layer_spatial(data = X$shapefiles$bathy, fill = NA, color = bathy.border.col, size = bathy.size)
    ',
    bathy_cb = '
      ggspatial::layer_spatial(data = X$shapefiles$bathy, aes(color = depth), show.legend = bathy.legend, fill = NA, size = land.size) +
      scale_color_manual(name = "Depth (m)", values = colorRampPalette(c("#DEEBF7", "#9ECAE1", "#4292C6", "#08306B", "#76536F"))(nlevels(X$shapefiles$bathy@data$depth)))
    ',
    land = '
      ggspatial::layer_spatial(data = X$shapefiles$land, fill = land.col, color = land.border.col, size = land.size)
    ',
    glacier = '
      ggspatial::layer_spatial(data = X$shapefiles$glacier, fill = gla.col, color = gla.border.col, size = gla.size)
    ',
    defs_rect = '
      scale_y_continuous(breaks = X$map.grid$lat.breaks, expand = c(0,0.1)) +
      scale_x_continuous(breaks = X$map.grid$lon.breaks, expand = c(0,0.1)) +
      labs(y = "Latitude (decimal degrees)", x = "Longitude (decimal degrees)") + {
        if(packageVersion("ggplot2") > "3.3.3")
        coord_sf(xlim = X$map.limits[1:2], ylim = X$map.limits[3:4],
                 default_crs = NULL, crs = sf::st_crs(X$proj), default = TRUE)
      } + {
        if(packageVersion("ggplot2") <= "3.3.3") 
        coord_sf(xlim = X$map.limits[1:2], ylim = X$map.limits[3:4],
                 crs = sf::st_crs(X$proj), default = TRUE)
      } +
      theme_map(base_size = base_size, grid.col = grid.col, grid.size = grid.size) +
      theme(legend.position = legend.position,
      legend.margin=margin(t = 0.2, b = 0, unit = "cm")
      )
    ',
    defs_rect_proj = '
      labs(y = "Latitude (meters)", x = "Longitude (meters)") + {
        if(packageVersion("ggplot2") > "3.3.3")
        coord_sf(xlim = X$map.limits[1:2], ylim = X$map.limits[3:4], expand = FALSE,
                 default_crs = NULL, crs = sf::st_crs(X$proj), datum = sf::st_crs(X$proj), 
                 default = TRUE)
      } + {
        if(packageVersion("ggplot2") <= "3.3.3") 
        coord_sf(xlim = X$map.limits[1:2], ylim = X$map.limits[3:4], expand = FALSE,
                 crs = sf::st_crs(X$proj), datum = sf::st_crs(X$proj), default = TRUE)
      } +
      theme_map(base_size = base_size, grid.col = grid.col, grid.size = grid.size) +
      theme(legend.position = legend.position,
      legend.margin=margin(t = 0.2, b = 0, unit = "cm")
      )
    ',
    defs_polar = '
    {
    if(!is.na(grid.col)) ggspatial::geom_spatial_path(data = X$map.grid$lon.grid.lines, aes(x = lon, y = lat), crs = 4326, color = grid.col, size = grid.size)
    } + {
    if(!is.na(grid.col)) ggspatial::geom_spatial_path(data = X$map.grid$lat.grid.lines, aes(x = lon, y = lat), crs = 4326, color = grid.col, size = grid.size)
    } +
    ggspatial::geom_spatial_path(data = X$map.grid$lat.limit.line, aes(x = lon, y = lat), crs = 4326, color = land.border.col, size = land.size) + {
        if(packageVersion("ggplot2") > "3.3.3")
        coord_sf(xlim = X$map.limits[1:2], ylim = X$map.limits[3:4], expand = FALSE,
                 default_crs = NULL, crs = sf::st_crs(X$proj), default = TRUE)
      } + {
        if(packageVersion("ggplot2") <= "3.3.3") 
        coord_sf(xlim = X$map.limits[1:2], ylim = X$map.limits[3:4], expand = FALSE,
                 crs = sf::st_crs(X$proj), default = TRUE)
      } +
    theme_void(base_size = base_size) +
    theme(legend.position = legend.position,
          legend.margin = margin(t = 0.2, b = 0, r = 0.2, unit = "cm")
    )
    ',
    defs_polar_proj = '
    labs(y = "Latitude (meters)", x = "Longitude (meters)") +
    ggspatial::geom_spatial_path(data = X$map.grid$lat.limit.line, aes(x = lon, y = lat), crs = 4326, color = land.border.col, size = land.size) + {
        if(packageVersion("ggplot2") > "3.3.3")
        coord_sf(xlim = X$map.limits[1:2], ylim = X$map.limits[3:4], expand = FALSE,
                 default_crs = NULL, crs = sf::st_crs(X$proj), datum = sf::st_crs(X$proj), 
                 default = TRUE)
      } + {
        if(packageVersion("ggplot2") <= "3.3.3") 
        coord_sf(xlim = X$map.limits[1:2], ylim = X$map.limits[3:4], expand = FALSE,
                 crs = sf::st_crs(X$proj), datum = sf::st_crs(X$proj), default = TRUE)
      } +
    theme_map(base_size = base_size, grid.col = grid.col, grid.size = grid.size) +
    theme(legend.position = legend.position,
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

