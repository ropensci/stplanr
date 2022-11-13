#' Spatial join function that is designed to add columns to a 'target' route network
#'   from a 'source' route network that contains the base geometry, e.g. from OSM
#'
#' @examples
#' library(sf)
#' plot(osm_net_example$geometry, lwd = 5, col = "grey")
#' route_network_sf$flow = 1:nrow(route_network_small)
#' plot(route_network_small["flow"], add = TRUE)
#' joined_network = rnet_join(osm_net_example, route_network_sf)
#' @export
rnet_join = function(x, y) {

}

rnet_subset = function(rnet_x, rnet_y, dist = 1, crop = TRUE, min_length = 3) {
  rnet_x$length_x_original = as.numeric(sf::st_length(rnet_x))
  rnet_y_union = sf::st_union(rnet_y)
  rnet_y_buffer = stplanr::geo_buffer(rnet_y_union, dist = dist, nQuadSegs = 2)
  if(crop) {
    rnet_x = sf::st_intersection(rnet_x, rnet_y_buffer)
    rnet_x = line_cast(rnet_x)
    rnet_x$length_x_cropped = as.numeric(sf::st_length(rnet_x))
    sel_short = rnet_x$length_x_cropped < min_length &
      rnet_x$length_x_original > min_length
    rnet_x = rnet_x[!sel_short, ]
  } else {
    rnet_x[rnet_y_buffer, , op = sf::st_within]
  }
  rnet_x
}

rnet_split_lines = function(rnet_x, rnet_y, dist = 1) {
  osm_start_end_points = c(
    lwgeom::st_startpoint(rnet_y),
    lwgeom::st_endpoint(rnet_y)
  ) %>%
    sf::st_union()

  ## Linking to GEOS 3.10.2, GDAL 3.4.1, PROJ 8.2.1; sf_use_s2() is TRUE
  # browser()
  osm_points_buffer = stplanr::geo_buffer(osm_start_end_points, dist = dist)
  rnet_split = sf::st_difference(rnet_x, osm_points_buffer)
  rnet_split_lines = line_cast(rnet_split)
  rnet_split_lines$length_osm_cast = as.numeric(sf::st_length(rnet_split_lines))
  # rnet_split_lines[rnet_split_lines$length_osm_cast > min_lenth, ]
  rnet_split_lines
}

rnet_join = function(x, y, dist = 1, break_y = TRUE) {
  x_buffer = stplanr::geo_buffer(x, dist = dist, nQuadSegs = 2)
  y$length_m_geometry_y = sf::st_length(y) %>% as.numeric()
  sf::st_join(x_buffer[1], y, join = sf::st_contains)
}


line_cast = function(x) {
  sf::st_cast(sf::st_cast(x, "MULTILINESTRING"), "LINESTRING")
}
