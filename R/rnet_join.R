#' Join route networks
#'
#' This is a spatial join function that is enables adding columns to a
#' 'target' route network from a 'source' route
#' network that contains the base geometry, e.g. from OSM
#'
#' The output is an sf object containing polygons representing
#' buffers around the route network in `rnet_x`.
#' The examples below demonstrate how to join attributes from
#' a route network object created with the function [overline()] onto
#' OSM geometries.
#'
#' Note: The main purpose of this function is to join an ID from `rnet_x`
#' onto `rnet_y`. Subsequent steps, e.g. with [dplyr::inner_join()]
#' are needed to join the attributes back onto `rnet_x`.
#' There are rarely 1-to-1 relationships between spatial network geometries
#' so we take care when using this function.
#'
#' See [#505](https://github.com/ropensci/stplanr/issues/505) for details
#' and a link to an interactive example of inputs and outputs shown below.
#'
#' @param rnet_x Target route network, the output will have the same geometries
#'   as features in this object.
#' @param rnet_y Source route network. Columns from this route network object will
#'   be copied across to the new network.
#' @param dist The buffer width around rnet_y in meters. 1 m by default.
#' @param length_y Add a new column called `length_y`? Useful when joining based on
#'   length of segments (e.g. weighted mean). `TRUE` by default.
#' @param key_column The index of the key (unique identifier) column in `rnet_x`.
#' @param subset_x Subset the source route network by the target network before
#'   creating buffers? This can lead to faster and better results. Default:
#'   `TRUE`.
#' @param dist_subset The buffer distance in m to apply when breaking up the
#'   source object `rnet_y`. Default: 5.
#' @param split_y Should the second route network be split at the start and
#'   end points of LINESTRING features in the first? `TRUE` by default.
#' @param ... Additional arguments passed to `rnet_subset`.
#' @examples
#' library(sf)
#' library(dplyr)
#' # Uncomment for interactive examples:
#' plot(route_network_small["flow"])
#' plot(osm_net_example$geometry, lwd = 5, col = "grey")
#' plot(route_network_small["flow"], add = TRUE)
#' rnetj = rnet_join(osm_net_example, route_network_small, dist = 9)
#' # library(mapview)
#' # mapview(rnetj, zcol = "flow") +
#' #  mapview(route_network_small, zcol = "flow")
#' plot(rnetj["flow"])
#' plot(route_network_small["flow"], add = TRUE)
#' rnetj_summary = rnetj %>%
#'   sf::st_drop_geometry() %>%
#'   group_by(osm_id) %>%
#'     summarise(
#'       flow = weighted.mean(flow, length_y, na.rm = TRUE),
#'       )
#' osm_joined_rnet = left_join(osm_net_example, rnetj_summary)
#' plot(route_network_small["flow"])
#' plot(osm_joined_rnet[c("flow")])
#' # Improve fit between geometries and performance by subsetting rnet_x
#' osm_subset = rnet_subset(osm_net_example, route_network_small, dist = 5)
#' osm_joined_rnet = left_join(osm_subset, rnetj_summary)
#' plot(route_network_small["flow"])
#' plot(osm_joined_rnet[c("flow")])
#' # mapview(joined_network) +
#' #   mapview(route_network_small)
#' @export
rnet_join = function(rnet_x, rnet_y, dist = 5, length_y = TRUE, key_column = 1,
                     subset_x = TRUE, dist_subset = 5, split_y = TRUE, ...) {
  if (subset_x) {
    rnet_x = rnet_subset(rnet_x, rnet_y, dist = dist_subset, ...)
  }
  rnet_x_buffer = geo_buffer(rnet_x, dist = dist, nQuadSegs = 2)
  if (split_y) {
    rnet_y = rnet_split_lines(rnet_y, rnet_x, dist = dist_subset)
  }
  if (length_y) {
    rnet_y$length_y = as.numeric(sf::st_length(rnet_y))
  }
  rnetj = sf::st_join(rnet_x_buffer[key_column], rnet_y, join = sf::st_contains)
  rnetj
}

#' Subset one route network based on overlaps with another
#'
#' @param rnet_x The route network to be subset
#' @param rnet_y The subsetting route network
#' @param dist The buffer width around y in meters. 1 m by default.
#' @param crop Crop `rnet_x`? `TRUE` is the default
#' @param min_x Segments shorter than this multiple of dist
#'   *and* which were longer
#'   before the cropping process will be removed. 3 by default.
#' @export
rnet_subset = function(rnet_x, rnet_y, dist = 1, crop = TRUE, min_x = 3) {
  rnet_x$length_x_original = as.numeric(sf::st_length(rnet_x))
  rnet_y_union = sf::st_union(rnet_y)
  rnet_y_buffer = stplanr::geo_buffer(rnet_y_union, dist = dist, nQuadSegs = 2)
  if(crop) {
    rnet_x = sf::st_intersection(rnet_x, rnet_y_buffer)
    rnet_x = line_cast(rnet_x)
    rnet_x$length_x_cropped = as.numeric(sf::st_length(rnet_x))
    min_length = dist * min_x
    sel_short = rnet_x$length_x_cropped < min_length &
      rnet_x$length_x_original > min_length
    rnet_x = rnet_x[!sel_short, ]
  } else {
    rnet_x[rnet_y_buffer, , op = sf::st_within]
  }
  rnet_x
}
#' Split lines in a route network based points
#'
#' If the object passed to the second argument has LINSTRING geometries
#'   the start and end points of linestrings are used.
#'
#' @param rnet_x The route network to be broken into smaller pieces
#' @param geo_y The geographic object used to break up the route network
#' @param dist The width of the buffer used when breaking up the route network.
#'   For imprecise data it may be worth increasing this above 1 m, the default.
#' @export
rnet_split_lines = function(rnet_x, geo_y, dist = 1) {
  if (all(grepl(pattern = "LINE", x = sf::st_geometry_type(rnet_x)))) {
    geo_y = c(
      lwgeom::st_startpoint(geo_y),
      lwgeom::st_endpoint(geo_y)
    )
  }
  # speed-up subsequent steps:
  points = sf::st_union(geo_y)
  points_buffer = stplanr::geo_buffer(points, dist = dist)
  rnet_split = sf::st_difference(rnet_x, points_buffer)
  rnet_split_lines = line_cast(rnet_split)
  rnet_split_lines$length_osm_cast = as.numeric(sf::st_length(rnet_split_lines))
  # rnet_split_lines[rnet_split_lines$length_osm_cast > min_lenth, ]
  rnet_split_lines
}

#' Convert multilinestring object into linestrings
#'
#' Without losing vertices
#'
#' @param x Linestring object
#' @export
line_cast = function(x) {
  sf::st_cast(sf::st_cast(x, "MULTILINESTRING"), "LINESTRING")
}
