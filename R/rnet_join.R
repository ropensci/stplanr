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

