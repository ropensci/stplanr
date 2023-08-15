# This needs a rethink:

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
rnet_split_lines = function(rnet_y, split_y = 10) {
  # Require qgisprocess, stop if not installed:
  # browser()
  if (!requireNamespace("qgisprocess", quietly = TRUE)) {
    stop("Please install qgisprocess to use this function")
  }
  rnet_y_projected = sf::st_transform(rnet_y, crs = stplanr::geo_select_aeq(rnet_y))
  q_out = qgisprocess::qgis_run_algorithm(
    algorithm = "grass7:v.split",
    input = rnet_y_projected[1],
    length = split_y
  )
  rnet_y_split = sf::st_as_sf(q_out)
  rnet_y_split = sf::st_transform(rnet_y_split, sf::st_crs(rnet_y))
  rnet_y_split
}