#' Generate nearest point on the route network of a point using the CycleStreets.net
#'
#' @section Details:
#' Retrieve coordinates of the node(s) on the network mapped from coordinates
#' passed to functions.
#'
#' Note: there is now a dedicated cyclestreets package:
#' https://github.com/Robinlovelace/cyclestreets
#'
#' @inheritParams route_cyclestreets
#' @param shp A spatial object
#' @param lat Numeric vector containing latitude coordinate for each coordinate
#' to map. Also accepts dataframe with latitude in the first column and
#' longitude in the second column.
#' @param lng Numeric vector containing longitude coordinate for each
#' coordinate to map.
#'
#' @export
#' @examples
#' \dontrun{
#' nearest_cyclestreets(53, 0.02, pat = Sys.getenv("CYCLESTREETS"))
#' nearest_cyclestreets(cents[1, ], pat = Sys.getenv("CYCLESTREETS"))
#' nearest_cyclestreets(cents_sf[1, ], pat = Sys.getenv("CYCLESTREETS"))
#' }
nearest_cyclestreets <- function(shp = NULL, lat, lng, pat = api_pat("cyclestreet")) {
  UseMethod("nearest_cyclestreets", object = shp)
}
#' @export
nearest_cyclestreets.NULL <- function(shp = NULL, lat, lng, pat = api_pat("cyclestreet")) {
  url <- paste0("https://api.cyclestreets.net/v2/nearestpoint?lonlat=", lng, ",", lat, "&key=", pat)
  obj <- jsonlite::fromJSON(url)
  coords <- obj$features$geometry$coordinates[[1]]
  sp::SpatialPointsDataFrame(
    coords = matrix(coords, ncol = 2),
    data = data.frame(orig_lat = lat, orig_lng = lng)
  )
}
#' @export
nearest_cyclestreets.Spatial <- function(shp, lat = shp@coords[1, 2], lng = shp@coords[1, 1], pat = api_pat("cyclestreet")) {
  nearest_cyclestreets.NULL(lat = lat, lng = lng, pat = pat)
}
#' @export
nearest_cyclestreets.sf <- function(shp, lat = sf::st_coordinates(shp)[2], lng = sf::st_coordinates(shp)[1], pat = api_pat("cyclestreet")) {
  sf::st_as_sf(nearest_cyclestreets.NULL(lat = lat, lng = lng, pat = pat))
}
