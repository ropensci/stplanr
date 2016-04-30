#' Generate nearest point on the route network of a point using the CycleStreets.net
#'
#' @section Details:
#' Retrieve coordinates of the node(s) on the network mapped from coordinates
#' passed to functions.
#' @inheritParams route_cyclestreet
#' @param lat Numeric vector containing latitude coordinate for each coordinate
#' to map. Also accepts dataframe with latitude in the first column and
#' longitude in the second column.
#' @param lng Numeric vector containing longitude coordinate for each
#' coordinate to map.
#' @export
#' @examples \dontrun{
#'  nearest_cyclestreets(lat = 53, lng = 0.02, google_api = "api_key_here")
#' }
nearest_cyclestreets <- function(lat, lng, pat = cyclestreet_pat()){
  url = paste0("https://api.cyclestreets.net/v2/nearestpoint?lonlat=", lng, ",", lat, "&key=", pat)
  obj = jsonlite::fromJSON(url)
  coords = obj$features$geometry$coordinates[[1]]
  SpatialPointsDataFrame(coords = matrix(coords, ncol = 2),
                         data = data.frame(orig_lat = lat, orig_lng = lng))
}

