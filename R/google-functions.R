#' Generate nearest point on the route network of a point using the Google Maps API
#'
#' @section Details:
#' Retrieve coordinates of the node(s) on the network mapped from coordinates
#' passed to functions.
#'
#' @param lat Numeric vector containing latitude coordinate for each coordinate
#' to map. Also accepts dataframe with latitude in the first column and
#' longitude in the second column.
#' @param lng Numeric vector containing longitude coordinate for each
#' coordinate to map.
#' @param google_api String value containing the Google API key to use.
#' @export
#' @examples \dontrun{
#'  nearest_google(lat = 50.333, lng = 3.222, google_api = "api_key_here")
#' }
nearest_google <- function(lat, lng, google_api){
  base_url = "https://roads.googleapis.com/v1/snapToRoads"
  url = paste0(base_url, "?path=", lat, ",", lng, "&key=", google_api)
  obj = jsonlite::fromJSON(url)
  coords = c(obj$snappedPoints$location$longitude, obj$snappedPoints$location$latitude)
  SpatialPointsDataFrame(coords = matrix(coords, ncol = 2),
                         data = data.frame(orig_lat = lat, orig_lng = lng))
}
#' Return travel network distances and time using the Google Maps API
#'
#' @section Details:
#' Estimate travel times accounting for the road network - see \url{https://developers.google.com/maps/documentation/distance-matrix/}
#'
#' @param lat Numeric vector containing latitude coordinate for each coordinate
#' to map. Also accepts dataframe with latitude in the first column and
#' longitude in the second column.
#' @param lng Numeric vector containing longitude coordinate for each
#' coordinate to map.
#' @param google_api String value containing the Google API key to use.
#' @param google_api Text string, either metric (default) or imperial.
#' @export
#' @examples \dontrun{
#'  nearest_google(lat = 50.333, lng = 3.222, google_api = "api_key_here")
#' }
dist_google <- function(lat, lng, google_api, units = 'metric'){
  base_url = "https://maps.googleapis.com/maps/api/distancematrix/json?units="
  # url =
   paste0(base_url, units, "&origins=", lat, "&", lng, "&key=", google_api)
  # obj = jsonlite::fromJSON(url)
  # coords = c(obj$snappedPoints$location$longitude, obj$snappedPoints$location$latitude)
  # SpatialPointsDataFrame(coords = matrix(coords, ncol = 2),
                        # data = data.frame(orig_lat = lat, orig_lng = lng))
}


https://maps.googleapis.com/maps/api/distancematrix/json?units=imperial&origins=Washington,DC&destinations=New+York+City,NY&key=YOUR_API_KEY



