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
#' Note: Currently returns the json object returned by the Google Maps API and uses the same origins and destinations.
#' @inheritParams route_cyclestreet
#' @param google_api String value containing the Google API key to use.
#' @param g_units Text string, either metric (default) or imperial.
#' @export
#' @examples \dontrun{
#'  dist_google(from = c(0, 52), to = c(0, 53), google_api = Sys.getenv("GOOGLEDIST"))
#'  dist_google(from = data.frame(
#'    lat = c(-33.1,-33.2,-33.3,-33.4,-33.5),
#'    lng = c(150.0,150.1,150.2,150.3,150.4)
#'  ), to = data.frame(
#'    lat = c(-33.1,-33.2,-33.3,-33.4,-33.5),
#'    lng = c(150.0,150.1,150.2,150.3,150.4)
#'  ))
#' }
dist_google <- function(from, to, google_api = "", g_units = 'metric'){
  base_url <- "https://maps.googleapis.com/maps/api/distancematrix/json?units="
  # Convert sp object to lat/lon vector
  if(class(from) == "SpatialPoints" | class(from) == "SpatialPointsDataFrame" )
    from <- coordinates(from)[,c(2,1)]
  if(class(to) == "SpatialPoints" | class(to) == "SpatialPointsDataFrame" )
    to <- coordinates(to)[,c(2,1)]
  if (google_api == "") {
    google_api_param <- ""
  } else {
    google_api_param <- "&key="
  }
  if (is(from, "data.frame") == TRUE | is(from, "matrix") == TRUE) {
    if (is(from, "data.frame") == TRUE) {
      if (length(which(colnames(from) %in% c("lat","latitude","Latitude","y"))) > 0) {
        latcol = which(colnames(from) %in% c("lat","latitude","Latitude","y"))[1]
      } else {
        latcol = 1
      }
      if (length(which(colnames(from) %in% c("lng","long","longitude","Longitude","x"))) > 0) {
        lngcol = which(colnames(from) %in% c("lng","long","longitude","Longitude","x"))[1]
      } else {
        lngcol = 2
      }
    } else if (is(from, "matrix")) {
      latcol = 1
      lngcol = 2
    }
    from = paste(paste(from[,latcol], from[,lngcol], sep=','), collapse='|')
  } else {
    from = paste0(rev(from), collapse = ",")
  }
  if (is(to, "data.frame") == TRUE | is(to, "matrix") == TRUE) {
    if (is(to, "data.frame") == TRUE) {
      if (length(which(colnames(to) %in% c("lat","latitude","Latitude","y"))) > 0) {
        latcol = which(colnames(to) %in% c("lat","latitude","Latitude","y"))[1]
      } else {
        latcol = 1
      }
      if (length(which(colnames(to) %in% c("lng","long","longitude","Longitude","x"))) > 0) {
        lngcol = which(colnames(to) %in% c("lng","long","longitude","Longitude","x"))[1]
      } else {
        lngcol = 2
      }
    } else if (is(to, "matrix")) {
      latcol = 1
      lngcol = 2
    }
    to = paste(paste(to[,latcol], to[,lngcol], sep=','), collapse='|')
  } else {
    to = paste0(rev(to), collapse = ",")
  }
  url <- paste0(base_url, g_units, "&origins=", from,
          "&destinations=", to,
          google_api_param,
          google_api)
  obj <- jsonlite::fromJSON(url)
  return(obj)
  # coords = c(obj$snappedPoints$location$longitude, obj$snappedPoints$location$latitude)
  # SpatialPointsDataFrame(coords = matrix(coords, ncol = 2),
                        # data = data.frame(orig_lat = lat, orig_lng = lng))
}



