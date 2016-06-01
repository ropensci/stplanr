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
#' @param mode Text string specifying the mode of transport. Can be
#' bicycling (default), walking, driving or transit
#' @param arrival_time Time of arrival in date format.
#' @export
#' @examples \dontrun{
#'  # Distances from one origin to one destination
#'  dist_google(from = c(0, 52), to = c(0, 53))
#'  data("cents")
#'  # Distances from between all origins and destinations
#'  dists_cycle = dist_google(from = cents, to = cents)
#'  dists_drive = dist_google(cents, cents, mode = "driving")
#'  dists_trans = dist_google(cents, cents, mode = "transit")
#'  dists_trans_am = dist_google(cents, cents, mode = "transit",
#'   arrival_time = strptime("2016-05-27 09:00:00",
#'    format = "%Y-%m-%d %H:%M:%S", tz = "BST"))
#'  # Find out how much longer (or shorter) cycling takes than walking
#'  summary(dists_cycle$duration / dists_trans$duration)
#'  # Difference between travelling now and for 9am arrival
#'  summary(dists_trans_am$duration / dists_trans$duration)
#'  odf = points2odf(cents)
#'  odf = cbind(odf, dists)
#'  head(odf)
#'  flow = points2flow(cents)
#'  # show the results for duration (thicker line = shorter)
#'  plot(flow, lwd = mean(odf$duration) / odf$duration)
#'  dist_google(c("Hereford"), c("Weobley", "Leominster", "Kington"))
#'  dist_google(c("Hereford"), c("Weobley", "Leominster", "Kington"),
#'   mode = "transit", arrival_time = strptime("2016-05-27 17:30:00",
#'   format = "%Y-%m-%d %H:%M:%S", tz = "BST"))
#' }
dist_google <- function(from, to, google_api = Sys.getenv("GOOGLEDIST"),
                        g_units = 'metric',
                        mode = 'bicycling', arrival_time = ""){
  base_url <- "https://maps.googleapis.com/maps/api/distancematrix/json?units="
  # Convert sp object to lat/lon vector
  if(class(from) == "SpatialPoints" | class(from) == "SpatialPointsDataFrame" )
    from <- coordinates(from)
  if(class(to) == "SpatialPoints" | class(to) == "SpatialPointsDataFrame" )
    to <- coordinates(to)
  if (google_api == "") {
    google_api_param <- ""
  } else {
    google_api_param <- "&key="
  }
  if(is(from, "matrix") | is(from, "data.frame"))
    from = paste(from[,2], from[,1], sep = ",")
  if(is(from, "numeric"))
    from = paste(from[2], from[1], sep = ",")
  if(is(to, "matrix") | is(to, "data.frame"))
    to = paste(to[,2], to[,1], sep = ",")
  if(is(to, "numeric"))
    to = paste(to[2], to[1], sep = ",")
  from = paste0(from, collapse = "|")
  to = paste0(to, collapse = "|")
  url_travel <- paste0(base_url, g_units, "&origins=", from,
          "&destinations=", to,
          paste0("&mode=", mode))
  if(class(arrival_time)[1] == "POSIXlt"){
    arrival_time <- as.numeric(arrival_time)
    url_travel <- paste0(url_travel, "&arrival_time=", arrival_time)
  }
  url = paste0(url_travel,
          google_api_param,
          google_api)
  message(paste0("Sent this request: ", url))
  obj <- jsonlite::fromJSON(url)
  # some of cols are data.frames, e.g.
  # lapply(obj$rows$elements[[1]], class)
  # obj$rows$elements[[1]][1]
  # obj$rows$elements[[1]][1]$distance$value
  distances = lapply(obj$rows$elements,
                     function(x) x[1]$distance$value)
  distances = unlist(distances)
  duration = lapply(obj$rows$elements,
                     function(x) x[2]$duration$value)
  duration = unlist(duration)
  currency = NA
  fare = NA
  if(mode == "transit" & !is.null(obj$rows$elements[[1]]$fare)){
    currency = lapply(obj$rows$elements,
                      function(x) x$fare$currency)
    currency = unlist(currency)
    fare = lapply(obj$rows$elements,
                  function(x) x$fare$value)
    fare = unlist(fare)
  }
  # is_ok = lapply(obj$rows$elements,
  #                   function(x) x$status)
  from_addresses = rep(obj$origin_addresses, each = length(obj$origin_addresses))
  to_addresses = rep(obj$destination_addresses, length(obj$origin_addresses))
  res_df = data.frame(from_addresses, to_addresses, distances, duration, currency, fare)
  res_df$from_addresses <- as.character(res_df$from_addresses)
  res_df$to_addresses <- as.character(res_df$to_addresses)
  return(res_df)
}





