#' Convert text strings into points on the map
#'
#' Generate a lat/long pair from data using Google's geolocation API.
#'
#' @param address Text string representing the address you want to geocode
#' @param base_url The base url to query
#' @export
#' @examples
#' address = "LS7 3HB"
#' geo_code(address = address)
geo_code = function(address,
                    base_url = "http://maps.google.com/maps/api/geocode/json") {

  q <- list(address = address, sensor = "false")
  u <- httr::modify_url(base_url, query = q)
  res <- jsonlite::fromJSON(u)
  res_df <- jsonlite::flatten(res$results)
  lat_lon <- c(lat = res_df$geometry.location.lat, lon = res_df$geometry.location.lng)

  lat_lon

}
