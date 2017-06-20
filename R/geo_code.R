#' Convert text strings into points on the map
#'
#' Generate a lat/long pair from data using Google's geolocation API.
#'
#' @param address Text string representing the address you want to geocode
#' @param base_url The base url to query
#' @param return_all Should the request return all information returned by Google Maps?
#'  The default is `FALSE`: to return only two numbers: the longitude and latitude, in that order
#' @export
#' @examples
#' address = "LS7 3HB"
#' geo_code(address = address)
#' geo_code(address = address, return_all = TRUE)
geo_code = function(address,
                    base_url = "http://maps.google.com/maps/api/geocode/json",
                    return_all = FALSE) {

  q <- list(address = address, sensor = "false")
  u <- httr::modify_url(base_url, query = q)
  res <- jsonlite::fromJSON(u)
  res_df <- jsonlite::flatten(res$results)
  lon_lat <- c(
    lon = res_df$geometry.location.lng,
    lat = res_df$geometry.location.lat
    )

  if(return_all) {
    return(res_df)
  } else {
    return(lon_lat)
  }

}
