#' Convert text strings into points on the map
#'
#' Generate a lat/long pair from data using Google's geolocation API.
#'
#' @param address Text string representing the address you want to geocode
#' @param base_url The base url to query
#' @param return_all Should the request return all information returned by Google Maps?
#'  The default is `FALSE`: to return only two numbers: the longitude and latitude, in that order
#' @param service Which service to use? Nominatim by default
#' @inheritParams route_cyclestreet
#' @export
#' @examples
#' \dontrun{
#' geo_code(address = "Hereford")
#' geo_code("LS7 3HB")
#' geo_code("hereford", return_all = TRUE)
#' # needs api key in .Renviron
#' geo_code("hereford", service = "google", pat = Sys.getenv("GOOGLE"), return_all = TRUE)
#' }
geo_code <- function(address,
                     service = "nominatim",
                    base_url = "https://maps.google.com/maps/api/geocode/json",
                    return_all = FALSE,
                    pat = NULL
                    ) {

    if(service == "nominatim") {
      if(base_url == "https://maps.google.com/maps/api/geocode/json") {
        base_url <- "https://nominatim.openstreetmap.org"
    }
      place_name <- address
      query <- list (q = place_name, format = "json")
      if(!return_all) {
        query <- c(query, limit = 1)
      }

      q_url <- httr::modify_url(base_url, query = query)
      res <- httr::GET (q_url)
      txt <- httr::content(res, as = "text", encoding = "UTF-8",
                           type = "application/xml")
      obj <- jsonlite::fromJSON(txt)
      res_df <- data.frame(lon = obj$lon, lat = obj$lat, name = obj$display_name)
      lon_lat <- as.numeric(c(lon = obj$lon[1], lat = obj$lat[1]))
  } else {

    query <- list(address = address, sensor = "false")
    if(!is.null(pat)) {
      query <- c(query, key = pat)
    }
  u <- httr::modify_url(base_url, query = query)
  res <- jsonlite::fromJSON(u)
  if(res$status == "OVER_QUERY_LIMIT") {
    stop(res$error_message)
  }

  res_df <- jsonlite::flatten(res$results)
  lon_lat <- c(
    lon = res_df$geometry.location.lng,
    lat = res_df$geometry.location.lat
    )
  }

  if(return_all) {
    return(res_df)
  } else {
    return(lon_lat)
  }

}
