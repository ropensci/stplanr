#' Get a route from the BikeCitizens web service
#'
#' See [bikecitizens.net](https://map.bikecitizens.net/gb-leeds#/!/1/1/53.8265,-1.576195/53.80025,-1.51577)
#' for an interactive version of the routing engine used by BikeCitizens.
#'
#' See the bikecitizens.R file in the data-raw directory of the package's
#' development repository for details on usage and examples.
#'
#' @param from A numeric vector representing the start point
#' @param to A numeric vector representing the end point
#' @param base_url The base URL for the routes
#' @param cccode The city code for the routes
#' @param routing_profile What type of routing to use?
#' @param bike_profile What type of bike?
#' @param from_lat Latitude of origin
#' @param from_lon Longitude of origin
#' @param to_lat Latitude of destination
#' @param to_lon Longitude of destination
route_bikecitizens <- function(
                               from = NULL,
                               to = NULL,
                               base_url = "https://map.bikecitizens.net/api/v1/locations/route.json",
                               cccode = "gb-leeds",
                               routing_profile = "balanced",
                               bike_profile = "citybike",
                               from_lat = 53.8265,
                               from_lon = -1.576195,
                               to_lat = 53.80025,
                               to_lon = -1.51577) {
  if (!is.null(from) && !is.null(to)) {
    from_lon <- from[1]
    from_lat <- from[2]
    to_lon <- to[1]
    to_lat <- to[2]
  }
  q <- list(
    cccode,
    routing_profile,
    bike_profile,
    from_lat,
    from_lon,
    to_lat,
    to_lon
  )
  n <- c(
    "cccode",
    "routing_profile",
    "bike_profile",
    "from_lat",
    "from_lon",
    "to_lat",
    "to_lon"
  )
  names(q) <- n
  u <- httr::modify_url(
    url = base_url,
    query = q
  )
  r <- jsonlite::read_json(u)
  d <- do.call(rbind, r$route)
  storage.mode(d) <- "numeric"
  dsfc <- sf::st_sfc(sf::st_linestring(d[, c(2, 1, 3)]), crs = 4326)
  dsf <- sf::st_sf(geometry = dsfc)
  dsf
}
