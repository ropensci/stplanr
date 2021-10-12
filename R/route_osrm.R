#' Plan routes on the transport network using the OSRM server
#'
#' This function is a simplified and (because it uses GeoJSON not binary polyline format)
#' slower R interface to OSRM routing services compared with the excellent
#' [osrm::osrmRoute()] function (which can be used via the [route()]) function.
#'
#' @param profile Which routing profile to use? One of "foot" (default)
#'   "bike" or "car" for the default open server.
#' @param osrm.server The base URL of the routing server.
#' getOption("osrm.server") by default.
#' @param osrm.profile The routing profile to use, e.g. "car", "bike" or "foot"
#' (when using the routing.openstreetmap.de test server).
#' getOption("osrm.profile") by default.
#' @inheritParams route
#' @family routes
#' @export
#' @examples
#' \donttest{
#' l1 = od_data_lines[49, ]
#' l1m = od_coords(l1)
#' from = l1m[, 1:2]
#' to = l1m[, 3:4]
#' if(curl::has_internet()) {
#' r_foot = route_osrm(from, to)
#' r_bike = route_osrm(from, to, osrm.profile = "bike")
#' r_car = route_osrm(from, to, osrm.profile = "car")
#' plot(r_foot$geometry, lwd = 9, col = "grey")
#' plot(r_bike, col = "blue", add = TRUE)
#' plot(r_car, col = "red", add = TRUE)
#' }
#' }
route_osrm <- function(from, to, osrm.server = "https://routing.openstreetmap.de/",
                       osrm.profile = "foot"){

  if(osrm.server == "https://routing.openstreetmap.de/") {
    osrm.server = paste0(osrm.server, "routed-", osrm.profile, "/")
    osrm.profile = "driving"
  }

  # from osrm package: https://github.com/rCarto/osrm
    req <- paste(osrm.server,
                 "route/v1/", osrm.profile, "/",
                 from[1], ",", from[2], ";", to[1], ",", to[2],
                 "?alternatives=false&geometries=geojson&steps=false&overview=full",
                 sep="")
    # browseURL(req)
    r <- jsonlite::read_json(req)
    d <- do.call(rbind, r$routes[[1]]$geometry$coordinates)
    storage.mode(d) <- "numeric"
    dsfc <- sf::st_sfc(sf::st_linestring(d), crs = 4326)
    ddf <- data.frame(
      distance = r$routes[[1]]$distance,
      duration = r$routes[[1]]$duration
    )
    dsf <- sf::st_sf(ddf, geometry = dsfc)
    dsf
}