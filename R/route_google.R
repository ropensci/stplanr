#' Find shortest path using Google services
#'
#' Find the shortest path using Google's services.
#' See the `mapsapi` package for details.
#'
#' @inheritParams route
#' @param mode Mode of transport, walking (default), bicycling, transit, or driving
#'
#' @export
#' @examples
#' \donttest{
#' from = "university of leeds"
#' to = "pedallers arms leeds"
#' r = route(from, to, route_fun = cyclestreets::journey)
#' plot(r)
#' # r_google <- route(from, to, route_fun = mapsapi::mp_directions) # fails
#' r_google1 <- route_google(from, to)
#' plot(r_google1)
#' r_google <- route(from, to, route_fun = route_google)
#' }
route_google <- function(from, to, mode = "walking", key = Sys.getenv("GOOGLE"), ...) {
  doc <- mapsapi::mp_directions(origin = from, destination = to, mode = mode, key = key, ...)
  mapsapi::mp_get_routes(doc)
}
