#' Plan a route with local data
#'
#' This function returns the shortest path between locations
#' in, or near to, segements on a `SpatialLinesNetwork`.
#'
#' @inheritParams route_graphhopper
#' @inheritParams sum_network_routes
#' @export
#' @examples
#' bb = sf::st_bbox(routes_fast)
#' set.seed(5)
#' from = c(runif(1, bb$xmin, bb$xmax), runif(1, bb$ymin, bb$ymax))
#' to = c(runif(1, bb$xmin, bb$xmax), runif(1, bb$ymin, bb$ymax))
#' sln = SpatialLinesNetwork(routes_fast)
#' r <- route_local(sln, from, to)
#' plot(r)
#'  \dontrun{
#' # todo: next code chunk is not currently working:
#' r2 = route_local(sln = sln, cents_sf[5, ], cents_sf[6, ])
#' plot(r2)
#' r_many <- line2route(flowlines_sf[2:9, ], route_local, sln = sln)
#' plot(cents)
#' plot(sln@sl, add = TRUE)
#' plot(r_many, add = TRUE)
#' }
route_local <- function(sln, from, to, l = NULL) {

  coords <- od_coords(from, to, l)
  coords <- od_coords(from, to, l)
  from_sln = find_network_nodes(sln, coords[1, "fx"], coords[1, "fy"])
  to_sln = find_network_nodes(sln, coords[1, "tx"], coords[1, "ty"])
  sum_network_routes(sln, from_sln, to_sln, "length", combinations = FALSE)

}
