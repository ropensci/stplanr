#' Plan a route with local data
#'
#' This function returns the shortest path between locations
#' in, or near to, segements on a `SpatialLinesNetwork`.
#'
#' @inheritParams route_graphhopper
#' @inheritParams sum_network_routes
#' @family routes
#' @export
#' @examples
#' # from <- matrix(stplanr::geo_code("pedallers arms leeds"), ncol = 2)
#' from <-c(-1.5327711, 53.8006988)
#' # to <- matrix(stplanr::geo_code("gzing"), ncol = 2)
#' to <- c(-1.527937, 53.8044309)
#' sln <- SpatialLinesNetwork(osm_net_example)
#' \dontrun{
#' r <- route_local(sln, from, to)
#' plot(r)
#' # todo: next code chunk is not currently working:
#' r2 <- route_local(sln = sln, cents_sf[5, ], cents_sf[6, ])
#' plot(r2)
#' r_many <- line2route(flowlines_sf[2:9, ], route_local, sln = sln)
#' plot(cents)
#' plot(sln@sl, add = TRUE)
#' plot(r_many, add = TRUE)
#' }
route_local <- function(sln, from, to, l = NULL) {

  coords <- od_coords(from, to, l)
  from_sln <- find_network_nodes(sln, coords[1, "fx"], coords[1, "fy"])
  to_sln <- find_network_nodes(sln, coords[1, "tx"], coords[1, "ty"])
  nodes_near = find_network_nodes(sln = sln, x = as.vector(coords[, c(1, 3)]),
                                  y = as.vector(coords[, c(2, 4)]), maxdist = 2000)
  od_df <- data.frame(start = nodes_near[1], end = nodes_near[2])
  sum_network_links(sln, routedata = od_df)

}
