#' Plan a route with local data
#'
#' This function returns the shortest path between locations
#' in, or near to, segements on a `SpatialLinesNetwork`.
#'
#' @inheritParams od_coords
#' @inheritParams sum_network_routes
#' @family routes
#' @export
#' @examples
#' from <- c(-1.535181, 53.82534)
#' to <- c(-1.52446, 53.80949)
#' sln <- SpatialLinesNetwork(route_network_sf)
#' r <- route_local(sln, from, to)
#' plot(sln)
#' plot(r$geometry, add = TRUE, col = "red", lwd = 5)
#' plot(cents[c(3, 4), ], add = TRUE)
#' r2 <- route_local(sln = sln, cents_sf[3, ], cents_sf[4, ])
#' plot(r2$geometry, add = TRUE, col = "blue", lwd = 3)
route_local <- function(sln, from, to, l = NULL) {

  coords <- od_coords(from, to, l)
  # from_sln <- find_network_nodes(sln, coords[1, "fx"], coords[1, "fy"])
  # to_sln <- find_network_nodes(sln, coords[1, "tx"], coords[1, "ty"])
  nodes_near = find_network_nodes(sln = sln, x = as.vector(coords[, c(1, 3)]),
                                  y = as.vector(coords[, c(2, 4)]), maxdist = 2000)
  od_df <- data.frame(start = nodes_near[1], end = nodes_near[2])
  sum_network_links(sln, routedata = od_df)

}
