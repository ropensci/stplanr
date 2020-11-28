#' Get points at the beginner and end of linestrings
#'
#' @inheritParams rnet_breakup_vertices
#' @export
#' @examples
#' has_sfheaders <- requireNamespace("sfheaders", quietly = TRUE)
#' if(has_sfheaders) {
#' rnet <- rnet_roundabout
#' bp1 <- rnet_boundary_points(rnet)
#' bp2 <- line2points(rnet) # slower version with lwgeom
#' bp3 <- rnet_boundary_points_lwgeom(rnet) # slower version with lwgeom
#' bp4 <- rnet_boundary_unique(rnet)
#' nrow(bp1)
#' nrow(bp3)
#' identical(sort(sf::st_coordinates(bp1)), sort(sf::st_coordinates(bp2)))
#' identical(sort(sf::st_coordinates(bp3)), sort(sf::st_coordinates(bp4)))
#' plot(rnet$geometry)
#' plot(bp3, add = TRUE)
#' }
rnet_boundary_points <- function(rnet) {
  pairs <- rnet_boundary_df(rnet)
  pairs_xyz <- pairs[names(pairs) %in% c("x", "y", "z")]
  boundary_points <- sfheaders::sf_point(pairs_xyz)
  sf::st_crs(boundary_points) <- sf::st_crs(rnet)
  boundary_points
}
#' @rdname rnet_boundary_points
#' @export
rnet_boundary_df <- function(rnet) {
  stopifnot(requireNamespace("sfheaders", quietly = TRUE))
  coordinates <- sfheaders::sf_to_df(rnet)
  first_pair <- !duplicated(coordinates[["sfg_id"]])
  last_pair <- !duplicated(coordinates[["sfg_id"]], fromLast = TRUE)
  idxs <- first_pair | last_pair
  pairs <- coordinates[idxs, ]
  pairs
}
#' @rdname rnet_boundary_points
#' @export
rnet_boundary_unique <- function(rnet) {
  pairs <- rnet_boundary_points(rnet)
  unique(pairs)
}
#' @rdname rnet_boundary_points
#' @export
rnet_boundary_points_lwgeom <- function(rnet) {
  start_points <- lwgeom::st_startpoint(rnet)
  end_points <- lwgeom::st_endpoint(rnet)
  start_and_end_points <- c(start_points, end_points)
  duplicates <- duplicated(start_and_end_points)
  sf::st_sf(start_and_end_points[!duplicates])
}
#' @rdname rnet_boundary_points
#' @param n The minimum number of time a vertex must be duplicated to be returned
#' @export
rnet_duplicated_vertices <- function(rnet, n = 2) {
  coords <- sfheaders::sf_to_df(rnet)
  coords_n_all <- stats::aggregate(coords$sfg_id, list(coords$x, coords$y), FUN = length)
  names(coords_n_all) <- c("x", "y", "n")
  # coords_n_all = dplyr::count(coords, x, y) # v. slow
  coords_n <- coords_n_all[coords_n_all$n >= n, -ncol(coords_n_all)]
  p <- sfheaders::sf_point(coords_n)
  sf::st_crs(p) <- sf::st_crs(rnet)
  # data.table implementation (to check)
  # if(requireNamespace("data.table", quietly = TRUE)) {
  #   coords_dt = data.table::setDT(coordinates[, c("x", "y")])
  #   coords_n_all = coords_dt[, data.table::.N]
  #   coords_n = coords_n_all[coords_n_all$N >= n]
  #   p <- sfheaders::sf_point(coords_n[, -ncol(coords_n)])
  #   sf::st_crs(p) <- sf::st_crs(rnet)
  # }
  p
}

# bench::mark(check = FALSE,
#             rnet_boundary_points(rnet),
#             rnet_boundary_points_lwgeom(rnet),
#             rnet_duplicated_vertices(rnet),
#             line2points(rnet)
# )
# A tibble: 4 x 13
# expression                             min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc
# <bch:expr>                        <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>
#   1 rnet_boundary_points(rnet)           372µs 384.58µs     2559.     331KB     15.0  1198     7
# 2 rnet_boundary_points_lwgeom(rnet)   1.21ms   1.25ms      783.     249KB     15.1   363     7
# 3 rnet_duplicated_vertices(rnet)      1.13ms   1.15ms      864.     273KB     10.6   408     5
# 4 line2points(rnet)                   1.93ms   1.97ms      501.     265KB     13.0   231     6
# # … with 5 more variables: total_time <bch:tm>, result <list>, memory <list>, time <list>,
# #   gc <list>

# coords_single <- stplanr::od_id_szudzik(coords$x, coords$y)
# coords_count <- table(coords_single)
# coords_in <- coords[coords_single %in% names(coords_count)[coords_count >= 3], ]
# coords_n <- unique(coords_in[-c(1, 2)])


