#' Get points at the beginner and end of linestrings
#'
#' @inheritParams rnet_breakup_vertices
#' @export
#' @examples
#' has_sfheaders <- requireNamespace("sfheaders", quietly = TRUE)
#' if(has_sfheaders) {
#' rnet <- rnet_roundabout
#' bp1 <- rnet_boundary_points(rnet)
#' bp2 <- rnet_boundary_points_lwgeom(rnet) # slower version with lwgeom
#' identical(sort(sf::st_coordinates(bp1)), sort(sf::st_coordinates(bp2)))
#' plot(rnet$geometry)
#' plot(bp1, add = TRUE)
#' }
rnet_boundary_points <- function(rnet) {
  stopifnot(requireNamespace("sfheaders", quietly = TRUE))
  coordinates <- sfheaders::sf_to_df(rnet)
  # names(coordinates) # "sfg_id"        "linestring_id" "x"             "y"
  # head(coordinates)
  coordinates <- coordinates[-1]
  first_pair <- !duplicated(coordinates[, 1])
  last_pair <- !duplicated(coordinates[, 1], fromLast = TRUE)
  idxs <- first_pair | last_pair
  pairs <- coordinates[idxs, ]
  pairs_unique <- unique(pairs[c("x", "y")])
  boundary_points <- sfheaders::sf_point(pairs_unique)
  sf::st_crs(boundary_points) <- sf::st_crs(rnet)
  boundary_points
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
# bench::mark(check = FALSE, rnet_boundary_points(rnet), rnet_boundary_points_lwgeom(rnet))
