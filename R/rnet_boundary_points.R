#' Get points at the beginner and end of linestrings
#'
#' @inheritParams rnet_breakup_vertices
#' @export
#' @examples
#' has_sfheaders <- requireNamespace("sfheaders", quietly = TRUE)
#' if(has_sfheaders) {
#' rnet <- rnet_roundabout
#' j <- rnet_boundary_points(rnet)
#' summary(duplicated(j))
#' plot(rnet$geometry)
#' plot(j, add = TRUE)
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
