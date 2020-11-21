#' Get points where 3 or more linestrings meet (junctions on road networks)
#'
#' @inheritParams rnet_breakup_vertices
#' @export
#' @examples
#' has_sfheaders <- requireNamespace("sfheaders", quietly = TRUE)
#' if(has_sfheaders) {
#' rnet <- rnet_roundabout
#' j <- rnet_junction(rnet)
#' plot(rnet$geometry)
#' plot(j, add = TRUE)
#' }
rnet_junction <- function(rnet) {
  stopifnot(requireNamespace("sfheaders", quietly = TRUE))
  coordinates <- sfheaders::sf_to_df(rnet)
  # names(coordinates)
  # head(coordinates)
  coordinates <- as.matrix(coordinates[c("x", "y", "linestring_id")])
  L1_index <- ncol(coordinates)
  coordinates <- unname(coordinates)
  first_pair <- !duplicated(coordinates[, L1_index])
  last_pair <- !duplicated(coordinates[, L1_index], fromLast = TRUE)
  idxs <- first_pair | last_pair
  pairs <- coordinates[idxs, ]
  boundary_points <- sfheaders::sf_point(
    pairs[, -L1_index]
  )
  sf::st_crs(boundary_points) <- sf::st_crs(rnet)
  boundary_points
}
