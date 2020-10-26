#' Break up line objects into shorter segments
#'
#' This function breaks up a LINESTRING geometries into smaller pieces.
#'
#' @param l An sf object with LINESTRING geometry
#' @param z An sf object with `POLYGON` geometry or a number representing the
#'   resolution of grid cells used to break up the linestring objects
#' @return An sf object with LINESTRING geometry created after breaking up the
#'   input object.
#' @family lines
#' @export
#' @examples
#' library(sf)
#' z = zones_sf$geometry
#' l <- routes_fast_sf$geometry[2]
#' l_split <- line_breakup(l, z)
#' l
#' l_split
#' sf::st_length(l)
#' sum(sf::st_length(l_split))
#' plot(z)
#' plot(l, add = TRUE, lwd = 9, col = "grey")
#' plot(l_split, add = TRUE, col = 1:length(l_split))
line_breakup <- function(l, z) {
  l_split <- lwgeom::st_split(l, z)
  l_split_linestring <- sf::st_collection_extract(l_split, type = "LINESTRING")
  l_split_linestring
}

