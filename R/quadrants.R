#' Split a spatial object into quadrants
#'
#' Returns a character vector of NE, SE, SW, NW corresponding to north-east, south-east
#' quadrants respectively. If number_out is TRUE, returns numbers from 1:4, respectively.
#'
#' @param x Object of class sf
#' @param cent The centrepoint of the region of interest.
#'   Quadrants will be defined based on this point.
#'   By default this will be the geographic centroid of the zones.
#' @param number_out Should the result be returned as a number?
#' @family geo
#'
#' @export
#' @examples
#' x = zones_sf
#' (quads <- quadrant(x))
#' plot(x$geometry, col = factor(quads))
quadrant <- function(x, cent = NULL, number_out = FALSE) {
  if(is.null(cent)) {
    cent = sf::st_centroid(sf::st_union(x))
  }
  ccords = sf::st_coordinates(cent)
  x_cents <- sf::st_centroid(x)
  coords = sf::st_coordinates(x_cents)
  in_quadrant <- rep(NA, nrow(x))
  if (number_out) {
    in_quadrant[coords[, 1] > ccords[, 1] & coords[, 2] > ccords[, 2]] <- 1
    in_quadrant[coords[, 1] > ccords[, 1] & coords[, 2] < ccords[, 2]] <- 2
    in_quadrant[coords[, 1] < ccords[, 1] & coords[, 2] > ccords[, 2]] <- 3
    in_quadrant[coords[, 1] < ccords[, 1] & coords[, 2] < ccords[, 2]] <- 4
  } else {
    in_quadrant[coords[, 1] > ccords[, 1] & coords[, 2] > ccords[, 2]] <- "NE"
    in_quadrant[coords[, 1] > ccords[, 1] & coords[, 2] < ccords[, 2]] <- "SE"
    in_quadrant[coords[, 1] < ccords[, 1] & coords[, 2] > ccords[, 2]] <- "SW"
    in_quadrant[coords[, 1] < ccords[, 1] & coords[, 2] < ccords[, 2]] <- "NW"
  }
  in_quadrant
}
