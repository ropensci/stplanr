#' Split a spatial object into quadrants
#'
#' Split a spatial object (initially tested on SpatialPolygons) into quadrants.
#'
#' Returns a character vector of NE, SE, SW, NW corresponding to north-east, south-east
#' quadrants respectively. If number_out is TRUE, returns numbers from 1:4, respectively.
#'
#' @param sp_obj Spatial object
#' @param number_out Should the output be numbers from 1:4 (FALSE by default)
#'
#' @export
#' @examples
#' data(zones)
#' sp_obj <- zones
#' (quads <- quadrant(sp_obj))
#' plot(sp_obj, col = factor(quads))
#' points(rgeos::gCentroid(sp_obj), col = "white")
#' # edge cases (e.g. when using rasters) lead to NAs
#' sp_obj <- raster::rasterToPolygons(raster::raster(ncol = 3, nrow = 3))
#' (quads <- quadrant(sp_obj))
#' plot(sp_obj, col = factor(quads))
quadrant <- function(sp_obj, number_out = FALSE) {
  cent <- rgeos::gCentroid(sp_obj)
  cents <- rgeos::gCentroid(sp_obj, byid = TRUE)
  in_quadrant <- rep(NA, length(sp_obj))
  if (number_out) {
    in_quadrant[cents@coords[, 1] > cent@coords[, 1] & cents@coords[, 2] > cent@coords[, 2]] <- 1
    in_quadrant[cents@coords[, 1] > cent@coords[, 1] & cents@coords[, 2] < cent@coords[, 2]] <- 2
    in_quadrant[cents@coords[, 1] < cent@coords[, 1] & cents@coords[, 2] > cent@coords[, 2]] <- 3
    in_quadrant[cents@coords[, 1] < cent@coords[, 1] & cents@coords[, 2] < cent@coords[, 2]] <- 4
  } else {
    in_quadrant[cents@coords[, 1] > cent@coords[, 1] & cents@coords[, 2] > cent@coords[, 2]] <- "NE"
    in_quadrant[cents@coords[, 1] > cent@coords[, 1] & cents@coords[, 2] < cent@coords[, 2]] <- "SE"
    in_quadrant[cents@coords[, 1] < cent@coords[, 1] & cents@coords[, 2] > cent@coords[, 2]] <- "SW"
    in_quadrant[cents@coords[, 1] < cent@coords[, 1] & cents@coords[, 2] < cent@coords[, 2]] <- "NW"
  }
  in_quadrant
}
