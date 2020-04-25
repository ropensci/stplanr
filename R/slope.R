#' Calculate the gradient of line segments from distance and elevation vectors
#'
#' @param x Vector of locations
#' @param e Elevations in same units as x (assumed to be metres)
#' @export
#' @family route_funs
#' @examples
#' x = c(0, 2, 3, 4, 5, 9)
#' e = c(1, 2, 2, 4, 3, 1) / 10
#' route_slope_vector(x, e)
route_slope_vector = function(x, e) {
  d = diff(x)
  e_change = diff(e)
  e_change / d
}

#' Calculate the gradient of line segments from a matrix of coordinates
#'
#' @param m Matrix containing coordinates and elevations
#' @inheritParams route_slope_vector
#' @inheritParams route_sequential_dist
#' @family route_funs
#' @export
#' @examples
#' x = c(0, 2, 3, 4, 5, 9)
#' y = c(0, 0, 0, 0, 0, 9)
#' z = c(1, 2, 2, 4, 3, 1) / 10
#' m = cbind(x, y, z)
#' plot(x, z, ylim = c(-0.5, 0.5), type = "l")
#' (gx = route_slope_vector(x, z))
#' (gxy = route_slope_matrix(m, lonlat = FALSE))
#' abline(h = 0, lty = 2)
#' points(x[-length(x)], gx, col = "red")
#' points(x[-length(x)], gxy, col = "blue")
#' title("Distance (in x coordinates) elevation profile",
#'   sub = "Points show calculated gradients of subsequent lines")
route_slope_matrix = function(m, e = m[, 3], lonlat = TRUE) {
  d = route_sequential_dist(m, lonlat = lonlat)
  e_change = diff(e)
  g = e_change / d
  g
}

#' Calculate the sequential distances between sequential coordinate pairs
#'
#' @param lonlat Are the coordinates in lon/lat order? `TRUE` by default
#' @inheritParams route_slope_matrix
#' @family route_funs
#' @export
#' @examples
#' x = c(0, 2, 3, 4, 5, 9)
#' y = c(0, 0, 0, 0, 0, 1)
#' m = cbind(x, y)
#' route_sequential_dist(m)
route_sequential_dist = function(m, lonlat = TRUE) {
  if(lonlat) {
    if(requireNamespace("geodist")) {
      geodist::geodist(m[, 1:2], sequential = TRUE) # lon lat
    } else {
      message("Install geodist")
    }
  } else {
    sqrt(diff(m[, 1])^2 + diff(m[, 2])^2)
  }
}
