#' Write to geojson easily
#'
#' Provides a user-friendly wrapper for `sf::st_write()`. Note,
#' `geojson_write` from the geojsonio package
#' provides the same functionality <https://github.com/ropensci/geojsonio>.
#'
#' @inheritParams gclip
#' @param filename File name of the output geojson
writeGeoJSON <- function(shp, filename) {
  name <- nm <- deparse(substitute(shp))
  newname <- paste0(filename, ".geojson")
  sf::st_write(sf::st_as_sf(shp), newname)
}

#' Scale a bounding box
#'
#' Takes a bounding box as an input and outputs a bounding box of a different size, centred at the same point.
#'
#' @inheritParams gclip
#' @param scale_factor Numeric vector determining how much the bounding box will grow or shrink.
#' Two numbers refer to extending the bounding box in x and y dimensions, respectively.
#' If the value is 1, the output size will be the same as the input.
#' @family geo
#' @export
#' @examples
#' bb <- matrix(c(-1.55, 53.80, -1.50, 53.83), nrow = 2)
#' bb1 <- bbox_scale(bb, scale_factor = 1.05)
#' bb2 <- bbox_scale(bb, scale_factor = c(2, 1.05))
#' bb3 <- bbox_scale(bb, 0.1)
#' plot(x = bb2[1, ], y = bb2[2, ])
#' points(bb1[1, ], bb1[2, ])
#' points(bb3[1, ], bb3[2, ])
#' points(bb[1, ], bb[2, ], col = "red")
bbox_scale <- function(bb, scale_factor) {
  if (length(scale_factor == 1)) scale_factor <- rep(scale_factor, 2)
  b <- (bb - rowMeans(bb)) * scale_factor + rowMeans(bb)
  b
}

#' Flexible function to generate bounding boxes
#'
#' Takes a geographic object or bounding box as an input and outputs a bounding box,
#' represented as a bounding box, corner points or rectangular polygon.
#'
#' @inheritParams bbox_scale
#' @param shp Spatial object (from sf or sp packages)
#' @param distance Distance in metres to extend the bounding box by
#' @param output Type of object returned (polygon by default)
#' @aliases bb2poly
#' @seealso bb_scale
#' @family geo
#' @export
#' @examples
#' # Simple features implementation:
#' shp <- routes_fast_sf
#' shp_bb <- geo_bb(shp, distance = 100)
#' plot(shp_bb, col = "red", reset = FALSE)
#' plot(geo_bb(routes_fast_sf, scale_factor = 0.8), col = "green", add = TRUE)
#' plot(geo_bb(routes_fast_sf, output = "points"), add = TRUE)
#' plot(routes_fast_sf$geometry, add = TRUE)
geo_bb <- function(shp, scale_factor = 1, distance = 0, output = c("polygon", "points", "bb")) {
  UseMethod("geo_bb")
}

#' @export
geo_bb.sf <- function(shp, scale_factor = 1, distance = 0, output = c("polygon", "points", "bb")) {
  output <- match.arg(output)
  bb <- geo_bb_matrix(shp)
  bb <- bbox_scale(bb = bb, scale_factor = scale_factor)
  bb_sp <- bb2poly(bb = bb, distance = distance)
  bb <- sf::st_as_sf(bb_sp)
  sf::st_crs(bb) <- sf::st_crs(shp)
  if (output == "polygon") {
    return(bb)
  } else if (output == "points") {
    bb_point <- sp::SpatialPoints(raster::geom(bb_sp)[1:4, c(5, 6)])
    bb_point <- sf::st_as_sf(bb_point)
    sf::st_crs(bb_point) <- sf::st_crs(shp)
    return(bb_point)
  } else if (output == "bb") {
    return(geo_bb_matrix(bb))
  }
}

#' @export
geo_bb.bbox <- function(shp, scale_factor = 1, distance = 0, output = c("polygon", "points", "bb")) {
  output <- match.arg(output)
  bb <- matrix(shp, ncol = 2)
  bb <- bbox_scale(bb = bb, scale_factor = scale_factor)
  bb_sp <- bb2poly(bb = bb, distance = distance)
  bb <- sf::st_as_sf(bb_sp)
  sf::st_crs(bb) <- sf::st_crs(shp)
  if (output == "polygon") {
    return(bb)
  } else if (output == "points") {
    bb_point <- sp::SpatialPoints(raster::geom(bb_sp)[1:4, c(5, 6)])
    bb_point <- sf::st_as_sf(bb_point)
    sf::st_crs(bb_point) <- sf::st_crs(shp)
    return(bb_point)
  } else if (output == "bb") {
    return(geo_bb_matrix(bb))
  }
}

#' @export
geo_bb.matrix <- function(shp, scale_factor = 1, distance = 0, output = c("polygon", "points", "bb")) {
  output <- match.arg(output)
  if (nrow(shp) != 2) {
    bb <- geo_bb_matrix(shp)
  } else {
    bb <- shp
  }
  bb <- bbox_scale(bb = bb, scale_factor = scale_factor)
  bb <- bb2poly(bb = bb, distance = distance)
  if (output == "polygon") {
    return(bb)
  } else if (output == "points") {
    bb_point <- sp::SpatialPoints(raster::geom(bb)[1:4, c(5, 6)])
    return(bb_point)
  } else if (output == "bb") {
    return(geo_bb_matrix(bb))
  }
}

#' @export
bb2poly <- function(bb, distance = 0) {
  if (is(bb, "matrix")) {
    b_poly <- as(raster::extent(as.vector(t(bb))), "SpatialPolygons")
  } else {
    b_poly <- as(raster::extent(bb), "SpatialPolygons")
    proj4string(b_poly) <- proj4string(bb)
  }
  if (distance > 0) {
    b_poly_buff <- geo_buffer(shp = b_poly, width = distance)
    b_poly <- bb2poly(b_poly_buff)
  }
  b_poly
}

#' Create matrix representing the spatial bounds of an object
#'
#' Converts a range of spatial data formats into a matrix representing the bounding box
#'
#' @inheritParams geo_bb
#' @family geo
#' @export
#' @examples
#' geo_bb_matrix(routes_fast)
#' geo_bb_matrix(routes_fast_sf)
#' geo_bb_matrix(cents[1, ])
#' geo_bb_matrix(c(-2, 54))
#' geo_bb_matrix(sf::st_coordinates(cents_sf))
geo_bb_matrix <- function(shp) {
  UseMethod("geo_bb_matrix")
}
#' @export
geo_bb_matrix.Spatial <- function(shp) {
  sp::bbox(shp)
}
#' @export
geo_bb_matrix.sf <- function(shp) {
  bb <- sf::st_bbox(shp)
  bb <- matrix(bb, ncol = 2)
  bb
}
#' @export
geo_bb_matrix.numeric <- function(shp) {
  matrix(rep(shp, 2), ncol = 2)
}
#' @export
geo_bb_matrix.matrix <- function(shp) {
  range_x <- range(shp[, 1])
  range_y <- range(shp[, 2])
  matrix(c(range_x, range_y), ncol = 2, byrow = TRUE)
}
