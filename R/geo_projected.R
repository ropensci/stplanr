#' Select a custom projected CRS for the area of interest
#'
#' This function takes a spatial object with a geographic (WGS84)
#' CRS and returns a custom projected CRS focussed on the centroid of the object.
#' This function is especially useful for using units of metres in all directions
#' for data collected anywhere in the world.
#'
#' The function is based on this stackexchange answer:
#' <https://gis.stackexchange.com/questions/121489>
#'
#' @param shp A spatial object with a geographic (WGS84) coordinate system
#' @export
#' @examples
#' shp <- zones_sf
#' geo_select_aeq(shp)
#' @family geo
#' @export
geo_select_aeq <- function(shp) {
  UseMethod("geo_select_aeq")
}
#' @export
geo_select_aeq.sf <- function(shp) {
  cent <- sf::st_geometry(shp)
  coords <- sf::st_coordinates(shp)
  coords_mat <- matrix(coords[, 1:2], ncol = 2)
  midpoint <- apply(coords_mat, 2, mean)
  aeqd <- sprintf(
    "+proj=aeqd +lat_0=%s +lon_0=%s +x_0=0 +y_0=0",
    midpoint[2], midpoint[1]
  )
  sf::st_crs(aeqd)
}
#' @export
geo_select_aeq.sfc <- function(shp) {
  cent <- sf::st_geometry(shp)
  coords <- sf::st_coordinates(shp)
  coords_mat <- matrix(coords[, 1:2], ncol = 2)
  midpoint <- apply(coords_mat, 2, mean)
  aeqd <- sprintf(
    "+proj=aeqd +lat_0=%s +lon_0=%s +x_0=0 +y_0=0",
    midpoint[2], midpoint[1]
  )
  sf::st_crs(aeqd)
}

#' Perform GIS functions on a temporary, projected version of a spatial object
#'
#' This function performs operations on projected data.
#'
#' @param shp A spatial object with a geographic (WGS84) coordinate system
#' @param fun A function to perform on the projected object (e.g. from the sf package)
#' @param crs An optional coordinate reference system (if not provided it is set
#' automatically by [geo_select_aeq()])
#' @param silent A binary value for printing the CRS details (default: TRUE)
#' @param ... Arguments to pass to `fun`
#' @aliases gprojected
#' @export
#' @family geo
#' @examples
#' lib_versions <- sf::sf_extSoftVersion()
#' lib_versions
#' # fails on some systems (with early versions of PROJ)
#' if (lib_versions[3] >= "6.3.1") {
#'   shp <- routes_fast_sf[2:4, ]
#'   geo_projected(shp, sf::st_buffer, dist = 100)
#' }
geo_projected <- function(shp, fun, crs, silent, ...) {
  UseMethod(generic = "geo_projected")
}
#' @export
geo_projected.sf <- function(shp, fun, crs = geo_select_aeq(shp), silent = TRUE, ...) {
  # assume it's not projected  (i.e. lat/lon) if there is no CRS
  if (is.na(sf::st_crs(shp))) {
    sf::st_crs(shp) <- sf::st_crs(4326)
  }
  crs_orig <- sf::st_crs(shp)
  # If the original CRS is already projected, run the fun() on the original:
  if (!is.na(sf::st_crs(shp)) && !sf::st_is_longlat(shp)) {
    if (!silent) {
      message("Running function on original projection")
    }
    res <- fun(shp, ...)
    return(res)
  }
  shp_projected <- sf::st_transform(shp, crs)
  if (!silent) {
    message(paste0("Running function on a temporary projection: ", crs$proj4string))
  }
  res <- fun(shp_projected, ...)
  if (grepl("sf", x = class(res)[1])) {
    res <- sf::st_transform(res, crs_orig)
  }
  res
}
#' @export
geo_projected.sfc <- function(shp, fun, crs = geo_select_aeq(shp), silent = TRUE, ...) {
  shp_sf <- sf::st_as_sf(shp)
  res <- geo_projected.sf(shp_sf, fun, crs, silent, ...)
  sf::st_geometry(res)
}
#' Perform a buffer operation on a temporary projected CRS
#'
#' This function solves the problem that buffers will not be circular when used on
#' non-projected data.
#'
#' Requires recent version of PROJ (>= 6.3.0).
#' Buffers on `sf` objects with geographic (lon/lat) coordinates can also
#' be done with the [`s2`](https://r-spatial.github.io/s2/) package.
#'
#' @param shp A spatial object with a geographic CRS (e.g. WGS84)
#' around which a buffer should be drawn
#' @param dist The distance (in metres) of the buffer (when buffering simple features)
#' @param width The distance (in metres) of the buffer (when buffering sp objects)
#' @param ... Arguments passed to the buffer (see `?sf::st_buffer` for details)
#' @examples
#' lib_versions <- sf::sf_extSoftVersion()
#' lib_versions
#' if (lib_versions[3] >= "6.3.1") {
#'   buff_sf <- geo_buffer(routes_fast_sf, dist = 50)
#'   plot(buff_sf$geometry)
#'   geo_buffer(routes_fast_sf$geometry, dist = 50)
#' }
#' @family geo
#' @export
geo_buffer <- function(shp, dist = NULL, width = NULL, ...) {
  UseMethod("geo_buffer")
}
#' @export
geo_buffer.sf <- function(shp, ...) {
  geo_projected(shp, sf::st_buffer, ...)
}
#' @export
geo_buffer.sfc <- function(shp, ...) {
  geo_projected(shp, sf::st_buffer, ...)
}

#' Calculate line length of line with geographic or projected CRS
#'
#' Takes a line (represented in sf or sp classes)
#' and returns a numeric value representing distance in meters.
#' @param shp A spatial line object
#' @examples
#' lib_versions <- sf::sf_extSoftVersion()
#' lib_versions
#' if (lib_versions[3] >= "6.3.1") {
#'   geo_length(routes_fast_sf)
#' }
#' @family geo
#' @export
geo_length <- function(shp) {
  UseMethod("geo_length")
}

#' @export
geo_length.sf <- function(shp) {
  if (sf::st_is_longlat(shp)) {
    l <- lwgeom::st_geod_length(shp)
  } else {
    l <- sf::st_length(shp)
  }
  as.numeric(l)
}
