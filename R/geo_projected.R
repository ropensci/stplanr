#' Select a projected CRS
#'
#' @examples
#' shp = st_sf(st_sfc(st_point(c(1, 0))))
#' geo_select_aeq(shp)
geo_select_aeq <- function(shp){
  cent <- st_geometry(shp)
  coords = st_coordinates(shp)
  coords_mat = matrix(coords[,1:2], ncol = 2)
  midpoint = apply(coords_mat, 2, mean)
  aeqd <- sprintf("+proj=aeqd +lat_0=%s +lon_0=%s +x_0=0 +y_0=0",
                  midpoint[1], midpoint[1])
  st_crs(aeqd)
}

#' Perform GIS functions on a temporary, projected version of a spatial object
#'
#' This function performs operations on projected data.
#' @param shp A spatial object with a geographic (WGS84) coordinate system
#' @param crs An optional coordinate reference system (if not provided it is set
#' automatically by \code{\link{crs_select_aeq}}).
#' @export
#' @examples
#' library(sf)
#' shp = st_sf(st_sfc(st_point(c(1, 0))))
#' geo_projected(shp, st_buffer, dist = 100)
#' library(sp)
#' shp = as(shp, "Spatial")
#' geo_projected(shp, fun = rgeos::gBuffer, width = 100)
#' geo_projected(routes_fast, fun = rgeos::gLength, byid = TRUE)
geo_projected = function(x, ...) {
  UseMethod(generic = "geo_projected")
}
#' @export
geo_projected.sf = function(shp, fun, crs_temp = geo_select_aeq(shp),  ...){
  # assume it's not projected  (i.e. lat/lon) if there is no CRS
  if(is.na(st_crs(shp))) st_crs(shp) = 4326
  crs_orig = st_crs(shp)
  shp_projected = st_transform(shp, crs_temp)
  message(paste0("Running function on a temporary projected version of the Spatial object using the CRS: ", crs_temp$proj4string))
  res = fun(shp_projected, ...)
  if(grepl("sf", x = class(res)[1]))
    res = st_transform(res, crs_orig)
  res
}
#' @export
geo_projected.Spatial = function(shp, fun, crs = crs_select_aeq(shp), ...) {
  gprojected(shp, fun, crs = crs_select_aeq(shp), ...)
}
#' Perform a buffer operation on a temporary projected CRS
#'
#' This function solves the problem that buffers will not be circular when used on
#' non-projected data.
#' @param shp A spatial object with a geographic CRS (e.g. WGS84)
#' around which a buffer should be drawn
#' @param width The distance (in metres) of the buffer
#' @param ... Arguments passed to the buffer (see \code{?rgeos::gBuffer} or \code{?sf::st_buffer} for details)
#' @param silent A binary value for printing the CRS details (default: FALSE)
#' @examples
#' buff_sp = geo_buffer(routes_fast, dist = 100)
#' plot(buff_sp, col = "red")
#' routes_fast_sf = sf::st_as_sf(routes_fast)
#' buff_sf = geo_buffer(routes_fast_sf, dist = 50)
#' @export
geo_buffer = function(x, ...) {
  UseMethod("geo_buffer")
}

#' @export
geo_buffer.sf = function(shp, dist = 0, ...) {
  geo_projected(shp, sf::st_buffer, dist = dist)
}

#' @export
geo_buffer.Spatial = function(shp, dist = 0, ...) {
  buff_geo(shp = shp, width = dist, ...)
}
