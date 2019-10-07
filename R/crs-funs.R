#' Select a custom projected CRS for the area of interest
#'
#' This function takes a spatial object with a geographic (WGS84)
#' CRS and returns a custom projected CRS focussed on the centroid of the object.
#' This function is especially useful for using units of metres in all directions
#' for data collected anywhere in the world.
#'
#' The function is based on this stackexchange answer:
#' <http://gis.stackexchange.com/questions/121489>
#'
#' @param shp A spatial object with a geographic (WGS84) coordinate system
#' @family geo
#' @export
#' @examples
#' data("routes_fast")
#' new_crs <- geo_select_aeq(routes_fast)
#' new_crs2 <- geo_select_aeq(routes_fast) # to be deprecated
#' identical(new_crs, new_crs2)
#' plot(routes_fast)
#' rf_projected <- sp::spTransform(routes_fast, new_crs)
#' plot(rf_projected)
#' sp::bbox(rf_projected)
#' line_length <- rgeos::gLength(rf_projected, byid = TRUE)
#' plot(line_length, rf_projected$length)
#' cor(line_length, rf_projected$length)
crs_select_aeq <- function(shp) {
  .Deprecated(new = "geo_select_aeq")
  cent <- rgeos::gCentroid(shp)
  aeqd <- sprintf(
    "+proj=aeqd +lat_0=%s +lon_0=%s +x_0=0 +y_0=0",
    cent@coords[[2]], cent@coords[[1]]
  )
  sp::CRS(aeqd)
}
#' Reproject lat/long spatial object so that they are in units of 1m
#'
#' Many GIS functions (e.g. finding the area)
#'
#' @param shp A spatial object with a geographic (WGS84) coordinate system
#' @param crs An optional coordinate reference system (if not provided it is set
#' automatically by [geo_select_aeq()]).
#' @family geo
#' @export
#' @examples
#' data(routes_fast)
#' rf_aeq <- reproject(routes_fast[1:3, ])
#' rf_osgb <- reproject(routes_fast[1:3, ], 27700)
reproject <- function(shp, crs = geo_select_aeq(shp)) {
  if (is.na(raster::crs(shp))) {
    message("Assuming a geographical (lat/lon) CRS (EPSG:4326)")
    raster::crs(shp) <- sp::CRS("+init=epsg:4326")
  }
  if (is.numeric(crs)) { # test if it's an epsg code
    crs <- sp::CRS(paste0("+init=epsg:", crs))
  }
  message(paste0("Transforming to CRS ", crs))
  res <- sp::spTransform(shp, crs)
  res
}
