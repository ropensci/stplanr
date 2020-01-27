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
