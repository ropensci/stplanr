#' Select a custom projected CRS for the area of interest
#'
#' This function takes a spatial object with a geographic (WGS84)
#' CRS and returns a custom projected CRS focussed on the centroid of the object.
#' This function is especially useful for using units of metres in all directions
#' for data collected anywhere in the world.
#'
#' The function is based on this stackexchange answer:
#' \url{http://gis.stackexchange.com/questions/121489}
#'
#' @param shp A spatial object with a geographic (WGS84) coordinate system
#' @export
#' @examples
#' data("routes_fast")
#' proj4string(routes_fast) <- CRS("+init=epsg:4326")
#' new_crs <- crs_select_aeq(routes_fast)
#' plot(routes_fast)
#' rf_projected <- spTransform(routes_fast, new_crs)
#' plot(rf_projected)
#' bbox(rf_projected)
#' line_length <- rgeos::gLength(rf_projected, byid = TRUE)
#' plot(line_length, rf_projected$length)
#' cor(line_length, rf_projected$length)
crs_select_aeq <- function(shp){
  cent <- rgeos::gCentroid(shp)
  aeqd <- sprintf("+proj=aeqd +lat_0=%s +lon_0=%s +x_0=0 +y_0=0",
          cent@coords[[2]], cent@coords[[1]])
  CRS(aeqd)
}
#' Reproject lat/long spatial object so that they are in units of 1m
#'
#' Many GIS functions (e.g. finding the area)
#'
#' @param shp A spatial object with a geographic (WGS84) coordinate system
#' @param crs An optional coordinate reference system (if not provided it is set
#' automatically by \code{\link{crs_select_aeq}}).
#' @export
#' @examples
#' data(routes_fast)
#' rf_aeq = reproject(routes_fast)
#' rf_osgb = reproject(routes_fast, 27700)
#' cor(rgeos::gLength(rf_aeq, byid = TRUE), rgeos::gLength(rf_osgb, byid = TRUE))
#' rf_aeq_wgs84 = sp::spTransform(rf_aeq, CRS("+init=epsg:4326"))
#' rf_osgb_wgs84 = sp::spTransform(rf_osgb, CRS("+init=epsg:4326"))
#' plot(rf_aeq_wgs84)
#' plot(rf_osgb_wgs84, col = "red", add = TRUE)
reproject = function(shp, crs = crs_select_aeq(shp)){
  if(is.na(raster::crs(shp))){
    message("Assuming a geographical (lat/lon) CRS (EPSG:4326)")
    raster::crs(shp) = CRS("+init=epsg:4326")
  }
  if(is.numeric(crs)) # test if it's an epsg code
    crs = CRS(paste0("+init=epsg:", crs))
  message(paste0("Transforming to CRS ", crs))
  res = spTransform(shp, crs)
  res
}

#' Perform GIS functions on a temporary, projected version of a spatial object
#'
#' @inheritParams reproject
#' @param fun A function to perform on the projected object (e.g.  \code{\link{gLength}}))
#' @param ... Arguments to pass to \code{fun}, e.g. \code{byid = TRUE} if the function is \code{\link{gLength}}))
#' @export
#' @examples
#' # Find the length of routes that are in lat/long format
#' data(routes_fast)
#' rlength = gprojected(routes_fast, fun = rgeos::gLength, byid = TRUE)
#' plot(routes_fast$length, rlength)
#' cor(routes_fast$length, rlength)
#' rbuf = gprojected(routes_fast, rgeos::gBuffer, byid = TRUE, width = 100)
#' plot(rbuf)
#' raster::crs(rbuf)
#' plot(routes_fast, col = "green", add = TRUE)
gprojected = function(shp, fun, crs = crs_select_aeq(shp), ...){
  # assume it's not projected  (i.e. lat/lon) if there is no CRS
  if(!is.na(is.projected(shp))){
    if(is.projected(shp)){
      res = fun(shp, ...)
    } else {
      shp_projected = reproject(shp, crs = crs)
      message(paste0("Running function on a temporary projected version of the Spatial object using the CRS: ", crs))
      res = fun(shp_projected, ...)
      if(is(res, "Spatial"))
        res = spTransform(res, CRS("+init=epsg:4326"))
    }
  } else {
    shp_projected = reproject(shp, crs = crs)
    message(paste0("Running function on a temporary projected version of the Spatial object using the CRS: ", crs))
    res = fun(shp_projected, ...)
    if(is(res, "Spatial"))
      res = spTransform(res, CRS("+init=epsg:4326"))
  }
  res
}

