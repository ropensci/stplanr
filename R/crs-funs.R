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
#' @param sp_obj A spatial object with a geographic (WGS84) coordinate system
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
crs_select_aeq <- function(sp_obj){
  cent <- rgeos::gCentroid(sp_obj)
  aeqd <- sprintf("+proj=aeqd +lat_0=%s +lon_0=%s +x_0=0 +y_0=0",
          cent@coords[[2]], cent@coords[[1]])
  CRS(aeqd)
}
