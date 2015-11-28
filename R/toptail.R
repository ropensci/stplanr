#' Clip the first and last n metres of SpatialLines
#'
#' Takes lines and removes the start and end point, to a distance determined
#' by the user.
#'
#' @param l A SpatialLines object
#' @param toptail_dist The distance (in metres) to top and tail the line by
#' @param ... Arguments passed to rgeos::gBuffer()
#' @export
#' @examples
#' data("routes_fast")
#' sp::proj4string(routes_fast) <- CRS("+init=epsg:4326")
#' r_toptail <- toptail(routes_fast, toptail_dist = 300)
#' plot(routes_fast, lwd = 3)
#' plot(r_toptail, col = "red", add = TRUE)
#' plot(cents, col = "blue", add = TRUE, pch = 15)
#' # Note the behaviour when the buffer size removes lines
#' r_toptail <- toptail(routes_fast, toptail_dist = 1000)
#' length(r_toptail) # note short routes have been removed
#' length(routes_fast)
#' plot(routes_fast, lwd = 3)
#' plot(r_toptail, col = "red", add = TRUE)
toptail <- function(l, toptail_dist, ...){
  for(i in 1:length(l)){
    l1 <- l[i,]
    lpoints <- line2points(l1)

    # Create buffer for geographic or projected crs
    if(!is.projected(l)){
      sel <- buff_geo(lpoints, width = toptail_dist, ...)
    } else {
      sel <- rgeos::gBuffer(lpoints, width = toptail_dist, ...)
    }

    if(rgeos::gContainsProperly(sel, l1)){
      print(paste0("Line ", i, " is completely removed by the clip and",
                   " is omitted from the results"))
      next
    }
    l2 <- rgeos::gDifference(l1, sel)
    if(!exists("out")){
      out <- l2
    } else {
      out <- tmap::sbind(out, l2)
    }
  }
  out
}

#' Create a buffer of n metres for non-projected 'geographical' spatial data
#'
#' Solves the problem that buffers will not be circular when used on
#' non-projected data.
#'
#' Returns a
#'
#' @param sp_obj A spatial object with a geographic CRS (WGS84)
#' around which a buffer should be drawn
#' @param width The distance (in metres) of the buffer
#' @param ... Arguments passed to rgeos::gBuffer()
#' @param silent A binary value for printing the CRS details (default: FALSE)
#' @export
#' @examples
#' data("routes_fast")
#' sp::proj4string(routes_fast) <- CRS("+init=epsg:4326")
#' buff <- buff_geo(routes_fast, width = 100)
#' plot(buff_geo)
#' plot(routes_fast, add = T)
buff_geo <- function(sp_obj, width, ..., silent = TRUE){
  old_proj <- CRS(proj4string(sp_obj))
  new_proj <- crs_select_aeq(sp_obj)
  if(silent == FALSE){
    print(paste0("The new Azimuthal equidistant projection",
    "used to create the buffer was ", new_proj))
    print(paste0("The original projection was ", old_proj))
  }
  sp_obj <- sp::spTransform(sp_obj, new_proj)
  buff <- rgeos::gBuffer(sp_obj, width = width, ...)
  sp::spTransform(buff, old_proj)
}
