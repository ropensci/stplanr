#' Clip the first and last n metres of SpatialLines
#'
#' Takes lines and removes the start and end point, to a distance determined
#' by the user.
#'
#' Note: \code{\link{toptailgs}} is around 10 times faster, but only works
#' on data with geographic CRS's due to its reliance on the geosphere
#' package.
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
#' plot(buff)
#' plot(routes_fast, add = TRUE)
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

#' Clip the first and last n metres of SpatialLines
#'
#' Takes lines and removes the start and end point, to a distance determined
#' by the user. Uses the geosphere::distHaversine function and requires
#' coordinates in WGS84 (lng/lat).
#'
#' @param l A SpatialLines object
#' @param toptail_dist The distance (in metres) to top and tail the line by
#' @export
#' @examples
#' data("routes_fast")
#' r_toptail <- toptailgs(routes_fast, toptail_dist = 300)
#' plot(routes_fast, lwd = 3)
#' plot(r_toptail, col = "red", add = TRUE)
#' plot(cents, add = TRUE)
toptailgs <- function(l, toptail_dist) {

  i <- 1
  while(i <= length(l)) {
    linecoords <- coordinates(l@lines[[i]])[[1]]
    topdists <- geosphere::distHaversine(linecoords[1,],linecoords)
    linecoords <- rbind(
      tail(linecoords[which(topdists < toptail_dist),,drop=FALSE],n=1)+(
        linecoords[which(topdists >= toptail_dist),,drop=FALSE][1,]-
          tail(linecoords[which(topdists < toptail_dist),,drop=FALSE],n=1)
      )*(
        (toptail_dist-tail(topdists[which(topdists < toptail_dist)],n=1))/(topdists[which(topdists >= toptail_dist)][1]-tail(topdists[which(topdists < toptail_dist)],n=1))
      ),
      linecoords[which(topdists >= toptail_dist),,drop=FALSE]
    )
    bottomdists <- geosphere::distHaversine(linecoords[nrow(linecoords),],linecoords)

    linecoords <- rbind(
      linecoords[which(bottomdists >= toptail_dist),,drop=FALSE],
      tail(linecoords[which(bottomdists >= toptail_dist),,drop=FALSE],n=1)+(
        linecoords[which(bottomdists < toptail_dist),,drop=FALSE][1,]-
          tail(linecoords[which(bottomdists >= toptail_dist),,drop=FALSE],n=1)
      )*
        ((tail(bottomdists[which(bottomdists >= toptail_dist)],n=1)-toptail_dist)/(tail(bottomdists[which(bottomdists >= toptail_dist)],n=1)-bottomdists[which(bottomdists < toptail_dist)][1]))
    )
    l@lines[[i]]@Lines[[1]]@coords <- unname(linecoords)
    i <- i + 1
  }
  return(l)
}
