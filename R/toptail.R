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
#' @param toptail_dist The distance (in metres) to top and tail the line by.
#' Can either be a single value or a vector of the same length as the
#' SpatialLines object.
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

  if (length(toptail_dist) > 1) {
    if (length(toptail_dist) != length(l)) {
      stop("toptail_dist is vector but not of equal length to SpatialLines object")
    }
  }
  toptail_disto <- toptail_dist

  for(i in 1:length(l)){
    toptail_dist <- ifelse(length(toptail_disto) == 1, toptail_disto, toptail_disto[i])
    l1 <- l[i,]
    lpoints <- line2points(l1)

    # Create buffer for geographic or projected crs
    if(!is.projected(l)){
      sel <- buff_geo(lpoints, width = toptail_dist, ...)
    } else {
      sel <- rgeos::gBuffer(lpoints, width = toptail_dist, ...)
    }

    if(rgeos::gContainsProperly(sel, l1)){
      message(paste0("Line ", i, " is completely removed by the clip and",
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
#' @param shp A spatial object with a geographic CRS (WGS84)
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
buff_geo <- function(shp, width, ..., silent = TRUE){
  old_proj <- CRS(proj4string(shp))
  new_proj <- crs_select_aeq(shp)
  if(silent == FALSE){
    message(paste0("The new Azimuthal equidistant projection",
    "used to create the buffer was ", new_proj))
    message(paste0("The original projection was ", old_proj))
  }
  shp <- sp::spTransform(shp, new_proj)
  buff <- rgeos::gBuffer(shp, width = width, ...)
  sp::spTransform(buff, old_proj)
}

#' Clip the first and last n metres of SpatialLines
#'
#' Takes lines and removes the start and end point, to a distance determined
#' by the user. Uses the geosphere::distHaversine function and requires
#' coordinates in WGS84 (lng/lat).
#'
#' @param l A SpatialLines object
#' @param toptail_dist The distance (in metres) to top the line by.
#' Can be either a single value or a vector of the same length as the
#' SpatialLines object. If tail_dist is missing, is used as the tail distnce.
#' @param tail_dist The distance (in metres) to tail the line by. Can be
#' either a single value or a vector of the same length as the SpatialLines
#' object.
#' @export
#' @examples
#' data("routes_fast")
#' r_toptail <- toptailgs(routes_fast, toptail_dist = 300)
#' plot(routes_fast, lwd = 3)
#' plot(r_toptail, col = "red", add = TRUE)
#' plot(cents, add = TRUE)
toptailgs <- function(l, toptail_dist, tail_dist = NULL) {

  if (length(toptail_dist) > 1) {
    if (length(toptail_dist) != length(l)) {
      stop("toptail_dist is vector but not of equal length to SpatialLines object")
    }
  }
  if (!missing(tail_dist)) {
    if (length(tail_dist) > 1) {
      if (length(tail_dist) != length(l)) {
        stop("tail_dist is vector but not of equal length to SpatialLines object")
      }
    }
  }
  else {
    tail_dist <- toptail_dist
  }

  toptail_disto <- toptail_dist
  tail_disto <- tail_dist

  i <- 1
  while(i <= length(l)) {
    toptail_dist <- ifelse(length(toptail_disto) == 1, toptail_disto, toptail_disto[i])
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
    tail_dist <- ifelse(length(tail_disto) == 1, tail_disto, tail_disto[i])

    linecoords <- rbind(
      linecoords[which(bottomdists >= tail_dist),,drop=FALSE],
      tail(linecoords[which(bottomdists >= tail_dist),,drop=FALSE],n=1)+(
        linecoords[which(bottomdists < tail_dist),,drop=FALSE][1,]-
          tail(linecoords[which(bottomdists >= tail_dist),,drop=FALSE],n=1)
      )*
        ((tail(bottomdists[which(bottomdists >= tail_dist)],n=1)-tail_dist)/(tail(bottomdists[which(bottomdists >= tail_dist)],n=1)-bottomdists[which(bottomdists < tail_dist)][1]))
    )
    l@lines[[i]]@Lines[[1]]@coords <- unname(linecoords)
    i <- i + 1
  }
  return(l)
}

#' Clip the beginning and ends SpatialLines to the edge of SpatialPolygon borders
#'
#' Takes lines and removes the start and end point, to a distance determined
#' by the nearest polygon border.
#'
#'
#' @param l A SpatialLines object
#' @param buff A SpatialPolygons object to act as the buffer
#' @param ... Arguments passed to rgeos::gBuffer()
#' @export
#' @examples
#' data("routes_fast")
#' data("zones")
#' r_toptail <- toptail_buff(routes_fast, zones)
#' sel <- row.names(routes_fast) %in% row.names(r_toptail)
#' rf_cross_poly <- routes_fast[sel,]
#' plot(zones)
#' plot(routes_fast, col = "blue", lwd = 4, add = TRUE)
#' # note adjacent lines removed
#' plot(rf_cross_poly, add = TRUE, lwd = 2)
#' plot(r_toptail, col = "red", add = TRUE)
toptail_buff <- function(l, buff, ...){
  # force same crs
  if(!identicalCRS(l, buff)){
    proj4string(buff) <- proj4string(l)
  }
  for(i in 1:length(l)){
    l1 <- l[i,]
    lpoints <- line2points(l1)
    # Select zones per line
    sel <- buff[lpoints,]
    l2 <- rgeos::gDifference(l1, sel)
    if(is.null(l2)){
      next
    }else{
      row.names(l2) <- row.names(l1)
    }
    if(!exists("out")){
      out <- l2
    } else {
      out <- maptools::spRbind(out, l2)
    }
  }
  proj4string(out) <- proj4string(l)
  out
}
