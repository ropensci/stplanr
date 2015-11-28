#' Clip the first and last n metres of SpatialLines
#'
#' Takes lines and removes the start and end point, to a distance determined
#' by the user.
#'
#' @param l A SpatialLines object
#' @param toptail_dist The distance (in metres) to top and tail the line by
#' @export
#' @examples
#' data("routes_fast")
#' sp::proj4string(routes_fast) <- CRS("+init=epsg:4326")
#' r_toptail <- toptail(routes_fast, toptail_dist = 300)
#' plot(routes_fast, lwd = 3)
#' plot(r_toptail, col = "red", add = TRUE)
#' plot(cents, add = TRUE)
#' # Note the behaviour when the buffer size removes lines
#' r_toptail <- toptail(routes_fast, toptail_dist = 1000)
#' length(r_toptail) # note short routes have been removed
#' length(routes_fast)
#' plot(routes_fast, lwd = 3)
#' plot(r_toptail, col = "red", add = TRUE)
toptail <- function(l, toptail_dist){
  old_proj <- CRS(proj4string(l))
  new_proj <- crs_select_aeq(l)
  l <- sp::spTransform(l, new_proj)
  for(i in 1:length(l)){
    l1 <- l[i,]
    lpoints <- line2points(l1)
    sel <- rgeos::gBuffer(lpoints, width = toptail_dist)
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
  sp::spTransform(out, old_proj)
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