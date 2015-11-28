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
