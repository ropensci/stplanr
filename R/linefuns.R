# Line functions

#' Retrieve the number of vertices from a SpatialLines or SpatialPolygons object
#'
#' Returns a vector of the same length as the number of lines,
#' with the number of vertices per line or polygon.
#'
#' See \url{http://gis.stackexchange.com/questions/58147/} for more information.
#'
#' @param l A SpatialLines or SpatalPolygons object
#'
#' @export
#' @examples
#' n_vertices(routes_fast)
n_vertices <- function(l){
  sapply(l@lines, function(x) nrow(x@Lines[[1]]@coords))
}

#' Identify lines that are points
#'
#' OD matrices often contain 'intrazonal' flows, where the origin is the same point as the
#' destination. This function can help identify such intrazonal OD pairs, using 2 criteria:
#' the total number of vertices (2 or fewer) and whether the origin and destination are the same.
#'
#' @details
#' Returns a boolean vector. TRUE means that the associated line is in fact a point
#' (has no distance). This can be useful for removing data that will not be plotted.
#'
#' @inheritParams line2df
#' @export
#' @examples
#' islp <- is_linepoint(flowlines)
#' nrow(flowlines)
#' sum(islp)
#' # Remove invisible 'linepoints'
#' nrow(flowlines[!islp,])
is_linepoint <- function(l){
  nverts <- n_vertices(l)
  sel <- nverts <= 2
  ldf <- line2df(l)
  ldf$fx == ldf$tx & ldf$fy & ldf$ty & sel
}
#' Find the bearing of straight lines
#'
#' This is a simple wrapper around the geosphere function \code{\link{bearing}} to return the
#' bearing (in degrees relative to north) of lines.
#'
#' @details
#' Returns a boolean vector. TRUE means that the associated line is in fact a point
#' (has no distance). This can be useful for removing data that will not be plotted.
#'
#' @inheritParams line2df
#' @param bidirectional Should the result be returned in a bidirectional format?
#' Default is FALSE. If TRUE, the same line in the oposite direction would have the same bearing
#' @export
#' @examples
#' line_bearing(flowlines)
#' line_bearing(flowlines, bidirectional = TRUE)
line_bearing = function(l, bidirectional = FALSE){
  ldf = line2df(l)
  bearing = geosphere::bearing(as.matrix(ldf[,c("fx", "fy")]), as.matrix(ldf[,c("tx", "ty")]))
    if(bidirectional){
    new_bearing = bearing + 180
    new_bearing[new_bearing >= 180] = new_bearing[new_bearing >= 180] - 180
  }
  bearing
}
#' Calculate the angular difference between lines and a predefined bearing
#'
#' This function was designed to find lines that are close to parallel and perpendicular
#' to some pre-defined route. It can return results that are absolute (contain information
#' on the direction of turn, i.e. + or - values for clockwise/anticlockwise),
#' bidirectional (which mean values greater than +/- 90 are impossible).
#'
#' Building on the convention used in \code{\link{bearing}} and in many applications,
#' North is definied as 0, East as 90 and West as -90.
#'
#' @inheritParams line_bearing
#' @param absolute If TRUE (the default) only positive values can be returned
#' @param angle an angle in degrees relative to North, with 90 being East and -90 being West.
#'  (direction of rotation is ignored).
#'
#' @author Robin Lovelace and Malcolm Morgan
#' @export
#' @examples
#' # find all routes going North-South
#' a = angle_diff(flowlines, angle = 0, bidirectional = TRUE, absolute = TRUE)
#' plot(flowlines)
#' plot(flowlines[a < 15,], add = TRUE, lwd = 3, col = "red")
#' # East-West
#' plot(flowlines[a > 75,], add = TRUE, lwd = 3, col = "green")
angle_diff = function(l, angle, bidirectional = FALSE, absolute = TRUE){
  if(is(object = l, "Spatial")){
    line_angles = line_bearing(l)
  } else {
    line_angles = l
  }
  angle_diff = angle - line_angles
  angle_diff[angle_diff <= -180] = angle_diff[angle_diff <= -180] + 180
  angle_diff[angle_diff >= 180] = angle_diff[angle_diff >= 180] - 180
  if(bidirectional){
    angle_diff[angle_diff <= -90] = 180 + angle_diff[angle_diff <= -90]
    angle_diff[angle_diff >= 90] = 180 - angle_diff[angle_diff >= 90]
  }
  if(absolute)
    angle_diff = abs(angle_diff)
  angle_diff
}
#' Find the mid-point of lines
#'
#' This is a wrapper around \code{\link{SpatialLinesMidPoints}} that allows it to find the midpoint
#' of lines that are not projected, which have a lat/long CRS.
#' @inheritParams line2df
#' @export
#' @examples
#' l = routes_fast
line_midpoint = function(l){
  stplanr::crs_select_aeq
}