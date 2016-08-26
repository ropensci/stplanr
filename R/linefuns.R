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
#' bearing (in degrees relative to north) of lines
#'
#' @details
#' Returns a boolean vector. TRUE means that the associated line is in fact a point
#' (has no distance). This can be useful for removing data that will not be plotted.
#'
#' @inheritParams line2df
#' @param bidirectional Should the result be returned in a bidirectional format?
#' Default is FALSE. If TRUE, the same line in the oposite direction would have the same bearing
#' @param north An angle in degrees representing the mid-point from which the uni-directional angle is relative to
#' @export
#' @examples
#' line_bearing(flowlines)
#' line_bearing(flowlines, bidirectional = TRUE)
line_bearing = function(l, bidirectional = FALSE, north = 0){
  ldf = line2df(l)
  bearing = geosphere::bearing(as.matrix(ldf[,c("fx", "fy")]), as.matrix(ldf[,c("tx", "ty")]))
    if(bidirectional){
    new_bearing = bearing + 180
    new_bearing[new_bearing >= 180] = new_bearing[new_bearing >= 180] - 180
  }
  bearing
}

angle_diff = function(l, angle, absolute = TRUE, bidirectional = FALSE){
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