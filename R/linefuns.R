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
#' Default is FALSE. If TRUE, the same line in the oposite direction would have the same bearing.
#' @export
#' @examples
#' line_bearing(flowlines)
#' line_bearing(flowlines, bidirectional = TRUE)
line_bearing = function(l, bidirectional = FALSE){
  ldf = line2df(l)
  bearing = geosphere::bearing(as.matrix(ldf[,1:2]), as.matrix(ldf[,3:4]))
  if(bidirectional){
    bearing[bearing > 90] = bearing[bearing > 90] - 90
    bearing[bearing < -90] = bearing[bearing < -90] + 90
  }
  bearing
}
