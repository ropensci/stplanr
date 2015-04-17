#' Do the intersections between two geometries create lines?
#'
#' This is a function required in \code{\link{gOverline()}}. It identifies
#' whether sets of lines overlap (beyond shared points) or
#' not.
#'
#' @examples
#' data(routes_fast)
#' rnet <- gOverline(routes_fast[c(2, 3, 22),], attr = "length")
#' r1 <- routes_fast[2,]
#' r2 <- routes_fast[3,]
#' r3 <- routes_fast[22,]
#' plot(rnet)
#' lines(r3, col = "red") # line without overlaps
#' islines(r1, r2)
#' islines(r1, r3)
#' islines(r2, r3)
islines <- function(g1, g2){
  ## return TRUE if geometries intersect as lines, not points
  inherits(gIntersection(g1,g2), "SpatialLines")
}

#' Function to split overlapping SpatialLines into segments
#'
#' Divides SpatialLinesDataFrame objects into separate Lines.
#' Each new Lines object is the aggregate of a single number
#' of aggregated lines.
#'
#' @example
#' data(routes_fast)
#' rsec <- gSection(routes_fast)
#' plot(rsec)
#' length(rsec)
#' set.seed(5)
#' sel <- sample(length(rsec), 20)
#' plot(rsec[sel,], col = "red", add = T, lwd = 3)
gSection <- function(sl){
  ## union and merge and disaggregate to make a
  ## set of non-overlapping line segments
  sp::disaggregate(rgeos::gLineMerge(rgeos::gUnion(sl,sl)))
}

lineLabels <- function(sldf, attr){
  text(coordinates(gCentroid(sldf,byid=TRUE)),labels=sldf[[attr]])
}


#' Convert series of overlapping lines into a route network
#'
#' This function takes a series of Lines stored in a
#'  \code{SpatialLinesDataFrame}
#' and converts these into a single route network.
#'
#' @param sldf A SpatialLinesDataFrame with overlapping elements
#' @param attr A text string corresponding to a named variable in \code{sldf}
#'
#' @author Barry Rowlingson
#'
#' @examples
#' data(routes_fast)
#' rnet <- gOverline(sldf = routes_fast[1:7,], attr = "length")
#' plot(rnet)
#' points(cents)
#' lineLabels(sldf = rnet, "length")
#' sum(routes_fast@data$length[1:7], na.rm = T) # verify highest flow
gOverline <- function(sldf, attr, fun=sum){
  ## simplify down to SpatialLines
  sl = as(sldf, "SpatialLines")
  ## get the line sections that make the network
  slu = gSection(sl)
  ## overlay network with routes
  overs = over(slu, sl, returnList=TRUE)
  ## overlay is true if end points overlay, so filter them out:
  overs = lapply(1:length(overs), function(islu){
    Filter(function(isl){
      islines(sl[isl,],slu[islu,])
    }, overs[[islu]])
  })
  ## now aggregate the required attribute using fun():
  aggs = sapply(overs, function(os){fun(sldf[[attr]][os])})

  ## make a SLDF with the named attribute:
  sldf = SpatialLinesDataFrame(slu, data.frame(Z=aggs))
  names(sldf)=attr
  sldf
}