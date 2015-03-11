#' Convert flow data to SpatialLinesDataFrame
#'
#' @section Details:
#' Origin-destination ('OD') flow data is often provided
#' in the form of 1 line per flow with zone codes of origin and destination
#' centroids. This can be tricky to plot and link-up with geographical data.
#' This function makes the task easier.
#'
#' @references
#' Rae, A. (2009). From spatial interaction data to spatial interaction information? Geovisualisation and spatial structures of migration from the 2001 UK census. Computers, Environment and Urban Systems, 33(3), 161–178. doi:10.1016/j.compenvurbsys.2009.01.007
#'
#'
gFlow2line <- function(flow, zones){
  l <- vector("list", nrow(flow))
  for(i in 1:nrow(flow)){
    from <- zones@data[,1] %in% flow[i, 1]
    to <- zones@data[,1] %in% flow[i, 2]
    x <- coordinates(zones[from, ])
    y <- coordinates(zones[to, ])
    l[[i]] <- Lines(list(Line(rbind(x, y))), as.character(i))
  }
  l <- SpatialLines(l)
  l <- SpatialLinesDataFrame(l, data = flow, match.ID = F)
  l
}
