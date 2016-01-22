#' Convert flow data to SpatialLinesDataFrame
#'
#' @section Details:
#' Origin-destination ('OD') flow data is often provided
#' in the form of 1 line per flow with zone codes of origin and destination
#' centroids. This can be tricky to plot and link-up with geographical data.
#' This function makes the task easier.
#'
#' @param flow A data frame representing the flow between two points
#' or zones. The first two columns of this data frame should correspond
#' to the first column of the data in the zones. Thus in \code{\link{cents}},
#' the first column is geo_code. This corresponds to the first two columns
#' of \code{\link{flow}}.
#' @param zones A SpatialPolygonsDataFrame or SpatialPointsDataFrame
#' representing origins and destinations of travel flows.
#' @references
#' Rae, A. (2009). From spatial interaction data to spatial interaction information? Geovisualisation and spatial structures of migration from the 2001 UK census. Computers, Environment and Urban Systems, 33(3). doi:10.1016/j.compenvurbsys.2009.01.007
#' @export
#' @examples \dontrun{
#' data(flow) # load data frame of od flows between zones
#' data(cents) # load centroids data
#' newflowlines <- od2line(flow = flow, zones = cents)
#' plot(cents)
#' lines(newflowlines)
#' }
od2line <- function(flow, zones){
  l <- vector("list", nrow(flow))
  for(i in 1:nrow(flow)){
    from <- zones@data[,1] %in% flow[i, 1]
    to <- zones@data[,1] %in% flow[i, 2]
    x <- sp::coordinates(zones[from, ])
    y <- sp::coordinates(zones[to, ])
    l[[i]] <- sp::Lines(list(sp::Line(rbind(x, y))), as.character(i))
  }
  l <- sp::SpatialLines(l)
  l <- sp::SpatialLinesDataFrame(l, data = flow, match.ID = F)
  sp::proj4string(l) <- sp::proj4string(zones)
  l
}

#' Convert straight SpatialLinesDataFrame to a data.frame with from and to coords
#'
#'
#' @param l A SpatialLinesDataFrame
#' @export
#' @examples
#' \dontrun{
#' data(flowlines) # load demo flowlines dataset
#' ldf <- line2df(flowlines)
#' }
line2df <- function(l){
  l_list <- lapply(slot(l, "lines"), function(x) lapply(slot(x, "Lines"),
  function(y) slot(y, "coords")))
  from_list <- lapply(l@lines, function(x) x@Lines[[1]]@coords[1,])
  to_list <- lapply(l@lines, function(x) x@Lines[[1]]@coords[2,])
  from_mat <- do.call(rbind, from_list)
  to_mat <- do.call(rbind, to_list)

  output <- as.data.frame(cbind(from_mat, to_mat))
  names(output) <- c("fx", "fy", "tx", "ty")

  output

}

#' Convert a SpatialLinesDataFrame to points at the origin and destination
#'
#' @param l A SpatialLinesDataFrame
#' @export
#' @examples
#' data(routes_fast)
#' lpoints <- line2points(routes_fast[2,]) # for a single line
#' data(flowlines) # load demo flowlines dataset
#' lpoints <- line2points(flowlines) # for many lines
#' plot(lpoints) # note overlapping points
#'
line2points <- function(l){
  for(i in 1:length(l)){
    l1 <- l[i,]
    lcoords <- sp::coordinates(l1)[[1]][[1]]
    lpoints <- sp::SpatialPoints(matrix(lcoords[c(1, nrow(lcoords)),], nrow = 2))
    sp::proj4string(lpoints) <- sp::proj4string(l)
    if(i == 1){
      out <- lpoints
    } else {
      out <- tmap::sbind(out, lpoints)
    }
  }
  out
}

#' Convert straight SpatialLinesDataFrame from flow data into routes
#'
#' @section Details:
#'
#' See \code{\link{route_cyclestreet}} and other route functions for details

#' @param l A SpatialLinesDataFrame, such as that produced by
#' \code{\link{od2line}}
#'
#' @param n_print A number specifying how frequently progress updates
#' should be shown
#' @param ... Arguements passed to \code{\link{route_cyclestreet}}
#'
#' @inheritParams route_cyclestreet
#' @export
#' @examples
#'
#' \dontrun{
#' plot(flowlines)
#' rf <- line2route(l = flowlines, plan = "fastest")
#' rq <- line2route(l = flowlines, plan = "quietest", silent = TRUE)
#' plot(rf, col = "red", add = T)
#' plot(rq, col = "green", add = T)
#' # Plot for a single line to compare 'fastest' and 'quietest' route
#' n = 21
#' plot(flowlines[n,])
#' lines(rf[n,], col = "red")
#' lines(rq[n,], col = "green")
#' }
line2route <- function(l, n_print = 10, ...){
  ldf <- line2df(l)
  r <- l
  rdata <- data.frame(matrix(nrow = nrow(l), ncol = 11))
  r@data <- rdata
  names(r) <- c("plan", "start", "finish", "length", "time", "waypoint", "change_elev",
                "av_incline", "co2_saving", "calories", "busyness")

  for(i in 1:nrow(ldf)){

    tryCatch({
        rc <- route_cyclestreet(from = ldf[i,1:2], to = ldf[i, 3:4], ...)
        rcl <- Lines(rc@lines[[1]]@Lines, row.names(l[i,]))
        r@lines[[i]] <- rcl
        r@data[i,] <- rc@data
    }, error = function(e){warning(paste0("Fail for line number ", i))})

    # Status bar
    perc_temp <- i %% round(nrow(ldf) / n_print)
    if(!is.na(perc_temp) & perc_temp == 0){
      message(paste0(round(100 * i/nrow(ldf)), " % out of ", nrow(ldf),
                   " distances calculated")) # print % of distances calculated
    }
    }
  r
}
