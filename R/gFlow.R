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
#'
#' @export
#'
#' @examples
#' data(flowlines) # load demo flowlines dataset
#' ldf <- line2df(flowlines)
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

#' Convert straight SpatialLinesDataFrame from flow data into routes
#'
#' @section Details:
#'
#' See \code{\link{route_cyclestreet}} and other route functions for details

#' @param ldf A SpatialLinesDataFrame or data.frame of coordinates produced by
#' \code{\link{line2df}}
#'
#' @param ... Arguements passed to \code{\link{route_cyclestreet}}
#'
#' @inheritParams route_cyclestreet
#'
#' @export
#'
#' @examples
#'
#' data(flowlines) # load demo flowlines dataset
#' \dontrun{
#' # Don't run as requires gdal dependency, can cause issues
#' library(rgdal)
#' flowlines <- spTransform(flowlines, CRS("+init=epsg:27700"))
#' flowlines <- flowlines[rgeos::gLength(flowlines, byid = TRUE) > 0,]
#' flowlines <- spTransform(flowlines, CRS("+init=epsg:4326"))
#' plot(flowlines)
#'
#' cckey <- readLines("~/Dropbox/dotfiles/cyclestreets-api-key-rl")
#' Sys.setenv(CYCLESTREET = cckey)
#' routes_fast <- line2route(l = flowlines, plan = "fastest")
#' routes_slow <- line2route(l = flowlines, plan = "quietest", silent = TRUE)
#' }
#'
#' flowlines <- flowlines[flowlines$Area.of.residence != flowlines$Area.of.workplace,]
#' # Save the route data (uncomment if this changes)
#' # devtools::use_data(routes_fast, overwrite = TRUE)
#' # devtools::use_data(routes_slow, overwrite = TRUE)
#'
#' if(!exists("routes_fast")){
#'   data(routes_fast, routes_slow) # load routes
#' }
#'
#' plot(flowlines)
#' lines(routes_fast, col = "red")
#' lines(routes_slow, col = "green")
#'
#' # Plot for a single line to compare 'fastest' and 'quietest' route
#' n = 18
#' plot(flowlines[n,])
#' lines(routes_fast[n,], col = "red")
#' lines(routes_slow[n,], col = "green")

line2route <- function(ldf, ...){
  if(class(ldf) == "SpatialLinesDataFrame") ldf <- line2df(ldf)
  rf <- route_cyclestreet(from = ldf[1,1:2], to = ldf[1, 3:4])

  for(i in 2:nrow(ldf)){
    tryCatch({
      # if (i==7) stop("Urgh, the iphone is in the blender !") # testing tryCatch
      rfnew <- route_cyclestreet(from = ldf[i,1:2], to = ldf[i, 3:4], ...)
      row.names(rfnew) <- as.character(i)
      rf <- maptools::spRbind(rf, rfnew)
    }, error = function(e){print(paste0("Fail for line number ", i))})

    # Status bar
    perc_temp <- i %% round(nrow(ldf) / 10)
    if(!is.na(perc_temp) & perc_temp == 0){
      print(paste0(round(100 * i/nrow(ldf)), " % out of ", nrow(ldf),
        " distances calculated")) # print % of distances calculated
    }
  }
  rf
}

