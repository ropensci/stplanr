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
    if(sum(from) == 0)
      warning(paste0("No match for line ", i))
    to <- zones@data[,1] %in% flow[i, 2]
    if(sum(to) == 0 & sum(from) == 1)
      warning(paste0("No match for line ", i))
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
#' @param l A SpatialLinesDataFrame
#' @param route_fun A routing function to be used for converting the straight lines to routes
#' \code{\link{od2line}}
#' @param n_print A number specifying how frequently progress updates
#' should be shown
#' @param ... Arguments passed to the routing function, e.g. \code{\link{route_cyclestreet}}
#' @inheritParams route_cyclestreet
#' @export
#' @examples
#'
#' \dontrun{
#' plot(flowlines)
#' rf <- line2route(l = flowlines, "route_cyclestreet", plan = "fastest")
#' rq <- line2route(l = flowlines, plan = "quietest", silent = TRUE)
#' plot(rf, col = "red", add = T)
#' plot(rq, col = "green", add = T)
#' # Plot for a single line to compare 'fastest' and 'quietest' route
#' n = 21
#' plot(flowlines[n,])
#' lines(rf[n,], col = "red")
#' lines(rq[n,], col = "green")
#' }
line2route <- function(l, route_fun = "route_cyclestreet", n_print = 10, ...){
  FUN <- match.fun(route_fun)
  ldf <- line2df(l)
  r <- l

  # test for the second od pair (the first often fails)
  rc2 <- FUN(from = ldf[2,1:2], to = ldf[2, 3:4], ...)

  rdata <- data.frame(matrix(nrow = nrow(l), ncol = ncol(rc2)))
  names(rdata) <- names(rc2)
  r@data <- rdata
      # stop(paste0("Sorry, the function ", route_fun, " cannot be used with line2route at present")
    for(i in 1:nrow(ldf)){
    tryCatch({
        rc <- FUN(from = ldf[i,1:2], to = ldf[i, 3:4], ...)
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
#' Convert a series of points into a dataframe of origins and destinations
#'
#' Takes a series of geographical points and converts them into a data.frame
#' representing the potential flows, or 'spatial interaction', between every combination
#' of points.
#'
#' @param p SpatialPointsDataFrame or data.frame
#' @export
#' @examples
#' df <- points2odf(cents)
#'
points2odf <- function(p){
  if(grepl(pattern = "DataFrame", class(p))){
    geo_code <- p@data[,1]
  } else {
    geo_code <- p[,1]
  }
  df = data.frame(
    expand.grid(geo_code, geo_code)[2:1]
  )
  names(df) <- c("O", "D")
  df
}
#' Convert a series of points into geographical flows
#'
#' Takes a series of geographical points and converts them into a SpatialLinesDataFrame
#' representing the potential flows, or 'spatial interaction', between every combination
#' of points.
#'
#' @param p SpatialPointsDataFrame
#'
#' @export
#' @examples
#' plot(cents)
#' flow <-points2flow(cents)
#' plot(flow, add = TRUE)
points2flow <- function(p){
  df <- points2odf(p)
  flow <- od2line(flow = df, zones = p)
}

#' Update line geometry
#'
#' Take two SpatialLines objects and update the geometry of the former with that of the latter,
#' retaining the data of the former.
#'
#' @param l A SpatialLines object, whose geometry is to be modified
#' @param nl A SpatialLines object of the same length as \code{l} to provide the new geometry
#'
#' @export
#' @examples
#' l <- flowlines
#' nl <- routes_fast
#' nrow(l)
#' nrow(nl)
#' l <- l[!is_linepoint(l),]
#' names(l)
#' names(routes_fast)
#' l_newgeom <- update_line_geometry(l, nl)
#' plot(l, lwd = l$All / mean(l$All))
#' plot(l_newgeom, lwd = l$All / mean(l$All))
#' names(l_newgeom)
update_line_geometry <- function(l, nl){
  for(i in 1:nrow(l)){
    l@lines[[i]] <- Lines(nl@lines[[i]]@Lines, row.names(l[i,]))
  }
  l
}
