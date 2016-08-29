#' Extract coordinates from OD data
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
#' Rae, A. (2009). From spatial interaction data to spatial interaction information?
#' Geovisualisation and spatial structures of migration from the 2001 UK census.
#'  Computers, Environment and Urban Systems, 33(3). doi:10.1016/j.compenvurbsys.2009.01.007
#' @export
#' @examples \dontrun{
#' od2odf(flow, zones)
#' }
od2odf <- function(flow, zones){

  coords = dplyr::data_frame(code = as.character(zones[[1]]),
                      fx = coordinates(zones)[,1], fy = coordinates(zones)[,2])
  flowcode = dplyr::data_frame(code_o = as.character(flow[[1]]), code_d = as.character(flow[[2]]))
  odf = dplyr::left_join(flowcode, coords, by = c("code_o" = "code"))
  coords = dplyr::rename_(coords, tx = quote(fx), ty = quote(fy))
  odf = dplyr::left_join(odf, coords, by = c("code_d" = "code"))

  data.frame(odf) # return data.frame as more compatible with spatial data

}
#' Convert flow data to SpatialLinesDataFrame
#'
#' @section Details:
#' Origin-destination ('OD') flow data is often provided
#' in the form of 1 line per flow with zone codes of origin and destination
#' centroids. This can be tricky to plot and link-up with geographical data.
#' This function makes the task easier.
#' \code{\link{od2line2}} is a faster implementation
#' (around 6 times faster on large datasets)
#' that returns a \code{SpatialLines} object (omitting the data).
#'
#' @param flow A data frame representing the flow between two points
#' or zones. The first two columns of this data frame should correspond
#' to the first column of the data in the zones. Thus in \code{\link{cents}},
#' the first column is geo_code. This corresponds to the first two columns
#' of \code{\link{flow}}.
#' @param zones A SpatialPolygonsDataFrame or SpatialPointsDataFrame
#' representing origins and destinations of travel flows.
#' @export
#' @examples \dontrun{
#' data(flow) # load data frame of od flows between zones
#' data(cents) # load centroids data
#' newflowlines <- od2line(flow = flow, zones = cents)
#' newflowlines2 <- od2line2(flow = flow, zones = cents)
#' sp::plot(cents)
#' lines(newflowlines, lwd = 3)
#' lines(newflowlines2, col = "white")
#' nfl_sldf <- SpatialLinesDataFrame(newflowlines, flow, match.ID = FALSE)
#' identical(nfl_sldf, newflowlines)
#' }
#' @name od2line
NULL

#' @rdname od2line
#' @export
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
  l <- sp::SpatialLinesDataFrame(l, data = flow, match.ID = FALSE)
  sp::proj4string(l) <- sp::proj4string(zones)
  l
}

#' @rdname od2line
#' @export
od2line2 <- function(flow, zones){

  odf = od2odf(flow, zones)
  l <- vector("list", nrow(odf))
  for(i in 1:nrow(odf)){
    l[[i]] <- sp::Lines(list(sp::Line(rbind(c(odf$fx[i], odf$fy[i]), c(odf$tx[i], odf$ty[i])))), as.character(i))
  }
  l <- sp::SpatialLines(l)
}

#' Convert SpatialLinesDataFrame objects to a data.frame with from and to coords
#'
#' This function returns a data frame with fx and fy and tx and ty variables
#' representing the beginning and end points of spatial line features respectively.
#'
#' @param l A SpatialLinesDataFrame
#' @export
#' @examples
#' line2df(flowlines[5,]) # beginning and end of a single straight line
#' line2df(flowlines) # on multiple lines
#' line2df(routes_fast[5:6,]) # beginning and end of routes
line2df <- function(l){
  ldf_geom = raster::geom(l)
  dplyr::group_by_(dplyr::as_data_frame(ldf_geom), 'object') %>%
    summarise_(fx = quote(first(x)), fy = quote(first(y)), tx = quote(last(x)), ty = quote(last(y)))
}

#' Convert a SpatialLinesDataFrame to points
#'
#' The number of points will be double the number of lines with \code{line2points}.
#' A closely related function, \code{line2pointsn} returns all the points that were line vertices.
#' The points corresponding with a given line, \code{i}, will be \code{(2*i):((2*i)+1)}.
#'
#' @param l A SpatialLinesDataFrame
#' @export
#' @examples
#' data(routes_fast)
#' lpoints <- line2pointsn(routes_fast[2,]) # for a single line
#' lpoints2 = line2points(routes_fast[2,])
#' plot(lpoints)
#' plot(lpoints2)
#' lpoints = line2pointsn(routes_fast) # for many lines
#' plot(lpoints)
#' data(flowlines) # load demo flowlines dataset
#' lpoints <- line2points(flowlines) # for many lines
#' sp::proj4string(lpoints) # maintains CRS info
#' plot(lpoints) # note overlapping points
#' i = 3
#' j = (2*i):((2*i)+1)
#' plot(flowlines[i,])
#' plot(lpoints[j,], add = TRUE)
#' @name line2points
NULL

#' @rdname line2points
#' @export
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

#' @rdname line2points
#' @export
line2pointsn <- function(l){
  spdf = raster::geom(l)
  p = sp::SpatialPoints(coords = spdf[,c("x", "y")])
  raster::crs(p) = raster::crs(l)
  p
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
#' plot(rf, col = "red", add = TRUE
#' plot(rq, col = "green", add = TRUE
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
#' cents_centroids <- rgeos::gCentroid(cents, byid = TRUE)
#' df2 <- points2odf(cents_centroids)
points2odf <- function(p){
  if(grepl(pattern = "DataFrame", class(p))){
    geo_code <- p@data[,1]
  } else if(is(p, "SpatialPoints")){
    geo_code <- 1:length(p)
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

#' Quickly calculate Euclidean distances of od pairs
#'
#' It is common to want to know the Euclidean distance between origins and destinations
#' in OD data. You can calculate this by first converting OD data to SpatialLines data,
#' e.g. with \code{\link{od2line}}. However this can be slow and overkill if you just
#' want to know the distance. This function is a few orders of magnitude faster.
#'
#' Note: this function assumes that the zones or centroids in \code{cents} have a geographic
#' (lat/lon) CRS.
#'
#' @inheritParams od2line
#' @export
#' @examples
#' data(flow)
#' data(cents)
#' od_dist(flow, cents)
od_dist <- function(flow, zones){
  omatch = match(flow[[1]], zones@data[[1]])
  dmatch = match(flow[[2]], zones@data[[1]])
  cents_o = zones@coords[omatch,]
  cents_d = zones@coords[dmatch,]
  geosphere::distHaversine(p1 = cents_o, p2 = cents_d)
}

#' Convert a series of points, or a matrix of coordinates, into a line
#'
#' This is a simple wrapper around \code{\link{spLines}} that makes the creation of
#' \code{SpatialLines} objects easy and intuitive
#'
#' @param p A SpatialPoints obect or matrix representing the coordinates of points.
#' @export
#' @examples
#' p = matrix(1:4, ncol = 2)
#' l = points2line(p)
#' plot(l)
#' l = points2line(cents)
#' plot(l)
#' p = line2points(routes_fast)
#' l = points2line(p)
#' plot(l)
points2line = function(p){
  if(is(p, "SpatialPoints")){
    p_proj = sp::proj4string(p)
    p = sp::coordinates(p)
  } else {
    p_proj = NA
  }
  l = raster::spLines(p)
  raster::crs(l) = p_proj
  l
}
