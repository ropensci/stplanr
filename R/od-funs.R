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
#' @examples
#' data(flow)
#' data(zones)
#' od2odf(flow, zones)
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
#' Origin-destination ('OD') flow data is often provided
#' in the form of 1 line per flow with zone codes of origin and destination
#' centroids. This can be tricky to plot and link-up with geographical data.
#' This function makes the task easier.
#'
#' @details
#' The function expects zone codes to be in the 1st column of the zones/destinations
#' datasets and the 1st and 2nd columns of the flow data, respectively.
#'
#' \code{\link{od2line2}} is a faster implementation
#' (around 6 times faster on large datasets)
#' that returns a \code{SpatialLines} object, omitting the data and working
#' only when there is no destinations dataset (i.e. when the geography of
#' origins is the same as that of destinations).
#'
#' @param flow A data frame representing the flow between two points
#' or zones. The first two columns of this data frame should correspond
#' to the first column of the data in the zones. Thus in \code{\link{cents}},
#' the first column is geo_code. This corresponds to the first two columns
#' of \code{\link{flow}}.
#' @param zones A SpatialPolygonsDataFrame or SpatialPointsDataFrame
#' representing origins (and destinations if no separate destinations object is provided)
#' of travel flows.
#' @param destinations A SpatialPolygonsDataFrame or SpatialPointsDataFrame
#' representing destinations of travel flows.
#' @param zone_code Name of the variable in \code{zones} containing the ids of the zone.
#' By default this is the first column names in the zones.
#' @param origin_code Name of the variable in \code{flow} containing the ids of the zone of origin.
#' By default this is the first column name in the flow input dataset.
#' @param dest_code Name of the variable in \code{flow} containing the ids of the zone of destination.
#' By default this is the second column name in the flow input dataset or the first column name in the
#' destinations if that is set.
#' @param zone_code_d Name of the variable in \code{destinations} containing the ids of the zone.
#' By default this is the first column names in the destinations.
#' @param silent TRUE by default, setting it to TRUE will show you the matching columns
#' @export
#' @examples
#' data(flow) # load example data - see ?flow for mor information
#' data(cents)
#' newflowlines <- od2line(flow = flow, zones = cents)
#' newflowlines2 <- od2line2(flow = flow, zones = cents)
#' plot(cents)
#' lines(newflowlines, lwd = 3)
#' lines(newflowlines2, col = "white")
#' nfl_sldf <- SpatialLinesDataFrame(newflowlines, flow, match.ID = FALSE)
#' identical(nfl_sldf, newflowlines)
#' # When destinations are different
#' data(destinations)
#' head(flow_dests[1:5]) # check data
#' head(destinations@data[1:5])
#' flowlines_dests = od2line(flow_dests, cents, destinations = destinations, silent = FALSE)
#' plot(flowlines_dests)
#' @name od2line
NULL

#' @rdname od2line
#' @export
od2line <- function(flow, zones, destinations = NULL,
                    zone_code = names(zones)[1],
                    origin_code = names(flow)[1],
                    dest_code = names(flow)[2],
                    zone_code_d = NA, silent = TRUE){
  l <- vector("list", nrow(flow))

  if(is.null(destinations)){
    if(!silent){
      message(paste("Matching", zone_code, "in the zones to", origin_code, "and", dest_code,
                    "for origins and destinations respectively"))
    }
    for(i in 1:nrow(flow)){
      from <- zones@data[[zone_code]] %in% flow[[origin_code]][i]
      if(sum(from) == 0)
        warning(paste0("No match for line ", i))
      to <- zones@data[[zone_code]] %in% flow[[dest_code]][i]
      if(sum(to) == 0 & sum(from) == 1)
        warning(paste0("No match for line ", i))
      x <- sp::coordinates(zones[from, ])
      y <- sp::coordinates(zones[to, ])
      l[[i]] <- sp::Lines(list(sp::Line(rbind(x, y))), as.character(i))
    }
  } else {
    if(is.na(zone_code_d)){
      zone_code_d <- names(destinations)[1]
    }
    if(!silent){
      message(paste("Matching", zone_code, "in the zones and", zone_code_d,  "in the destinations,\nto",
                    origin_code, "and", dest_code,
                    "for origins and destinations respectively"))
    }
    for(i in 1:nrow(flow)){
      from <- zones@data[[zone_code]] %in% flow[[origin_code]][i]
      if(sum(from) == 0)
        warning(paste0("No match for line ", i))
      to <- destinations@data[[zone_code_d]] %in% flow[[dest_code]][i]
      if(sum(to) == 0 & sum(from) == 1)
        warning(paste0("No match for line ", i))
      x <- sp::coordinates(zones[from, ])
      y <- sp::coordinates(destinations[to, ])
      l[[i]] <- sp::Lines(list(sp::Line(rbind(x, y))), as.character(i))
    }
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
#' data(flowlines)
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
      out <- raster::bind(out, lpoints)
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
#' @param list_output If FALSE (default) assumes SpatialLinesDataFrame output. Set to TRUE to save output as a list.
#' @param l_id Character string naming the id field from the input lines data,
#' typically the origin and destination ids pasted together. If absent, the row name of the
#' straight lines will be used.
#' @param n_processes The number of processes the routing requests should be made from, default 1.
#' @param ... Arguments passed to the routing function, e.g. \code{\link{route_cyclestreet}}
#' @inheritParams route_cyclestreet
#' @export
#' @examples
#' \dontrun{
#' data(flowlines)
#' l = flowlines[2:5,]
#' rf <- line2route(l = l, "route_cyclestreet", plan = "fastest")
#' rq <- line2route(l = l, plan = "quietest", silent = TRUE)
#' plot(rf, col = "red")
#' plot(rq, col = "green", add = TRUE)
#' plot(l, add = T)
#' # Plot for a single line to compare 'fastest' and 'quietest' route
#' n = 2
#' plot(l[n,])
#' lines(rf[n,], col = "red")
#' lines(rq[n,], col = "green")
#' # Example with list output
#' l <- l[1:3,]
#' rf_list <- line2route(l = l, list_output = TRUE)
#' class(rf_list)       # list output
#' class(rf_list[[2]])  # but individual elements are spatial
#' rf_list_of_lists <- line2route(l = l, list_output = TRUE, save_raw = TRUE)
#' class(rf_list_of_lists)       # list output
#' class(rf_list_of_lists[[2]])  # but individual elements are spatial
#' # illustration of how the l_id argument works:
#' rf$id # has id as l has "id" field
#' l$id <- NULL # remove id field for testing
#' rf_no_id <- line2route(l)
#' rf_no_id$id # [1] "1" "2" "3" "4"
#' rf_with_id = line2route(l, l_id = "All")
#' rf_with_id$id # [1] 38 10 44
#' # demo with parallel version spliting route API requests over multiple CPU processes
#' rf_parr <- line2route(l = l, n_processes = 4)
#' l = flowlines[1:2,]
#' rf_with_err = line2route(l,  reporterrors = T)
#' # Now rf_with_err$error[2] has the correct error message
#' }
line2route <- function(l, route_fun = "route_cyclestreet", n_print = 10, list_output = FALSE, l_id = NA, n_processes = 1, ...){
  FUN <- match.fun(route_fun)
  ldf <- line2df(l)
  n_ldf <- nrow(ldf)

  error_fun <- function(e){
    warning(paste("Fail for line number", i))
    e
  }

  if(n_processes > 1){
    n_processes <- min(c(n_processes, n_ldf))
    cl <- parallel::makeCluster(n_processes)
    doParallel::registerDoParallel(cl)
  }

  if(n_processes > 1){
    rc <- foreach::foreach(i = 1:n_ldf, .errorhandling = "pass") %dopar% {
      FUN(from = c(ldf$fx[i], ldf$fy[i]), to = c(ldf$tx[i], ldf$ty[i]), ...)
    }
    parallel::stopCluster(cl)
  } else {
    rc <- as.list(rep(NA, length(l)))
    for(i in 1:n_ldf){
      rc[[i]] <- tryCatch({
        FUN(from = c(ldf$fx[i], ldf$fy[i]), to = c(ldf$tx[i], ldf$ty[i]), ...)
      }, error = error_fun)
      perc_temp <- i %% round(n_ldf / n_print)
      # print % of distances calculated
      if(!is.na(perc_temp) & perc_temp == 0){
        message(paste0(round(100 * i/n_ldf), " % out of ", n_ldf, " distances calculated"))
      }
    }
  }

  if(!list_output){
    # Set the names based on the first non failing line (then exit loop)
    for(i in 1:n_ldf){
      if(grepl("Spatial.*DataFrame", class(rc[[i]]))[1]) {
        rdata <- data.frame(matrix(nrow = nrow(l), ncol = ncol(rc[[i]]) + 1))
        names(rdata) <- c(names(rc[[i]]), "error")
        r <- l
        r@data <- rdata
        break
      }
    }

    # Copy rc into r including the data or copy the error into r
    for(i in 1:n_ldf){
      if(grepl("Spatial.*DataFrame", class(rc[[i]]))[1]) {
        r@lines[[i]] <- Lines(rc[[i]]@lines[[1]]@Lines, row.names(l[i,]))
        r@data[i,] <- c(rc[[i]]@data, error = NA)
      } else {
        r@data[i, "error"] <- rc[[i]][1]
      }
    }

    # Set the id in r
    l_ids <- c(l_id, "id")
    l_id <- l_ids[!is.na(l_ids)][1]
    r$id <- if(l_id %in% names(l)){
      l@data[[l_id]]
    } else {
      row.names(l)
    }
  }
  r
}

#' Convert straight SpatialLinesDataFrame from flow data into routes retrying
#' on connection (or other) intermittent failures
#'
#' @section Details:
#'
#' See \code{\link{line2route}} for the version that is not retried on errors.
#' @param lines A SpatialLinesDataFrame
#' @param pattern A regex that the error messages must not match to be retried, default "^Error: " i.e. do not retry errors starting with "Error: "
#' @param n_retry Number of times to retry
#' @inheritParams line2route
#' @export
#' @examples
#' \dontrun{
#' data(flowlines)
#' rf_list <- line2routeRetry(flowlines[1:2,], pattern = "nonexistanceerror", silent = F)
#' }
line2routeRetry <- function(lines, pattern = "^Error: ", n_retry = 3, ...) {
  routes <- line2route(lines, reporterrors = T, ...)

  # When the time is NA then the routing failed,
  # if there is no error message or the message matches the pattern select line to be retried
  failed_to_route <- lines[is.na(routes$time) & (is.na(routes$error) | !grepl(pattern, routes$error)),]
  if (nrow(failed_to_route) > 0 && n_retry > 0){
    ids <- routes$ids
    routes_retry <- line2routeRetry(failed_to_route, pattern = pattern, n_retry = n_retry-1,  ...)
    for (idx_retry in 1:nrow(routes_retry)) {
      # Merge in retried routes if they are Spatial DataFrames
      if(grepl("Spatial.*DataFrame", class(routes_retry[[idx_retry]]))) {
        idx_to_replace <- which(routes$id == routes_retry$id[idx_retry])

        routes@data[idx_to_replace,] <- routes_retry@data[idx_retry,]
        routes@lines[[idx_to_replace]] <- Lines(routes_retry@lines[[idx_retry]]@Lines, row.names(routes_retry[idx_retry,]))
      }
    }
  }
  routes
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
#' data(cents)
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
#' data(cents)
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
#' data(flowlines)
#' l <- flowlines[2:5,]
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
