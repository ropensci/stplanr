setClass("igraph")

#' An S4 class representing a (typically) transport network
#'
#' This class uses a combination of a SpatialLinesDataFrame and an igraph
#' object to represent transport networks that can be used for routing and
#' other network analyses.
#' @slot sl A SpatialLinesDataFrame with the geometry and other attributes
#' for each link the in network.
#' @slot g The graph network corresponding to \code{sl}.
#' @slot nb A list containing vectors of the nodes connected to each node
#' in the network.
#' @slot weightfield A character vector containing the variable (column) name
#' from the SpatialLinesDataFrame to be used for weighting the network.
setClass("SpatialLinesNetwork", representation(sl = "SpatialLinesDataFrame",
                                               g = "igraph", nb = "list", weightfield = "character"),
         validity = function(object) {
           stopifnot(length(object@sl) == length(igraph::E(object@g)))
           stopifnot(length(object@nb) == length(igraph::V(object@g)))
         })

#' Create object of class SpatialLinesNetwork from SpatialLinesDataFrame
#'
#' Creates a new SpatialLinesNetwork object that can be used for routing
#' analysis within R.
#'
#' @section Details:
#' This function is used to create a new SpatialLinesNetwork from an existing
#' SpatialLines or SpatialLinesDataFrame object. A typical use case is to
#' represent a transport network for routing and other network analysis
#' functions. This function and the corresponding SpatialLinesNetwork
#' class is an implementation of the SpatialLinesNetwork developed by
#' Edzer Pebesma and presented on \href{http://rpubs.com/edzer/6767}{RPubs}.
#' The original implementation has been rewritten to better support large
#' (i.e., detailed city-size) networks and to provide additional methods
#' useful for conducting transport research following on from the initial
#' examples provided by \href{http://rpubs.com/janoskaz/10396}{Janoska(2013)}.
#'
#' @param sl A SpatialLines or SpatialLinesDataFrame containing the lines to
#' use to create the network.
#' @param uselonglat A boolean value indicating if the data should be assumed
#' to be using WGS84 latitude/longitude coordinates. If \code{FALSE} or not
#' set, uses the coordinate system specified by the SpatialLines object.
#'
#' @references
#' Pebesma, E. (2013). Spatial Networks, URL:http://rpubs.com/edzer/6767.
#'
#' Janoska, Z. (2013). Find shortest path in spatial network,
#' URL:http://rpubs.com/janoskaz/10396.
#' @export
#' @examples
#' data(routes_fast)
#' rnet <- overline(sldf = routes_fast, attrib = "length")
#' SLN <- SpatialLinesNetwork(rnet)
#' weightfield(SLN) # field used to determine shortest path
#' shortpath <- sum_network_routes(SLN, 1, 50, sumvars = "length")
#' plot(shortpath, col = "red", lwd = 4)
#' plot(SLN, add = TRUE)
SpatialLinesNetwork = function(sl, uselonglat = FALSE) {
  stopifnot(is(sl, "SpatialLines"))
  if (!is(sl, "SpatialLinesDataFrame"))
    sl = new("SpatialLinesDataFrame", sl, data = data.frame(id = 1:length(sl)))
  if (!all(sapply(sl@lines, length) == 1))
    stop("SpatialLines is not simple: each Lines element should have only a single Line")
  # Generate graph data
  gdata = coord_matches(sl)
  s = gdata$s
  g = igraph::graph(gdata$pts0, directed = FALSE)  # edges
  nodes = s[gdata$upts, ]
  g$x = nodes[, 1]  # x-coordinate vertex
  g$y = nodes[, 2]  # y-coordinate vertex
  g$n = as.vector(table(gdata$pts0))  # nr of edges
  # line lengths:
  # If uselonglat == FALSE then checks if sl uses longlat coordinate
  # system/projection. If so, passes longlat=TRUE.
  sl$length = sapply(sl@lines, function(x) LineLength(x@Lines[[1]],longlat=ifelse(uselonglat == TRUE,TRUE,ifelse(length(grep("proj=longlat",sp::proj4string(sl))) > 0,TRUE,FALSE))))
  igraph::E(g)$weight = sl$length
  new("SpatialLinesNetwork", sl = sl, g = g, nb = gdata$nb, weightfield="length")
}

#' Plot a SpatialLinesNetwork
#'
#' @param x The SpatialLinesNetwork to plot
#' @param component The component of the network to plot. Valid values are "sl"
#' for the geographic (SpatialLines) representation or "graph" for the graph
#' representation.
#' @param ... Arguments to pass to relevant plot function.
#' @export
setMethod("plot", signature = c(x="SpatialLinesNetwork"),
          definition = function(x, component = "sl", ...){
            if (component == "sl") {
              sp::plot(x@sl, ...)
            }
            else if (component == "graph") {
              igraph::plot.igraph(x@g, ...)
            }
            else {
              stop("Value of component not valid")
            }
          })

#' Get or set weight field in SpatialLinesNetwork
#'
#' Get or set value of weight field in SpatialLinesNetwork
#' @section Details:
#' These functions manipulate the value of weightfield in a
#' SpatialLinesNetwork. When changing the value of weightfield, the weights
#' of the graph network are updated with the values of the corresponding
#' variables.
#'
#' @param x SpatialLinesNetwork to use
#' @param varname The name of the variable to set/use.
#' @param value Either the name of the variable to use as the weight field or
#' a dataframe or vector containing the weights to use if \code{varname} is
#' passed to the replacement function. If the dataframe contains multiple
#' columns, the column with the same name as \code{varname} is used,
#' otherwise the first column is used.
#' @name weightfield
NULL

#' @rdname weightfield
#' @export
setGeneric("weightfield",
           function(x) standardGeneric("weightfield"))

#' @rdname weightfield
#' @export
setGeneric("weightfield<-",
           function(x, value) standardGeneric("weightfield<-"))

#' @rdname weightfield
#' @export
setGeneric("weightfield<-",
           function(x, varname, value) standardGeneric("weightfield<-"))

#' @rdname weightfield
setMethod("weightfield", signature(x = "SpatialLinesNetwork"), definition = function(x) {
  x@weightfield
})

#' @rdname weightfield
# @aliases <-
setReplaceMethod("weightfield", signature(x = "SpatialLinesNetwork", value = "ANY"), definition = function(x, value) {
  if (!is(x,"SpatialLinesNetwork")) {
    stop("x not SpatialLinesNetwork")
  }
  x@weightfield <- value
  igraph::E(x@g)$weight <- x@sl@data[,value]
  x
})

#' @rdname weightfield
setReplaceMethod("weightfield", signature(x = "SpatialLinesNetwork", varname = "character", value = "ANY"),
                 definition = function(x, varname, value) {
                   if (is(value,"data.frame")) {
                     if (sum(varname %in% colnames(value)) > 0) {
                       value <- value[,varname]
                     }
                     else {
                       value <- value[,1]
                     }
                   }
                   if (length(value) != nrow(x@sl@data)) {
                     stop("Length of value is not the same as the number of rows in the SpatialLinesDataFrame.")
                   }
                   x@sl@data[,varname] <- value
                   x@weightfield <- varname
                   igraph::E(x@g)$weight <- x@sl@data[,varname]
                   x
                 })

#' Print a summary of a SpatialLinesNetwork
#'
#' @param object The SpatialLinesNetwork
#' @param ... Arguments to pass to relevant plot function.
#' @export
setMethod("summary", signature = c(object="SpatialLinesNetwork"),
        definition = function(object, ...){
        cat(paste0("Weight attribute field: ",object@weightfield))
        summary(object@g)
        sp::summary(object@sl)
})

#' Find graph node ID of closest node to given coordinates
#'
#' @section Details:
#' Finds the node ID of the closest point to a single coordinate pair (or a
#' set of coordinates) from a SpatialLinesNetwork.
#'
#' @param sln SpatialLinesNetwork to search.
#' @param x Either the x (longitude) coordinate value, a vector of x values,
#' a dataframe or matrix with (at least) two columns, the first for coordinate
#' for x (longitude) values and a second for y (latitude) values, or a named
#' vector of length two with values of 'lat' and 'lon'. The output of
#' RgoogleMaps::getGeoCode() either as a single result or as multiple (using
#' rbind() ) can also be used.
#' @param y Either the y (latitude) coordinate value or a vector of y values.
#' @param maxdist The maximum distance within which to match the nodes to
#' coordinates. If the SpatialLinesNetwork is projected then distance should
#' be in the same units as the projection. If longlat, then distance is in
#' metres. Default is 1000.
#' @return An integer value with the ID of the node closest to \code{(x,y)}
#' with a value of \code{NA} the closest node is further than \code{maxdist}
#' from \code{(x,y)}. If \code{x} is a vector, returns a vector of Node IDs.
#' @export
find_network_nodes <- function(sln, x, y = NULL, maxdist = 1000) {
  if(!is(sln, "SpatialLinesNetwork")) {
    stop("sln is not a SpatialLinesNetwork.")
  }
  if(is(x,"numeric")) {
    if(length(x) == 2 & sum(c('lat','lon') %in% names(x)) == 2) {
      y=x['lat']
      x=x['lon']
    }
  }
  if(is(x,"data.frame") == FALSE & is(x,"matrix") == FALSE) {
    if (missing(y)) {
      stop("x is not a data.frame and y is missing.")
    }
  }
  else {
    if (all(c('lat','lon') %in% colnames(x))) {
      y = x[,'lat']
      x = x[,'lon']
    }
    else {
      y = x[,2]
      x = x[,1]
    }
  }
  if (length(x) != length(y)) {
    stop("x and y are not of equal lengths")
  }

  longlat <- ifelse(is.projected(sln@sl) == TRUE, FALSE, TRUE)
  maxdist <- ifelse(longlat == TRUE, maxdist/1000, maxdist)

  # if (length(x) == 1) {
  #   # nodedists = geosphere::distHaversine(data.frame(x=sln@g$x, y=sln@g$y),c(x,y))
  #   nodedists = sp::spDists(x = as.matrix(data.frame(x=sln@g$x, y=sln@g$y)),
  #                           y = matrix(c(x,y),ncol=2),
  #                           longlat = longlat)
  #   nodeid = which(nodedists == min(nodedists))[1]
  #   mindist = nodedists[nodeid]
  #   nodeid = ifelse(mindist <= maxdist,nodeid,NA)
  # }
  # else {
    # nodeid = c()
    # mindist = c()
    # i <- 1
    # while (i <= length(x)) {
    #   # nodedists = geosphere::distHaversine(data.frame(x=sln@g$x, y=sln@g$y),c(x[i],y[i]))
    #   nodedists = sp::spDists(x = as.matrix(data.frame(x=sln@g$x, y=sln@g$y)),
    #                           y = matrix(c(x[i],y[i]),ncol=2),
    #                           longlat = longlat)
    #   nodeid[i] = which(nodedists == min(nodedists))[1]
    #   mindist[i] = nodedists[nodeid[i]]
    #   i <- i + 1
    # }
    distlist <- lapply(1:length(x), function(i, gxy){
      sp::spDists(x = gxy,
                  y = matrix(c(x[i],y[i]),ncol=2),
                  longlat = longlat)
      }, as.matrix(data.frame(x=sln@g$x, y=sln@g$y)))
    nodeid <- sapply(distlist,
                     function(x, maxdist){
                       ifelse(min(x) > maxdist, NA, which(x == min(x))[1])
                     },
                     maxdist)

    # nodeid <- ifelse(mindist <= maxdist, nodeid, NA)
  # }


  return(nodeid)

}

#' Summarise shortest path between nodes on network
#'
#' @section Details:
#' Find the shortest path on the network between specified nodes and returns
#' a SpatialLinesdataFrame containing the path(s) and summary statistics of
#' each one.
#'
#' @param sln The SpatialLinesNetwork to use.
#' @param start Node ID(s) of route starts.
#' @param end Node ID(s) of route ends.
#' @param sumvars Character vector of variables for which to calculate
#' summary statistics.
#' @export
sum_network_routes <- function(sln, start, end, sumvars) {

  if (class(sln) != "SpatialLinesNetwork") {
    stop("sln is not a SpatialLinesNetwork.")
  }
  if (missing(start) | missing(end)) {
    stop("start or end is missing")
  }
  if (length(start) != length(end)) {
    stop("start and end not the same length.")
  }

  routesegs <- lapply(1:length(start), function(i) {
      unlist(igraph::get.shortest.paths(sln@g, start[i], end[i], output="epath")$epath)
    })
  routecoords <- mapply(function(routesegs, start) {
      join_spatiallines_coords(sln@sl[routesegs,],sln@g$x[start],sln@g$y[start])
    },
    routesegs, start, SIMPLIFY = FALSE)
  routecoords <- lapply(1:length(start), function(i) {
    if(nrow(routecoords[[i]]) > 0){
      routecoords[[i]]
    } else {
      matrix(c(sln@g$x[start[i]], sln@g$y[start[i]], sln@g$x[end[i]], sln@g$y[end[i]]), byrow=TRUE, nrow=2)
    }
  })
  routedata <- setNames(data.frame(cbind(1:length(routesegs), do.call(rbind, lapply(routesegs, function(routesegs, sumvars) {
    matrix(
      sapply(1:length(sumvars),
             FUN=function(j) {
               if(length(routesegs) == 0) { NA } else { sum(sln@sl[routesegs,]@data[sumvars[j]]) }
             }
      ), nrow=1)
    }, sumvars)))), c('ID',paste0('sum_',sumvars)))
  routedata$pathfound <- ifelse(unlist(lapply(routesegs, function(x){length(x)})) == 0, FALSE, TRUE)
  routelines <- mapply(function(x, i){sp::Lines(sp::Line(x), ID=i)}, routecoords, 1:length(routecoords))

  row.names(routedata) <- 1:nrow(routedata)
  sldf <- sp::SpatialLinesDataFrame(sp::SpatialLines(routelines, sln@sl@proj4string),
                                    routedata)

  return(sldf)

}