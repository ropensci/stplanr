#' Do the intersections between two geometries create lines?
#'
#' This is a function required in \code{\link{overline}}. It identifies
#' whether sets of lines overlap (beyond shared points) or
#' not.
#'
#' @param g1 A spatial object
#' @param g2 A spatial object
#' @export
#' @examples \dontrun{
#' rnet <- overline(routes_fast[c(2, 3, 22),], attrib = "length")
#' plot(rnet)
#' lines(routes_fast[22,], col = "red") # line without overlaps
#' islines(routes_fast[2,], routes_fast[3,])
#' islines(routes_fast[2,], routes_fast[22,])
#' # sf implementation
#' islines(routes_fast_sf[2,], routes_fast_sf[3,])
#' islines(routes_fast_sf[2,], routes_fast_sf[22,])
#' }
islines <- function(g1, g2) {
  UseMethod("islines")
}
islines.Spatial <- function(g1, g2){
  ## return TRUE if geometries intersect as lines, not points
  inherits(rgeos::gIntersection(g1,g2), "SpatialLines")
}
islines.sf <- function(g1, g2) {
  sf::st_geometry_type(sf::st_intersection(sf::st_geometry(g1), sf::st_geometry(g2))) == "MULTILINESTRING"
}

#' Function to split overlapping SpatialLines into segments
#'
#' Divides SpatialLinesDataFrame objects into separate Lines.
#' Each new Lines object is the aggregate of a single number
#' of aggregated lines.
#'
#' @param sl SpatialLinesDataFrame with overlapping Lines to split by
#' number of overlapping features.
#' @export
#' @examples
#' sl <- routes_fast[1:4,]
#' rsec <- gsection(sl)
#' plot(sl[1], lwd = 9, col = "yellow")
#' plot(rsec, col = 1:length(rsec), add = TRUE)
#' sl <- routes_fast_sf[1:4,]
#' rsec <- gsection(sl)
gsection <- function(sl) {
  UseMethod("gsection")
}
gsection.Spatial <- function(sl){
  ## union and merge and disaggregate to make a
  ## set of non-overlapping line segments
  u <- rgeos::gUnion(sl, sl)
  u_merged <- rgeos::gLineMerge(u)
  sp::disaggregate(u_merged)
}
gsection.sf <- function(sl){
  u <- sf::st_union(sf::st_geometry(sl))
  u_merged <- sf::st_line_merge(u)
  st_cast(u_merged, "LINESTRING")
}

  # # clean lines
  # if(min(overlap_totals) == 0) {
  #   dists <- sf::st_length(sl)
  #   if(min(as.numeric(dists)) == 0) {
  #     # clean so only lines with dists there (could be function)
  #     sl <- sl[as.numeric(dists) > 0,]
  #     # recalculate overlaps
  #     m <- sf::st_overlaps(sl, sparse = FALSE)
  #     overlap_totals <- apply(m, 2, sum)
  #     non_overlapping_lines <- sl[overlap_totals == 0,]
  #   }
  # }
  #
  # # find overlaps associated with minimally intersecting line
  # over_ord <- order(overlap_totals) # which order to run them in

# see https://github.com/edzer/sp/blob/356ff6972cf62a266b75c41b1337d85e10381811/R/disaggregate.R
# st_disaggregate <- function(sl) {
#   nl <- vector(length = length(sl$geometry))
#   for(i in 1:length(sl$geometry)) {
#   ...
#   }
# }

#' Label SpatialLinesDataFrame objects
#'
#' This function adds labels to lines plotted using base graphics. Largely
#' for illustrative purposes, not designed for publication-quality
#' graphics.
#'
#' @param sldf A SpatialLinesDataFrame with overlapping elements
#' @param attrib A text string corresponding to a named variable in \code{sldf}
#'
#' @author Barry Rowlingson
#'
#' @export
lineLabels <- function(sldf, attrib){
  text(sp::coordinates(
    rgeos::gCentroid(sldf, byid = TRUE)
    ), labels = sldf[[attrib]])
}

#' Convert series of overlapping lines into a route network
#'
#' This function takes a series of Lines stored in a
#'  \code{SpatialLinesDataFrame}
#' and converts these into a single route network.
#'
#' @param sldf A SpatialLinesDataFrame with overlapping elements
#' @param attrib A character vector corresponding to the variables in
#' \code{sldf$} on which the function(s) will operate.
#' @param fun The function(s) used to aggregate the grouped values (default: sum).
#' If length of \code{fun} is smaller than \code{attrib} then the functions are
#' repeated for subsequent attributes.
#' @param na.zero Sets whether aggregated values with a value of zero are removed.
#' @param byvars Character vector containing the column names to use for grouping
#'
#' @author Barry Rowlingson
#' @references
#'
#' Rowlingson, B (2015). Overlaying lines and aggregating their values for
#'  overlapping segments. Reproducible question from
#'  \url{http://gis.stackexchange.com}. See \url{http://gis.stackexchange.com/questions/139681/overlaying-lines-and-aggregating-their-values-for-overlapping-segments}.
#' @export
#' @examples \dontrun{
#' data(routes_fast)
#' data(cents)
#' rnet <- overline(sldf = routes_fast[1:7,], attrib = "length")
#' plot(rnet)
#' points(cents)
#' lineLabels(sldf = rnet, "length")
#' sum(routes_fast$length[1:7], na.rm = TRUE) # verify highest flow
#' data(flowlines)
#' plot(flowlines)
#' aggflow <- overline(flowlines, attrib = "All")
#' nrow(aggflow)
#' aggflow2 <- overline(flowlines, attrib = "All", na.zero = TRUE)
#' plot(aggflow2) # 8 lines
#' sel <- as.logical(colSums(gEquals(flowlines, aggflow2, byid = TRUE)))
#' flowlines_sub <- flowlines[!sel,]
#' plot(flowlines_sub)
#' flowlines_2way <- flowlines[sel,]
#' library(maptools)
#' flowlines_2way <- spChFIDs(flowlines_2way, as.character(100001:(nrow(flowlines_2way) + 100000)))
#' flowlines_1way <- raster::bind(flowlines_sub, flowlines_2way)
#' overlaps <- over()
#' nrow(overlaps)
#' routes_fast$group = rep(1:3, length.out = nrow(routes_fast))
#' rnet_grouped = overline(routes_fast, attrib = "length", byvars = "group")
#' plot(rnet_grouped, col = rnet_grouped$group, lwd =
#'   rnet_grouped$length / mean(rnet_grouped$length) * 3)
#' }
overline <- function(sldf, attrib, fun = sum, na.zero = FALSE, byvars = NA){

  fun <- c(fun)
  if (length(fun) < length(attrib)) {
    fun <- rep(c(fun),length.out=length(attrib))
  }

  if (is.na(byvars[1]) == TRUE) {
    ## simplify down to SpatialLines
    sl = as(sldf, "SpatialLines")
    ## get the line sections that make the network
    slu = gsection(sl)
    ## overlay network with routes
    overs = sp::over(slu, sl, returnList=TRUE)
    ## overlay is true if end points overlay, so filter them out:
    overs = lapply(1:length(overs), function(islu){
      Filter(function(isl){
        islines(sl[isl,],slu[islu,])
      }, overs[[islu]])
    })
    ## now aggregate the required attribibute using fun():
    #aggs = sapply(overs, function(os){fun(sldf[[attrib]][os])})
    aggs <- setNames(
      as.data.frame(
        lapply(1:length(attrib),
               function(y, overs, attribs, aggfuns){
                 sapply(overs, function(os,attrib,fun2){
                   fun2(sldf[[attrib]][os])},
                   attrib=attribs[y],
                   fun2=aggfuns[[y]])
                 },
               overs,
               attrib,
               fun)),
      attrib)

    ## make a SLDF with the named attribibute:
    sldf = sp::SpatialLinesDataFrame(slu, aggs)
    #names(sldf) = attrib
  } else {

    splitlines <- lapply(
      split(sldf, sldf@data[,byvars]),
      function(x, attrib, gvar){
        groupingcat <- unname(unlist(unique(x@data[,gvar])))
        sl = as(x, "SpatialLines")
        slu = gsection(sl)
        overs = sp::over(slu, sl, returnList = TRUE)
        overs = lapply(1:length(overs), function(islu) {
          Filter(function(isl){islines(sl[isl,],slu[islu,])}, overs[[islu]])
        })
        #aggs = sapply(overs, function(os){fun(x[[attrib]][os])})
        aggs <- setNames(
          as.data.frame(
            lapply(1:length(attrib),
                   function(y, overs, attribs, aggfuns){
                     sapply(overs, function(os,attrib,fun2){
                       fun2(x[[attrib]][os])},
                       attrib=attribs[y],
                       fun2=aggfuns[[y]])
                   },
                   overs,
                   attrib,
                   fun)
            ),
          attrib)
        sldf = sp::SpatialLinesDataFrame(slu, cbind(aggs,as.data.frame(matrix(groupingcat,nrow=1))))
        names(sldf) = c(attrib,gvar)
        sldf <- spChFIDs(sldf, paste(paste(groupingcat,collapse='.'),row.names(sldf@data),sep='.'))
        sldf
      },
      attrib,
      c(byvars)
    )

    splitlinesdf <- data.frame(dplyr::bind_rows(lapply(splitlines, function(x){x@data})))
    row.names(splitlinesdf) <- unname(unlist(lapply(splitlines, function(x){row.names(x@data)})))

    sldf <- SpatialLinesDataFrame(
      SpatialLines(unlist(lapply(splitlines, function(x){x@lines}), recursive = FALSE),
                   proj4string = splitlines[[1]]@proj4string),
      splitlinesdf
    )

  }

  ## remove lines with attribute values of zero
  if(na.zero == TRUE){
    sldf <- sldf[sldf[[attrib]] > 0, ]
  }

  sldf
}

#' Aggregate flows so they become non-directional (by geometry - the slow way)
#'
#' Flow data often contains movement in two directions: from point A to point B
#' and then from B to A. This can be problematic for transport planning, because
#' the magnitude of flow along a route can be masked by flows the other direction.
#' If only the largest flow in either direction is captured in an analysis, for
#' example, the true extent of travel will by heavily under-estimated for
#' OD pairs which have similar amounts of travel in both directions.
#' Flows in both direction are often represented by overlapping lines with
#' identical geometries (see \code{\link{flowlines}}) which can be confusing
#' for users and are difficult to plot.
#'
#' This function aggregates directional flows into non-directional flows,
#' potentially halving the number of lines objects and reducing the number
#' of overlapping lines to zero.
#'
#' @param x A SpatialLinesDataFrame
#' @param attrib A text string containing the name of the line's attribute to
#' aggregate or a numeric vector of the columns to be aggregated
#'
#' @return \code{onewaygeo} outputs a SpatialLinesDataFrame with single lines
#' and user-selected attribute values that have been aggregated. Only lines
#' with a distance (i.e. not intra-zone flows) are included
#' @export
#' @examples
#' data(flowlines)
#' plot(flowlines)
#' singlelines <- onewaygeo(flowlines, attrib = 3:14)
#' plot(singlelines, lwd = 3, col = "red")
#' lines(singlelines) # check we've got the right lines
#' sum(singlelines$All)
#' nrow(singlelines)
onewaygeo <- function(x, attrib){
  geq <- rgeos::gEquals(x, x, byid = TRUE) | rgeos::gEqualsExact(x, x, byid = TRUE)
  sel1 <- !duplicated(geq) # repeated rows
  singlelines <- x[sel1,]

  singlelines@data[,attrib] <- (matrix(
    unlist(
      lapply(
        apply(geq, 1, function(x){
          which(x == TRUE)
        }),
        function(y,x) {
          colSums(x[y,3:14]@data)
        }, x)),
    nrow=49,
    byrow=TRUE))[sel1,]

  return(singlelines)
}

`%>%` <- dplyr::`%>%`
