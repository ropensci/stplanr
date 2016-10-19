#' Do the intersections between two geometries create lines?
#'
#' This is a function required in \code{\link{overline}}. It identifies
#' whether sets of lines overlap (beyond shared points) or
#' not.
#'
#' @param g1 A SpatialLinesDataFrame
#' @param g2 A SpatialLinesDataFrame
#' @export
#' @examples \dontrun{
#' data(routes_fast)
#' rnet <- overline(routes_fast[c(2, 3, 22),], attrib = "length")
#' r1 <- routes_fast[2,]
#' r2 <- routes_fast[3,]
#' r3 <- routes_fast[22,]
#' plot(rnet)
#' lines(r3, col = "red") # line without overlaps
#' islines(r1, r2)
#' islines(r1, r3)
#' islines(r2, r3)
#'
#' }
islines <- function(g1, g2){
  ## return TRUE if geometries intersect as lines, not points
  inherits(rgeos::gIntersection(g1,g2), "SpatialLines")
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
#' @examples \dontrun{
#' data(routes_fast)
#' rsec <- gsection(routes_fast)
#' plot(routes_fast)
#' lines(rsec, col = "red", lwd = 3)
#' length(rsec)
#' set.seed(5)
#' sel <- sample(length(rsec), 20)
#' plot(rsec[sel,], col = "blue", add = TRUE, lwd = 3) # overlapping lines
#' }
gsection <- function(sl){
  ## union and merge and disaggregate to make a
  ## set of non-overlapping line segments
  sp::disaggregate(rgeos::gLineMerge(rgeos::gUnion(sl, sl)))
}

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
#' flowlines_1way <- maptools::spRbind(flowlines_sub, flowlines_2way)
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

    splitlinesdf <- data.frame(data.table::rbindlist(lapply(splitlines, function(x){x@data})))
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

#' Aggregate ods so they become non-directional, e.g. by summing travel in both directions.
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
#' @param x A data frame, representing an OD matrix
#' @param attrib A vector of column numbers or names
#' for deciding which attribute(s) of class numeric to
#' aggregate
#' @param id1 Optional (it is assumed to be the first column)
#' text string referring to the name of the variable containing
#' the unique id of the origin
#' @param id2 Optional (it is assumed to be the second column)
#' text string referring to the name of the variable containing
#' the unique id of the destination
#'
#' @return \code{onewayid} outputs a data.frame with rows containing
#' results for the user-selected attribute values that have been aggregated.
#' @export
#' @examples
#' data(flow)
#' flow_oneway = onewayid(flow, attrib = 3)
#' nrow(flow_oneway) < nrow(flow) # result has fewer rows
#' sum(flow$All) == sum(flow_oneway$All) # but the same total flow
#' # using names instead of index for attribute
#' onewayid(flow, attrib = "All")
#' # using many attributes to aggregate
#' attrib = which(vapply(flow, is.numeric, TRUE))
#' flow_oneway = onewayid(flow, attrib = attrib)
#' colSums(flow_oneway[attrib]) == colSums(flow[attrib]) # test if the colSums are equal
#' # Demonstrate the results from onewayid and onewaygeo are identical
#' flow_oneway_geo = onewaygeo(flowlines, attrib = attrib)
#' plot(flow_oneway$All, flow_oneway_geo$All)
onewayid <- function(x, attrib, id1 = names(x)[1], id2 = names(x)[2]){

  if(is.numeric(attrib)){
    attrib_names = names(x)[attrib]
  } else {
    attrib_names = attrib
    attrib = which(names(x) %in% attrib)
  }

  x_oneway <- x %>%
    dplyr::mutate_(stplanr.id1 = id1,
                   stplanr.id2 = id2,
                   stplanr.key = ~paste(pmin(stplanr.id1, stplanr.id2), pmax(stplanr.id1, stplanr.id2))) %>%
    dplyr::group_by_(quote(stplanr.key)) %>%
    dplyr::mutate(is_two_way = ifelse(n() > 1, TRUE, FALSE)) %>%
    dplyr::mutate_each("sum", attrib) %>%
    dplyr::summarise_each_(funs("stplanr.first"),c(id1, id2, attrib, ~is_two_way)) %>%
    dplyr::select_(quote(-stplanr.key))

  return(x_oneway)

}

stplanr.first <- function(...) {
  dplyr::first(...)
}