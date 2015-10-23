#' Do the intersections between two geometries create lines?
#'
#' This is a function required in \code{\link{overline}}. It identifies
#' whether sets of lines overlap (beyond shared points) or
#' not.
#'
#' @param g1 A SpatialLinesDataFrame
#' @param g2 A SpatialLinesDataFrame
#' @export
#'
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
#' @param attrib A text string corresponding to the variable in \code{sldf$} on
#' which the function will operate.
#' @param fun The function used to aggregate the grouped values (default: sum)
#' @param na.zero Sets whether aggregated values with a value of zero are removed.
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
#' }
overline <- function(sldf, attrib, fun = sum, na.zero = FALSE){
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
  aggs = sapply(overs, function(os){fun(sldf[[attrib]][os])})

  ## make a SLDF with the named attribibute:
  sldf = sp::SpatialLinesDataFrame(slu, data.frame(Z=aggs))
  names(sldf) = attrib

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
#' data("flowlines")
#' plot(flowlines)
#' singlelines <- onewaygeo(flowlines, attrib = 3:14)
#' plot(singlelines, lwd = 3, col = "red")
#' lines(singlelines) # check we've got the right lines
#' sum(singlelines$All)
#' nrow(singlelines)
onewaygeo <- function(x, attrib){
  geq <- rgeos::gEquals(x, x, byid = T)
  sel1 <- !duplicated(geq) # repeated rows
  sel2 <- rowSums(geq) > 1 # all features with an overlap
  sel3 <- rgeos::gLength(x, byid = TRUE) > 0 # remove points
  sel4 <- sel1 & sel2 & sel3
  singlelines <- x[sel4,]
  otherlines <- x[!sel4, ] # the lines that are duplicated

  for(i in 1:nrow(singlelines)){
    # select matching line
    l2 <- which(rgeos::gEquals(singlelines[i, ], x, byid = TRUE))[2]

    # only perform aggregation on flows that have a return flow
    if(length(l2) > 0){
      if(class(attrib) == "character"){
        # aggregate the data for reverse flows
        singlelines[[attrib]][i] <- singlelines[[attrib]][i] + x[[attrib]][l2]
      } else {
        # aggregate the data for reverse flows
        singlelines@data[i, attrib] <- singlelines@data[i, attrib] +
          as.numeric(x@data[l2, attrib])
      }
    }

  }
  singlelines
}

#' Aggregate flows so they become non-directional (by id - the faster way)
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
#' @param id1 A text string referring to the name of the variable containing the unique id of the origin
#' @param id2 A text string referring to the name of the variable containing the unique id of the destination
#'
#' @return \code{onewayid} outputs a SpatialLinesDataFrame with single lines
#' and user-selected attribute values that have been aggregated. Only lines
#' with a distance (i.e. not intra-zone flows) are included.
#' @export
#' @examples
#' data("flowlines")
#' id1 <- names(flowlines)[1]
#' id2 <- names(flowlines)[2]
#' plot(flowlines)
#' singlelines <- onewayid(flowlines, attrib = 3:14, id1, id2)
#' lines(singlelines) # check we've got the right lines
#' sum(singlelines$All)
#' nrow(singlelines)
#' sl2 <- onewaygeo(flowlines, attrib = 3:14)
#' # Demonstrate the results from onewayid and onewaygeo are identical
#` identical(singlelines, sl2)
onewayid <- function(x, attrib, id1 = names(x)[1], id2 = names(x)[2]){
  ids <- cbind(x[[id1]], x[[id2]])
  idsort <- t(apply(ids, 1, sort))
  # duplicate pairs - see http://stackoverflow.com/questions/9028369/
  sel <- !duplicated(idsort) & rgeos::gLength(x, byid = TRUE) > 0

  singlelines <- x[sel,]
  if(sum(sel) != nrow(x)) otherlines <- x[!sel, ] # the lines that are duplicated

  for(i in 1:nrow(singlelines)){
    # select matching lines
    idp1 <- paste0(x[[id1]], x[[id2]])
    idp2 <- paste0(x[[id2]], x[[id1]])
    l2 <- which(idp2 %in% idp1[sel][i])

    # only perform aggregation on flows that have a return flow
    if(length(l2) > 0){
      if(class(attrib) == "character"){
        # aggregate the data for reverse flows
        singlelines[[attrib]][i] <- singlelines[[attrib]][i] + x[[attrib]][l2]
      } else {
        # aggregate the data for reverse flows
        singlelines@data[i, attrib] <- singlelines@data[i, attrib] +
          as.numeric(x@data[l2, attrib])
      }
    }

  }
  singlelines
}

