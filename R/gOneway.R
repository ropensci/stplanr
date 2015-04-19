#' Aggregate flows so they become non-directional
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
#' @return \code{gOneway} outputs a SpatialLinesDataFrame with single lines
#' and user-selected attribute values that have been aggregated. Only lines
#' with a distance (i.e. not intra-zone flows) are included
#' @export
#' @examples
#' library(sp)
#' data("flowlines")
#' plot(flowlines)
#' singlelines <- gOneway(flowlines, attrib = 3:14)
#' plot(x, lwd = 3, col = "red")
#' lines(singlelines) # check we've got the right lines
#' sum(singlelines$All)
#' nrow(singlelines)
gOneway <- function(x, attrib){
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

    if(class(attrib) == "character"){
      # aggregate the data for reverse flows
      singlelines[[attrib]][i] <- sum(singlelines[[attrib]][i]) + sum(x[[attrib]][l2])
    } else {
      # aggregate the data for reverse flows
      singlelines@data[i, attrib] <- singlelines@data[i, attrib] +
        as.numeric(x@data[l2, attrib])
    }

  }
  singlelines
}