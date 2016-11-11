# Functions for aggregating two-way OD pairs into 'oneway' lines
#' Aggregate ods so they become non-directional
#'
#' For example, sum total travel in both directions.
#' @param x A data frame or SpatialLinesDataFrame, representing an OD matrix
#' @param attrib A vector of column numbers or names
#' for deciding which attribute(s) of class numeric to
#' aggregate
#' @param id1 Optional (it is assumed to be the first column)
#' text string referring to the name of the variable containing
#' the unique id of the origin
#' @param id2 Optional (it is assumed to be the second column)
#' text string referring to the name of the variable containing
#' the unique id of the destination
#' @return \code{onewayid} outputs a data.frame with rows containing
#' results for the user-selected attribute values that have been aggregated.
#' @export
onewayid <- function(x, attrib, id1 = names(x)[1], id2 = names(x)[2]) UseMethod("onewayid")

#' @name onewayid
#' @details
#' Flow data often contains movement in two directions: from point A to point B
#' and then from B to A. This can be problematic for transport planning, because
#' the magnitude of flow along a route can be masked by flows the other direction.
#' If only the largest flow in either direction is captured in an analysis, for
#' example, the true extent of travel will by heavily under-estimated for
#' OD pairs which have similar amounts of travel in both directions.
#' Flows in both direction are often represented by overlapping lines with
#' identical geometries (see \code{\link{flowlines}}) which can be confusing
#' for users and are difficult to plot.
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
#' # With spaces in id names
#' names(flow)[1] = "Area of residence"
#' onewayid(flow, attrib = 3)
#' @export
onewayid.data.frame <- function(x, attrib, id1 = names(x)[1], id2 = names(x)[2]){

  if(is.numeric(attrib)){
    attrib_names = names(x)[attrib]
  } else {
    attrib_names = attrib
    attrib = which(names(x) %in% attrib)
  }

  stplanr.ids <- od_id_order(x, id1, id2)
  x <- cbind(x, stplanr.ids)

  x_oneway <- dplyr::group_by_(x, quote(stplanr.key)) %>%
    dplyr::mutate(is_two_way = ifelse(n() > 1, TRUE, FALSE)) %>%
    dplyr::mutate_each("sum", attrib) %>%
    dplyr::summarise_each_(dplyr::funs("first"), c(as.name(id1), as.name(id2), attrib, ~is_two_way))

  x_oneway$stplanr.key <- NULL

  return(x_oneway)

}

#' @name onewayid
#' @examples
#' # with spatial data
#' data(flowlines)
#' fo <- onewayid(flowlines, attrib = "All")
#' head(fo@data)
#' plot(fo)
#' sum(fo$All) == sum(flowlines$All)
#' # test results for one line
#' n <- 3
#' plot(fo[n,], lwd = 20, add = TRUE)
#' f_over_n <- rgeos::gEquals(fo[n,], flowlines["All"], byid = TRUE)
#' sum(flowlines$All[f_over_n]) == sum(fo$All[n]) # check aggregation worked
#' plot(flowlines[which(f_over_n)[1],], add = TRUE, col = "white", lwd = 10)
#' plot(flowlines[which(f_over_n)[2],], add = TRUE, lwd = 5)
#' @export
onewayid.SpatialLines <- function(x, attrib, id1 = names(x)[1], id2 = names(x)[2]){

  x_geom <- sp::SpatialLines(x@lines, proj4string = CRS(proj4string(x)))
  x <- x@data

  x_oneway = onewayid(x, id1, id2, attrib = attrib)

  stplanr.key <- od_id_order(x_oneway[c(id1, id2)])$stplanr.key

  if(length(x_geom) != nrow(x_oneway)) {
    id_old <- paste(x[[id1]], x[[id2]])
    sel <- id_old %in% stplanr.key
    x_geom <- x_geom[sel,]
  }

  x_oneway <- sp::SpatialLinesDataFrame(sl = x_geom, data = x_oneway, match.ID = FALSE)

  return(x_oneway)

}

#' Generate ordered ids of OD pairs so lowest is always first
#'
#' @inheritParams onewayid
#'
#' @examples
#' x = data.frame(id1 = c(1, 1, 2, 2, 3), id2 = c(1, 2, 3, 1, 4))
#' od_id_order(x) # 4th line switches id1 and id2 so stplanr.key is in order
#' @export
od_id_order <- function(x, id1 = names(x)[1], id2 = names(x)[2]){
  dplyr::transmute_(x, stplanr.id1 = as.name(id1),
                    stplanr.id2 = as.name(id2),
                    stplanr.key = ~paste(pmin(stplanr.id1, stplanr.id2), pmax(stplanr.id1, stplanr.id2)))
}
