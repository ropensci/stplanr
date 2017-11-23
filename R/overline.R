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
#' @param buff_dist A number specifying the distance in meters of the buffer to be used to crop lines before running the operation. If the distance is zero (the default) touching but non-overlapping lines may be aggregated.
#' @export
#' @examples
#' sl <- routes_fast[2:4,]
#' rsec <- gsection(sl)
#' rsec_buff <- gsection(sl, buff_dist = 1)
#' plot(sl[1], lwd = 9, col = 1:nrow(sl))
#' plot(rsec, col = 5 + (1:length(rsec)), add = TRUE, lwd = 3)
#' plot(rsec_buff, col = 5 + (1:length(rsec_buff)), add = TRUE, lwd = 3)
#' # sf implementation (needs lwgeom)
#' if (!is.na(sf::sf_extSoftVersion()["lwgeom"])) {
#'   sl <- routes_fast_sf[2:4,]
#'   rsec <- gsection(sl)
#'   rsec <- gsection(sl, buff_dist = 100) # 4 features: issue
#' }
gsection <- function(sl, buff_dist = 0) {
  UseMethod("gsection")
}
#' @export
gsection.Spatial <- function(sl, buff_dist = 0){
  if(buff_dist > 0) {
    sl = geo_toptail(sl, toptail_dist = buff_dist)
  }
  overlapping = rgeos::gOverlaps(sl, byid = T)
  u <- rgeos::gUnion(sl, sl)
  u_merged <- rgeos::gLineMerge(u)
  sp::disaggregate(u_merged)
}
#' @export
gsection.sf <- function(sl, buff_dist = 0){

  if(buff_dist > 0) {
    sl = geo_toptail(sl, toptail_dist = buff_dist)
  }

  u <- sf::st_union(sl)
  u_merged <- sf::st_line_merge(u)
  u_disag <- sf::st_cast(u_merged, "LINESTRING")

  u_disag

}
# gsection.sf <- function(sl, buff_dist = 0){
#
#   if(buff_dist > 0) {
#     sl = geo_toptail(sl, toptail_dist = buff_dist)
#   }
#
#   u <- sf::st_union(sf::st_geometry(sl))
#
#   # u_merged <- sf::st_line_merge(u)
#   # u_disag <- sf::st_cast(u, "LINESTRING")
#   p <- line2points(sf::as_Spatial(sf::st_geometry(sl)))
#   p <- sf::st_as_sf(p)
#   p_inter <- sf::st_intersection(x = sl, y = u)
#   p_inter_p <- sf::st_as_sfc(p_inter[sf::st_geometry_type(p_inter) == "POINT",])
#   p_inter_p <- p_inter_p[,0]
#   p <- rbind(p, p_inter_p)
#   slu <- sf::st_split(u_disag, p)
#
# }
#' Label SpatialLinesDataFrame objects
#'
#' This function adds labels to lines plotted using base graphics. Largely
#' for illustrative purposes, not designed for publication-quality
#' graphics.
#'
#' @param sl A SpatialLinesDataFrame with overlapping elements
#' @param attrib A text string corresponding to a named variable in \code{sl}
#'
#' @author Barry Rowlingson
#'
#' @export
lineLabels <- function(sl, attrib){
  text(sp::coordinates(
    rgeos::gCentroid(sl, byid = TRUE)
    ), labels = sl[[attrib]])
}

#' Convert series of overlapping lines into a route network
#'
#' This function takes a series of Lines stored in a
#'  \code{SpatialLinesDataFrame}
#' and converts these into a single route network.
#'
#' @param sl A SpatialLinesDataFrame with overlapping elements
#' @param attrib A character vector corresponding to the variables in
#' \code{sl$} on which the function(s) will operate.
#' @param fun The function(s) used to aggregate the grouped values (default: sum).
#' If length of \code{fun} is smaller than \code{attrib} then the functions are
#' repeated for subsequent attributes.
#' @param na.zero Sets whether aggregated values with a value of zero are removed.
#' @param byvars Character vector containing the column names to use for grouping
#' @inheritParams gsection
#' @author Barry Rowlingson
#' @references
#' Rowlingson, B (2015). Overlaying lines and aggregating their values for
#'  overlapping segments. Reproducible question from
#'  \url{http://gis.stackexchange.com}. See \url{http://gis.stackexchange.com/questions/139681/overlaying-lines-and-aggregating-their-values-for-overlapping-segments}.
#' @export
#' @examples
#' sl <- routes_fast[2:4,]
#' rnet1 <- overline(sl = sl, attrib = "length")
#' rnet2 <- overline(sl = sl, attrib = "length", buff_dist = 1)
#' plot(rnet1, lwd = rnet1$length / mean(rnet1$length))
#' plot(rnet2, lwd = rnet2$length / mean(rnet2$length))
#' \dontrun{
#' routes_fast$group = rep(1:3, length.out = nrow(routes_fast))
#' rnet_grouped = overline(routes_fast, attrib = "length", byvars = "group", buff_dist = 1)
#' plot(rnet_grouped, col = rnet_grouped$group, lwd =
#'   rnet_grouped$length / mean(rnet_grouped$length) * 3)
#' # sf methods
#' sl = routes_fast_sf[2:4, ]
#' overline(sl = sl, attrib = "length", buff_dist = 10)
#' rnet_sf = overline(routes_fast_sf, attrib = "length", buff_dist = 10)
#' plot(rnet_sf$geometry, lwd = rnet_sf$length / mean(rnet_sf$length))
#' }
overline <- function(sl, attrib, fun = sum, na.zero = FALSE, byvars = NA, buff_dist = 0) {
  UseMethod("overline")
}
#' @export
overline.sf <- function(sl, attrib, fun = sum, na.zero = FALSE, byvars = NA, buff_dist = 0) {

  sl_spatial <- SpatialLinesDataFrame(sl = sf::as_Spatial(sl$geometry), data = sf::st_set_geometry(sl, NULL), match.ID = FALSE)
  rnet_sp <- overline(sl_spatial, attrib, fun = fun, na.zero = na.zero, byvars = byvars, buff_dist = buff_dist)
  sf::st_as_sf(rnet_sp)

  # attempt to run with sf funs
  # slu <- gsection(sl, buff_dist = buff_dist)
  # aggs <- aggregate(sl[attrib], slu, FUN = fun, join = sf::st_overlaps, na.rm = FALSE)
  # aggs
}
#' @export
overline.Spatial <- function(sl, attrib, fun = sum, na.zero = FALSE, byvars = NA, buff_dist = 0){

  fun <- c(fun)
  if (length(fun) < length(attrib)) {
    fun <- rep(c(fun),length.out=length(attrib))
  }

  sl_sp <- as(sl, "SpatialLines")

  if(is.na(byvars[1]) == TRUE) {
    ## get the line sections that make the network
    slu <- gsection(sl, buff_dist = buff_dist)
    ## overlay network with routes
    overs = sp::over(slu, sl_sp, returnList = TRUE)
    ## overlay is true if end points overlay, so filter them out:
    overs = lapply(1:length(overs), function(islu){
      Filter(function(isl){
        islines(sl_sp[isl,], slu[islu,])
      }, overs[[islu]])
    })
    ## now aggregate the required attribibute using fun():
    #aggs = sapply(overs, function(os){fun(sl[[attrib]][os])})
    aggs <- setNames(
      as.data.frame(
        lapply(1:length(attrib),
               function(y, overs, attribs, aggfuns){
                 sapply(overs, function(os,attrib,fun2){
                   fun2(sl[[attrib]][os])},
                   attrib=attribs[y],
                   fun2=aggfuns[[y]])
                 },
               overs,
               attrib,
               fun)),
      attrib)

    ## make a sl with the named attribibute:
    sl = sp::SpatialLinesDataFrame(slu, aggs)
    #names(sl) = attrib
  } else {

    splitlines <- lapply(
      split(sl, sl@data[,byvars]),
      function(x, attrib, gvar){
        groupingcat <- unname(unlist(unique(x@data[,gvar])))
        sl_spg = as(x, "SpatialLines")
        slu = gsection(sl, buff_dist = buff_dist)
        overs = sp::over(slu, sl_spg, returnList = TRUE)
        overs = lapply(1:length(overs), function(islu) {
          Filter(function(isl){islines(sl_spg[isl,],slu[islu,])}, overs[[islu]])
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
        sl = sp::SpatialLinesDataFrame(slu, cbind(aggs,as.data.frame(matrix(groupingcat,nrow=1))))
        names(sl) = c(attrib,gvar)
        sl <- spChFIDs(sl, paste(paste(groupingcat,collapse='.'),row.names(sl@data),sep='.'))
        sl
      },
      attrib,
      c(byvars)
    )

    splitlinesdf <- data.frame(dplyr::bind_rows(lapply(splitlines, function(x){x@data})))
    row.names(splitlinesdf) <- unname(unlist(lapply(splitlines, function(x){row.names(x@data)})))

    sl <- SpatialLinesDataFrame(
      SpatialLines(unlist(lapply(splitlines, function(x){x@lines}), recursive = FALSE),
                   proj4string = splitlines[[1]]@proj4string),
      splitlinesdf
    )

  }

  ## remove lines with attribute values of zero
  if(na.zero == TRUE){
    sl <- sl[sl[[attrib]] > 0, ]
  }

  sl
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
#' plot(flowlines[1:30, ], lwd = flowlines$On.foot[1:30])
#' singlines <- onewaygeo(flowlines[1:30, ], attrib = which(names(flowlines) == "On.foot"))
#' plot(singlines, lwd = singlines$On.foot / 2, col = "red", add = TRUE)
#' \dontrun{
#' plot(flowlines, lwd = flowlines$All / 10)
#' singlelines <- onewaygeo(flowlines, attrib = 3:14)
#' plot(singlelines, lwd = singlelines$All / 20, col = "red", add = TRUE)
#' sum(singlelines$All) == sum(flowlines$All)
#' nrow(singlelines)
#' singlelines_sf <- onewaygeo(flowlines_sf, attrib = 3:14)
#' sum(singlelines_sf$All) == sum(flowlines_sf$All)
#' summary(singlelines$All == singlelines_sf$All)
#' }
onewaygeo <- function(x, attrib) {
  UseMethod("onewaygeo")
}
#' @export
onewaygeo.sf <- function(x, attrib) {
  geq <- sf::st_equals(x, x, sparse = FALSE) | sf::st_equals_exact(x, x, sparse = FALSE, par = 0.0)
  sel1 <- !duplicated(geq) # repeated rows
  x$matching_rows =  apply(geq, 1, function(x) paste0(formatC(which(x), width = 4, format = "d", flag = 0), collapse = "-"))

  singlelines <- stats::aggregate(x[attrib], list(x$matching_rows), FUN = sum)

  return(singlelines)
}
#' @export
onewaygeo.Spatial <- function(x, attrib){
  geq <- rgeos::gEquals(x, x, byid = TRUE) | rgeos::gEqualsExact(x, x, byid = TRUE)
  sel1 <- !duplicated(geq) # repeated rows
  singlelines <- x[sel1,]
  non_numeric_cols <- which(!sapply(x@data, is.numeric))
  keeper_cols <- sort(unique(c(non_numeric_cols, attrib)))

  singlelines@data[, attrib] <- (matrix(
    unlist(
      lapply(
        apply(geq, 1, function(x){
          which(x == TRUE)
        }),
        function(y, x) {
          colSums(x[y, attrib]@data)
        }, x)),
    nrow = nrow(x),
    byrow=TRUE))[sel1,]

  singlelines@data <- singlelines@data[keeper_cols]

  return(singlelines)
}
