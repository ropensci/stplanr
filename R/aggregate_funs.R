#' Aggregate OD data between polygon geometries
#'
#' @section Details:
#' Origin-destination ('OD') flow data is often provided
#' in the form of 1 line per flow with zone codes of origin and destination
#' centroids. This function aggregates OD flows between polygon geometries
#' allocating the original flows to larger zones based on area.
#'
#' @param flow A data frame representing the flow between two points
#' or zones. The first two columns of this data frame should correspond
#' to the first column of the data in the zones. Thus in \code{\link{cents}},
#' the first column is geo_code. This corresponds to the first two columns
#' of \code{\link{flow}}.
#' @param zones A SpatialPolygonsDataFrame or SpatialPointsDataFrame
#' representing the original centroids or boundaries of the travel flow data.
#' Note that in the case of a SpatialPointsDataFrame, the entirety of the flow
#' will be allocated to the polygon in which the point is located rather than
#' being distributed by area.
#' @param aggzones A SpatialPolygonsDataFrame containing the new
#' boundaries to aggregate to.
#' @param aggzone_points Points representing origins of OD flows
#' (typically population-weighted centroids)
#' @param cols A character vector containing the names of columns on which to
#' apply FUN. By default, all numeric columns are aggregated.
#' @param aggcols A character vector containing the names of columns in
#' aggzones to retain in the aggregated data.frame. By default,
#' only the first column is retained. These columns are renamed with a prefix
#' of "o_" and "d_".
#' @param FUN Function to use on aggregation. Default is sum.
#' @inheritParams sp_aggregate
#' @return data.frame containing the aggregated od flows.
#'
#' @export
#' @examples
#' zones$quadrant = c(1, 2, 1, 4, 5, 6, 7, 1)
#' aggzones <- rgeos::gUnaryUnion(zones, id = zones@data$quadrant)
#' aggzones <- sp::SpatialPolygonsDataFrame(aggzones, data.frame(region = c(1:6)), match.ID = FALSE)
#' sp::proj4string(aggzones) = sp::proj4string(zones)
#' aggzones_sf <- sf::st_as_sf(aggzones)
#' aggzones_sf <- sf::st_set_crs(aggzones_sf, sf::st_crs(zones_sf))
#' od_agg <- od_aggregate(flow, zones_sf, aggzones_sf)
#' colSums(od_agg[3:9]) == colSums(flow[3:9])
#' od_sf_agg <- od2line(od_agg, aggzones_sf)
#' plot(flowlines, lwd = flowlines$Bicycle)
#' plot(od_sf_agg$geometry, lwd = od_sf_agg$Bicycle, add = TRUE, col = "red")
od_aggregate <- function(flow, zones, aggzones,
                         aggzone_points = NULL,
                         cols = FALSE,
                         aggcols = FALSE,
                         FUN = sum,
                         prop_by_area = ifelse(identical(FUN, mean) == FALSE, TRUE, FALSE),
                         digits = getOption("digits")) {
  UseMethod("od_aggregate", zones)
}
#' @export
od_aggregate.sf <- function(flow, zones, aggzones,
                            aggzone_points = NULL,
                            cols = FALSE,
                            aggcols = FALSE,
                            FUN = sum,
                            prop_by_area = ifelse(identical(FUN, mean) == FALSE, TRUE, FALSE),
                            digits = getOption("digits")) {

  flow_first_col <- colnames(flow)[1]
  flow_second_col <- colnames(flow)[2]
  zonesfirstcol <- colnames(zones)[1]
  aggzonesfirstcol <- colnames(aggzones)[1]

  if (identical(cols, FALSE)) {
    col_ids <- sapply(flow, is.numeric)
    cols <- names(col_ids)[col_ids]
  }
  if (aggcols == FALSE) {
    aggcols <- colnames(aggzones)[1]
  }

  zone_points <- sf::st_centroid(zones)
  if(is.null(aggzone_points)) {
    aggzone_points <- sf::st_centroid(aggzones)
  }

  zones_agg <- zone_points %>%
    sf::st_join(y = aggzones[aggcols]) %>%
    sf::st_set_geometry(NULL)

  names(zones_agg)[1] <- flow_first_col
  zones_agg$new_orig = zones_agg[, aggcols[1]]
  zones_agg$new_dest = zones_agg[, aggcols[1]]

  flow_new_orig <- flow %>%
    dplyr::inner_join(y = zones_agg[c(flow_first_col, "new_orig")])

  names(zones_agg)[1] <- flow_second_col

  flow_new_dest <- flow_new_orig %>%
    dplyr::inner_join(y = zones_agg[c(flow_second_col, "new_dest")])

  flow_ag <- flow_new_dest %>%
    dplyr::group_by(!!rlang::sym("new_orig"), !!rlang::sym("new_dest")) %>%
    dplyr::summarise_at(.vars = cols, .funs = sum) %>%
    dplyr::ungroup()

  flow_ag

  # od2line(flow = flow_ag, zones = aggzones) # to export as sf

}
#' @export
od_aggregate.Spatial <- function(flow, zones, aggzones,
                                 aggzone_points = NULL,
                                 cols = FALSE,
                                 aggcols = FALSE,
                                 FUN = sum,
                                 prop_by_area = ifelse(identical(FUN, mean) == FALSE, TRUE, FALSE),
                                 digits = getOption("digits")) {
  zonesfirstcol <- colnames(zones@data)[1]
  aggzonesfirstcol <- colnames(aggzones@data)[1]

  if (cols == FALSE) {
    cols <- unlist(lapply(flow, is, 'numeric'))
    cols <- names(cols[which(cols == TRUE)])
  }
  if (aggcols == FALSE) {
    aggcols <- colnames(aggzones@data)[1]
  }

  origzones <- zones
  origaggzones <- aggzones

  if (sp::is.projected(zones) == TRUE & all.equal(zones@proj4string, aggzones@proj4string) == FALSE) {
    aggzones <- sp::spTransform(aggzones, zones@proj4string)
  } else {
    projection <- paste0("+proj=aea +lat_1=90 +lat_2=-18.416667 ",
                         "+lat_0=0 +lon_0=10 +x_0=0 +y_0=0 +ellps=GRS80",
                         " +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
    zones <- sp::spTransform(zones, projection)
    aggzones <- sp::spTransform(aggzones, projection)
  }

  zones@data$stplanr_area <- rgeos::gArea(zones, byid = TRUE)
  zones@data$od_aggregate_charid <- row.names(zones@data)
  aggzones@data$od_aggregate_charid <- row.names(aggzones@data)

  zoneintersect <- rgeos::gIntersection(zones, aggzones, byid = TRUE)
  zoneintersect <- sp::SpatialPolygonsDataFrame(zoneintersect,
                                                  data=data.frame(
                                                    od_aggregate_charid=sapply(zoneintersect@polygons, function(x) x@ID),
                                                    row.names=sapply(zoneintersect@polygons, function(x) x@ID)
                                                  ))
  zoneintersect@data$od_aggregate_interarea <- rgeos::gArea(zoneintersect, byid = TRUE)
  zoneintersect@data$od_aggregate_zone_charid <- stringr::str_split(zoneintersect@data$od_aggregate_charid, " ", simplify = TRUE)[,1]
  zoneintersect@data$od_aggregate_aggzone_charid <- stringr::str_split(zoneintersect@data$od_aggregate_charid, " ", simplify = TRUE)[,2]

  zoneintersect <- merge(zoneintersect, zones@data, by.x = 'od_aggregate_zone_charid', by.y = 'od_aggregate_charid')
  zoneintersect@data$od_aggregate_proparea <- zoneintersect@data$od_aggregate_interarea/zoneintersect@data$stplanr_area

  intersectdf <- merge(merge(
    flow,
    setNames(zoneintersect@data, paste0('o_', colnames(zoneintersect@data))),
    by.x=colnames(flow)[1],
    by.y=paste0('o_',zonesfirstcol)),
    setNames(zoneintersect@data, paste0('d_', colnames(zoneintersect@data))),
    by.x=colnames(flow)[2],
    by.y=paste0('d_',zonesfirstcol)
  )

  if (prop_by_area == TRUE & is(zones, "SpatialPolygonsDataFrame") == TRUE) {
    intersectdf <- intersectdf %>%
      dplyr::mutate_at(
        cols, dplyr::funs_('round(.*o_od_aggregate_proparea*d_od_aggregate_proparea)',args = list('digits'=digits))
      )
  }

  intersectdf <- intersectdf %>%
    dplyr::group_by_('o_od_aggregate_aggzone_charid', 'd_od_aggregate_aggzone_charid') %>%
    dplyr::select(dplyr::one_of(c('o_od_aggregate_aggzone_charid','d_od_aggregate_aggzone_charid',cols))) %>%
    dplyr::summarise_at(cols,.funs = FUN) %>%
    dplyr::left_join(setNames(aggzones@data[,c('od_aggregate_charid', aggcols)], c('od_aggregate_charid', paste0('o_',aggcols))),
              by = c('o_od_aggregate_aggzone_charid' = 'od_aggregate_charid')) %>%
    dplyr::left_join(setNames(aggzones@data[,c('od_aggregate_charid', aggcols)], c('od_aggregate_charid', paste0('d_',aggcols))),
              by = c('d_od_aggregate_aggzone_charid' = 'od_aggregate_charid'))
  intersectdf <- intersectdf[,c(
    paste0('o_', c(aggzonesfirstcol, aggcols[which(aggcols != aggzonesfirstcol)])),
    paste0('d_', c(aggzonesfirstcol, aggcols[which(aggcols != aggzonesfirstcol)])),
    cols
  )]

  return(as.data.frame(intersectdf))

}

#' Aggregate SpatialPolygonsDataFrame to new geometry.
#'
#' @section Details:
#' This function performs aggregation on a SpatialPolygonsDataFrame to a
#' different geometry specified by another SpatialPolygons object.
#'
#' @param zones A SpatialPolygonsDataFrame or SpatialPointsDataFrame
#' representing the original centroids or boundaries.
#' Note that in the case of a SpatialPointsDataFrame, the original value
#' will be allocated to the polygon in which the point is located rather than
#' being distributed by area.
#' @param aggzones A SpatialPolygonsDataFrame containing the new
#' boundaries to aggregate to.
#' @param cols A character vector containing the names of columns on which to
#' apply FUN. By default, all numeric columns are aggregated.
#' @param FUN Function to use on aggregation. Default is sum.
#' @param prop_by_area Boolean value indicating if the values should be
#' proportionally adjusted based on area. Default is TRUE unless FUN = mean.
#' @param digits The number of digits to use when proportionally adjusting
#' values based on area. Default is the value of getOption("digits").
#'
#' @return SpatialPolygonsDataFrame
#'
#' @export
#' @examples
#' \dontrun{
#' zones@data$region <- 1
#' zones@data[c(2, 5), c('region')] <- 2
#' aggzones <- sp::SpatialPolygonsDataFrame(rgeos::gUnaryUnion(
#'  zones,
#'  id = zones@data$region), data.frame(region=c(1, 2))
#' )
#' zones@data$region <- NULL
#' zones@data$exdata <- 5
#' library(sp)
#' sp_aggregate(zones, aggzones)
#' }
sp_aggregate <- function(zones, aggzones, cols = FALSE,
                         FUN = sum,
                         prop_by_area = ifelse(identical(FUN, mean) == FALSE, TRUE, FALSE),
                         digits = getOption("digits")){

  zonesfirstcol <- colnames(zones@data)[1]
  aggzonesfirstcol <- colnames(aggzones@data)[1]
  aggcols <- colnames(aggzones@data)

  if (cols == FALSE) {
    cols <- unlist(lapply(zones@data, is, 'numeric'))
    cols <- names(cols[which(cols == TRUE)])
    cols <- cols[which(cols != zonesfirstcol)]
  }

  origzones <- zones
  origaggzones <- aggzones

  if (sp::is.projected(zones) == TRUE & all.equal(zones@proj4string, aggzones@proj4string) == FALSE) {
    aggzones <- sp::spTransform(aggzones, zones@proj4string)
  } else {
    projection <- paste0("+proj=aea +lat_1=90 +lat_2=-18.416667 ",
                         "+lat_0=0 +lon_0=10 +x_0=0 +y_0=0 +ellps=GRS80",
                         " +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
    zones <- sp::spTransform(zones, projection)
    aggzones <- sp::spTransform(aggzones, projection)
  }

  zones@data$stplanr_area <- rgeos::gArea(zones, byid = TRUE)
  zones@data$od_aggregate_charid <- row.names(zones@data)
  aggzones@data$od_aggregate_charid <- row.names(aggzones@data)

  zoneintersect <- rgeos::gIntersection(zones, aggzones, byid = TRUE)
  zoneintersect <- sp::SpatialPolygonsDataFrame(zoneintersect,
                                                data=data.frame(
                                                  od_aggregate_charid=sapply(zoneintersect@polygons, function(x) x@ID),
                                                  row.names=sapply(zoneintersect@polygons, function(x) x@ID)
                                                ))
  zoneintersect@data$od_aggregate_interarea <- rgeos::gArea(zoneintersect, byid = TRUE)
  zoneintersect@data$od_aggregate_zone_charid <- stringr::str_split(zoneintersect@data$od_aggregate_charid, " ", simplify = TRUE)[,1]
  zoneintersect@data$od_aggregate_aggzone_charid <- stringr::str_split(zoneintersect@data$od_aggregate_charid, " ", simplify = TRUE)[,2]

  zoneintersect <- merge(zoneintersect, zones@data, by.x = 'od_aggregate_zone_charid', by.y = 'od_aggregate_charid')
  zoneintersect@data$od_aggregate_proparea <- zoneintersect@data$od_aggregate_interarea/zoneintersect@data$stplanr_area

  intersectdf <- zoneintersect@data

  if (prop_by_area == TRUE & is(zones, "SpatialPolygonsDataFrame") == TRUE) {
    intersectdf <- intersectdf %>%
      dplyr::mutate_at(
        cols, dplyr::funs_('round(.*od_aggregate_proparea)',args = list('digits'=digits))
      )
  }

  intersectdf <- intersectdf %>%
    dplyr::group_by_('od_aggregate_aggzone_charid') %>%
    dplyr::select(dplyr::one_of(c('od_aggregate_aggzone_charid',cols))) %>%
    dplyr::summarise_at(cols,.funs = FUN) %>%
    dplyr::left_join(setNames(aggzones@data[,c('od_aggregate_charid', aggcols)],c('od_aggregate_aggzone_charid', aggcols)),
                     by = 'od_aggregate_aggzone_charid')
  intersectdf <- as.data.frame(intersectdf,
                               intersectdf$od_aggregate_aggzone_charid)
  intersectdf <- intersectdf[,c(
    c(aggzonesfirstcol, aggcols[which(aggcols != aggzonesfirstcol)]),
    cols
  )]

  aggzones <- origaggzones
  aggzones@data <- intersectdf

  return(aggzones)

}
