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
#' @param cols A character vector containing the names of columns on which to
#' apply FUN. By default, all numeric columns are aggregated.
#' @param aggcols A character vector containing the names of columns in
#' aggzones to retain in the aggregated data.frame. By default,
#' only the first column is retained. These columns are renamed with a prefix
#' of "o_" and "d_".
#' @param FUN Function to use on aggregation. Default is sum.
#' @param prop_by_area Boolean value indicating if the values should be
#' proportionally adjusted based on area. Default is TRUE unless FUN = mean.
#' @param digits The number of digits to use when proportionally adjusting
#' values based on area. Default is the value of getOption("digits").
#'
#' @return data.frame containing the aggregated od flows.
#'
#' @export
#' @examples
#' data(flow)
#' data(zones)
#' zones@data$region <- 1
#' zones@data[c(2, 5), c('region')] <- 2
#' aggzones <- SpatialPolygonsDataFrame(rgeos::gUnaryUnion(
#'  zones,
#'  id = zones@data$region), data.frame(region=c(1, 2))
#' )
#' zones@data$region <- NULL
#' od_aggregate(flow, zones, aggzones)
#' # another example with more zones and plots
#' zones$quadrant = quadrant(zones, number_out = TRUE)
#' aggzones <- SpatialPolygonsDataFrame(
#' rgeos::gUnaryUnion(
#'  zones,
#'  id = zones@data$quadrant), data.frame(region = c(1:4))
#' )
#' od = od_aggregate(flow, zones, aggzones)
#' od_sp = od2line(flow, zones)
#' zones@data = cbind(1:nrow(zones), zones@data)
#' od_sp_agg = od2line(od, zones, aggzones)
#' # plot results
#' plot(aggzones, lwd = 5)
#' plot(zones, border = "red", add = TRUE)
#' plot(od_sp, add = TRUE, col = "yellow")
#' lwd = od_sp_agg$All / 50
#' plot(od_sp_agg, lwd = lwd, add = TRUE)
od_aggregate <- function(flow, zones, aggzones, cols = FALSE, aggcols = FALSE,
                         FUN = sum,
                         prop_by_area = ifelse(identical(FUN, mean) == FALSE, TRUE, FALSE),
                         digits = getOption("digits")){

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
#' data(flow)
#' data(zones)
#' zones@data$region <- 1
#' zones@data[c(2, 5), c('region')] <- 2
#' aggzones <- SpatialPolygonsDataFrame(rgeos::gUnaryUnion(
#'  zones,
#'  id = zones@data$region), data.frame(region=c(1, 2))
#' )
#' zones@data$region <- NULL
#' zones@data$exdata <- 5
#' sp_aggregate(zones, aggzones)
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