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
#' @param aggzones A SpatialPolygonsDataFrame containing the new (larger)
#' boundaries to aggregate to.
#' @param cols A character vector containing the names of columns on which to
#' apply \code{\link{FUN}}. By default, all numeric columns are aggregated.
#' @param aggcols A character vector containing the names of columns in
#' \code{\link{aggzones}} to retain in the aggregated data.frame. By default,
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
#' zones@data[c(2,5),c('region')] <- 2
#' aggzones <- SpatialPolygonsDataFrame(rgeos::gUnaryUnion(
#'  zones,
#'  id = zones@data$region),data.frame(region=c(1,2))
#' )
#' zones@data$region <- NULL
#' #od2odf(flow, zones)
od_aggregate <- function(flow, zones, aggzones, cols = FALSE, aggcols = FALSE,
                         FUN = sum,
                         prop_by_area = ifelse(identical(FUN, mean) == FALSE, TRUE, FALSE),
                         digits = getOption("digits")){

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

  zonesfirstcol <- colnames(zones@data)[1]
  aggzonesfirstcol <- colnames(aggzones@data)[1]

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
  zoneintersect@data$od_aggregate_zone_charid <- stringr::str_split(zoneintersect@data$od_aggregate_charid, " ", 2, simplify = TRUE)[,1]
  zoneintersect@data$od_aggregate_aggzone_charid <- stringr::str_split(zoneintersect@data$od_aggregate_charid, " ", 2, simplify = TRUE)[,2]

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

  if (prop_by_area == TRUE) {
    intersectdf <- intersectdf %>%
      dplyr::mutate_at(
        cols, dplyr::funs(round(. * o_od_aggregate_proparea * d_od_aggregate_proparea, digits))
      )
  }

  intersectdf <- intersectdf %>%
    dplyr::group_by(o_od_aggregate_aggzone_charid, d_od_aggregate_aggzone_charid) %>%
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