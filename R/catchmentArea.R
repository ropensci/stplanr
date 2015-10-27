#' Calculate catchment area and associated summary statistics.
#'
#' @section Details:
#' Calculates the catchment area of a facility (e.g., cycle path) using
#' straight-line distance as well as summary statistics from variables
#' available in a SpatialPolygonsDataFrame with census tracts or other
#' zones. Assumes that the frequency of the variable is evenly distributed
#' throughout the zone. Returns a SpatialPolygonsDataFrame.
#'
#' @param polygonlayer A SpatialPolygonsDataFrame containing zones from which
#' the summary statistics for the catchment variable will be calculated.
#' Smaller polygons will increase the accuracy of the results.
#' @param targetlayer A SpatialPolygonsDataFrame, SpatialLinesDataFrame,
#' SpatialPointsDataFrame, SpatialPolygons, SpatialLines or SpatialPoints
#' object containing the specifications of the facility for which the
#' catchment area is being calculated. If the object contains more than one
#' facility (e.g., multiple cycle paths) the aggregate catchment area will be
#' calculated.
#' @param calccols A vector of column names containing the variables in the
#' polygonlayer to be used in the calculation of the summary statistics for
#' the catchment area. If dissolve = FALSE, all other variables in the
#' original SpatialPolygonsDataFrame for zones that fall partly or entirely
#' within the catchment area will be included in the returned
#' SpatialPolygonsDataFrame but will not be adjusted for the proportion within
#' the catchment area.
#' @param distance Defines the size of the catchment area as the distance
#' around the targetlayer in the units of the projection
#' (default = 500 metres)
#' @param projection The proj4string used to define the projection to be used
#' for calculating the catchment areas or a character string 'austalbers' to
#' use the Australian Albers Equal Area projection. Ignored if the polygonlayer
#' is projected in which case the targetlayer will be converted to the
#' projection used by the polygonlayer. In all cases the resulting object will
#' be reprojected to the original coordinate system and projection of the
#' polygon layer. Default is an Albers Equal Area projection but for more
#' reliable results should use a local projection (e.g., Australian Albers
#' Equal Area project).
#' @param retainAreaProportion Boolean value. If TRUE retains a variable in
#' the resulting SpatialPolygonsDataFrame containing the proportion of the
#' original area within the catchment area (Default = FALSE).
#' @param dissolve Boolean value. If TRUE collapses the underlying zones
#' within the catchment area into a single region with statistics for the
#' whole catchment area.
#' @export
#' @examples \dontrun{
#' data_dir <- system.file("extdata", package = "stplanr")
#' unzip(file.path(data_dir, 'smallsa1.zip'))
#' unzip(file.path(data_dir, 'testcycleway.zip'))
#' sa1income <- readOGR(".","smallsa1")
#' testcycleway <- readOGR(".","testcycleway")
#' calc_catchment(
#'    polygonlayer = sa1income,
#'    targetlayer = testcycleway,
#'    calccols = c('Total'),
#'    distance = 800,
#'    projection = 'austalbers',
#'    dissolve = TRUE
#' )
#' }
calc_catchment <- function(
  polygonlayer,
  targetlayer,
  calccols,
  distance = 500,
  projection = paste0("+proj=aea +lat_1=90 +lat_2=-18.416667 ",
                      "+lat_0=0 +lon_0=10 +x_0=0 +y_0=0 +ellps=GRS80",
                      " +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"),
  retainAreaProportion = FALSE,
  dissolve = FALSE
  ){

  if (projection != "skipproj") {
    confproj <- checkprojs(polygonlayer = polygonlayer, targetlayer = targetlayer, projection = projection)
    polygonlayer <- confproj[["polygonlayer"]]
    targetlayer <- confproj[["targetlayer"]]
    origprojpolygon <- confproj[["origprojpolygon"]]
  }

  polygonlayer@data$calc_catchment_fullArea <- rgeos::gArea(polygonlayer, byid=TRUE)

  targetbuffer <- rgeos::gBuffer(targetlayer, width=distance)
  polygonlayer@data$calc_catchment_charid <- paste(row.names(polygonlayer@data),targetbuffer@polygons[[1]]@ID)

  targetintersect <- rgeos::gIntersection(polygonlayer,targetbuffer,byid=TRUE)
  targetintersect <- sp::SpatialPolygonsDataFrame(targetintersect,
                                                  data=data.frame(
                                                    calc_catchment_charid=sapply(targetintersect@polygons, function(x) x@ID),
                                                    row.names=sapply(targetintersect@polygons, function(x) x@ID)
                                                  ))
  targetintersect@data$calc_catchment_sectArea <- rgeos::gArea(targetintersect,byid=TRUE)
  targetintersect@data <- merge(targetintersect@data,polygonlayer@data,by='calc_catchment_charid',all.x=TRUE)
  targetintersect@data$calc_catchment_propArea <- targetintersect@data$calc_catchment_sectArea/targetintersect@data$calc_catchment_fullArea
  targetintersect@data[,calccols] <- targetintersect@data[,calccols]*targetintersect@data$calc_catchment_propArea


  if(dissolve == TRUE) {
    targetintersectd <- rgeos::gUnaryUnion(targetintersect)
    targetintersectd <- sp::SpatialPolygonsDataFrame(targetintersectd,
                                                     data=if(length(calccols) == 1){
                                                       setNames(
                                                         data.frame(x=sum(targetintersect@data[,calccols])),
                                                         calccols
                                                       )}else{
                                                         data.frame(as.list(colSums(targetintersect@data[,calccols])))
                                                       }
    )
    targetintersectd@data$calc_catchment_fullArea <- sum(targetintersect@data$calc_catchment_fullArea)
    targetintersectd@data$calc_catchment_sectArea <- sum(targetintersect@data$calc_catchment_sectArea)
    targetintersectd@data$calc_catchment_propArea <- targetintersectd@data$calc_catchment_sectArea/targetintersectd@data$calc_catchment_fullArea
    targetintersectd@data$calc_catchment_charid <- 'charid'
    targetintersect <- targetintersectd
    rm(targetintersectd)
  }


  targetintersect@data$calc_catchment_fullArea <- NULL
  targetintersect@data$calc_catchment_sectArea <- NULL
  targetintersect@data$calc_catchment_charid <- NULL

  if (retainAreaProportion == FALSE) {
    targetintersect@data$calc_catchment_propArea <- NULL
  }

  row.names(targetintersect@data) <- 1:nrow(targetintersect@data)

  if (projection != "skipproj") {
    targetintersect <- sp::spTransform(targetintersect, sp::CRS(origprojpolygon))
  }
  return(targetintersect)

}

#' Calculate summary statistics for catchment area.
#'
#' @section Details:
#' Calculates the summary statistics for a catchment area of a facility
#' (e.g., cycle path) using straight-line distance from variables
#' available in a SpatialPolygonsDataFrame with census tracts or other
#' zones. Assumes that the frequency of the variable is evenly distributed
#' throughout the zone. Returns either a single value if calccols is of
#' length = 1, or a named vector otherwise.
#'
#' @param polygonlayer A SpatialPolygonsDataFrame containing zones from which
#' the summary statistics for the catchment variable will be calculated.
#' Smaller polygons will increase the accuracy of the results.
#' @param targetlayer A SpatialPolygonsDataFrame, SpatialLinesDataFrame,
#' SpatialPointsDataFrame, SpatialPolygons, SpatialLines or SpatialPoints
#' object containing the specifications of the facility for which the
#' catchment area is being calculated. If the object contains more than one
#' facility (e.g., multiple cycle paths) the aggregate catchment area will be
#' calculated.
#' @param calccols A vector of column names containing the variables in the
#' polygonlayer to be used in the calculation of the summary statistics for
#' the catchment area.
#' @param distance Defines the size of the catchment area as the distance
#' around the targetlayer in the units of the projection
#' (default = 500 metres)
#' @param projection The proj4string used to define the projection to be used
#' for calculating the catchment areas or a character string 'austalbers' to
#' use the Australian Albers Equal Area projection. Ignored if the polygonlayer
#' is projected in which case the targetlayer will be converted to the
#' projection used by the polygonlayer. In all cases the resulting object will
#' be reprojected to the original coordinate system and projection of the
#' polygon layer. Default is an Albers Equal Area projection but for more
#' reliable results should use a local projection (e.g., Australian Albers
#' Equal Area project).
#' @param retainAreaProportion Boolean value. If TRUE retains a variable in
#' the resulting SpatialPolygonsDataFrame containing the proportion of the
#' original area within the catchment area (Default = FALSE).
#' @export
#' @examples \dontrun{
#' data_dir <- system.file("extdata", package = "stplanr")
#' unzip(file.path(data_dir, 'smallsa1.zip'))
#' unzip(file.path(data_dir, 'testcycleway.zip'))
#' sa1income <- readOGR(".","smallsa1")
#' testcycleway <- readOGR(".","testcycleway")
#' calc_catchment_sum(
#'    polygonlayer = sa1income,
#'    targetlayer = testcycleway,
#'    calccols = c('Total'),
#'    distance = 800,
#'    projection = 'austalbers'
#' )
#'
#' calc_catchment_sum(
#' polygonlayer = sa1income,
#' targetlayer = testcycleway,
#' calccols = c('Total'),
#' distance = 800,
#' projection = 'austalbers'
#' )
#' }
calc_catchment_sum <- function(
  polygonlayer,
  targetlayer,
  calccols,
  distance = 500,
  projection = paste0("+proj=aea +lat_1=90 +lat_2=-18.416667",
                      " +lat_0=0 +lon_0=10 +x_0=0 +y_0=0",
                      " +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"),
  retainAreaProportion = FALSE
  ){
  if(length(calccols) == 1) {
    return(sum(calc_catchment(
      polygonlayer = polygonlayer,
      targetlayer = targetlayer,
      calccols = calccols,
      distance = distance,
      projection = projection,
      retainAreaProportion = retainAreaProportion,
      dissolve = FALSE
    )@data[,calccols]))
  } else {
    return(colSums(calc_catchment(
      polygonlayer = polygonlayer,
      targetlayer = targetlayer,
      calccols = calccols,
      distance = distance,
      projection = projection,
      retainAreaProportion = retainAreaProportion,
      dissolve = FALSE
    )@data[,calccols]))
  }
}

#' Calculate summary statistics for all features independently.
#'
#' @section Details:
#' Calculates the summary statistics for a catchment area of multiple
#' facilities or zones using straight-line distance from variables
#' available in a SpatialPolygonsDataFrame with census tracts or other
#' zones. Assumes that the frequency of the variable is evenly distributed
#' throughout the zone. Returns the original source dataframe with additional
#' columns with summary variables.
#'
#' @param polygonlayer A SpatialPolygonsDataFrame containing zones from which
#' the summary statistics for the catchment variable will be calculated.
#' Smaller polygons will increase the accuracy of the results.
#' @param targetlayer A SpatialPolygonsDataFrame, SpatialLinesDataFrame or
#' SpatialPointsDataFrame object containing the specifications of the
#' facilities and zones for which the catchment areas are being calculated.
#' @param calccols A vector of column names containing the variables in the
#' polygonlayer to be used in the calculation of the summary statistics for
#' the catchment areas.
#' @param distance Defines the size of the catchment areas as the distance
#' around the targetlayer in the units of the projection
#' (default = 500 metres)
#' @param projection The proj4string used to define the projection to be used
#' for calculating the catchment areas or a character string 'austalbers' to
#' use the Australian Albers Equal Area projection. Ignored if the polygonlayer
#' is projected in which case the targetlayer will be converted to the
#' projection used by the polygonlayer. In all cases the resulting object will
#' be reprojected to the original coordinate system and projection of the
#' polygon layer. Default is an Albers Equal Area projection but for more
#' reliable results should use a local projection (e.g., Australian Albers
#' Equal Area project).
#' @param retainAreaProportion Boolean value. If TRUE retains a variable in
#' the resulting SpatialPolygonsDataFrame containing the proportion of the
#' original area within the catchment area (Default = FALSE).
#' @export
#' @examples \dontrun{
#' data_dir <- system.file("extdata", package = "stplanr")
#' unzip(file.path(data_dir, 'smallsa1.zip'))
#' unzip(file.path(data_dir, 'testcycleway.zip'))
#' sa1income <- readOGR(".","smallsa1")
#' testcycleway <- readOGR(".","testcycleway")
#' calc_moving_catchment(
#'    polygonlayer = sa1income,
#'    targetlayer = testcycleway,
#'    calccols = c('Total'),
#'    distance = 800,
#'    projection = 'austalbers'
#' )
#' }
calc_moving_catchment <- function(
  polygonlayer,
  targetlayer,
  calccols,
  distance = 500,
  projection = 'worldalbers',
  retainAreaProportion = FALSE
){
  newcalccols <- paste0('sum_',calccols)

  confproj <- checkprojs(polygonlayer = polygonlayer, targetlayer = targetlayer, projection = projection)
  polygonlayer <- confproj[["polygonlayer"]]
  targetlayer <- confproj[["targetlayer"]]
  origprojpolygon <- confproj[["origprojpolygon"]]

  targetlayer@data[,newcalccols] <- NA

  count <- 1
  while (count <= nrow(targetlayer)) {
    targetlayer[count,newcalccols] <- setNames(
      calc_catchment_sum(
        polygonlayer = polygonlayer,
        targetlayer = targetlayer[count,],
        calccols = calccols,
        distance = distance,
        projection = "skipproj",
        retainAreaProportion = retainAreaProportion
      ),
      newcalccols
    )
    count <- count + 1
  }

  targetlayer <- sp::spTransform(targetlayer, sp::CRS(origprojpolygon))

  return(targetlayer)
}


checkprojs <- function(polygonlayer, targetlayer, projection) {
  # Define Named vector of known projection strings
  knownprojs <- c(
    'austalbers'='+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs',
    'worldalbers'='+proj=aea +lat_1=90 +lat_2=-18.416667 +lat_0=0 +lon_0=10 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'
  )

  if (sum(is.na(knownprojs[projection])) == 0) {
    projection <- knownprojs[projection]
  }

  polyproj <- sp::is.projected(polygonlayer)
  lineproj <- sp::is.projected(targetlayer)

  origprojpolygon <- sp::proj4string(polygonlayer)

  if (polyproj == FALSE & lineproj == FALSE) {

    polygonlayer <- sp::spTransform(polygonlayer, sp::CRS(projection))
    targetlayer <- sp::spTransform(targetlayer, sp::CRS(projection))
  } else if (polyproj == TRUE & lineproj == FALSE) {
    projection = sp::proj4string(polygonlayer)
    targetlayer <- sp::spTransform(targetlayer, sp::CRS(projection))
  } else if (polyproj == TRUE & lineproj == TRUE) {
    if (sp::proj4string(polyproj) != sp::proj4string(lineproj)) {
      projection = sp::proj4string(polygonlayer)
      targetlayer <- sp::spTransform(targetlayer, sp::CRS(projection))
    }
  }
  return(list("polygonlayer"=polygonlayer,"targetlayer"=targetlayer,"origprojpolygon"=origprojpolygon))
}