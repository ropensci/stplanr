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
#' @param quadsegs Number of line segments to use to approximate a quarter
#' circle. Parameter passed to buffer functions, default is 5 for sp and
#' 30 for sf.
#' @family rnet
#' @export
#' @examples
#' \dontrun{
#' data_dir <- system.file("extdata", package = "stplanr")
#' unzip(file.path(data_dir, "smallsa1.zip"))
#' unzip(file.path(data_dir, "testcycleway.zip"))
#' sa1income <- as(sf::read_sf("smallsa1.shp"), "Spatial")
#' testcycleway <- as(sf::read_sf("testcycleway.shp"), "Spatial")
#' cway_catch <- calc_catchment(
#'   polygonlayer = sa1income,
#'   targetlayer = testcycleway,
#'   calccols = c("Total"),
#'   distance = 800,
#'   projection = "austalbers",
#'   dissolve = TRUE
#' )
#' plot(sa1income)
#' plot(cway_catch, add = TRUE, col = "green")
#' plot(testcycleway, col = "red", add = TRUE)
#' sa1income <- sf::read_sf("smallsa1.shp")
#' testcycleway <- sf::read_sf("testcycleway.shp")
#' f <- list.files(".", "testcycleway|smallsa1")
#' file.remove(f)
#' cway_catch <- calc_catchment(
#'   polygonlayer = sa1income,
#'   targetlayer = testcycleway,
#'   calccols = c("Total"),
#'   distance = 800,
#'   projection = "austalbers",
#'   dissolve = TRUE
#' )
#' plot(sa1income$geometry)
#' plot(testcycleway$geometry, col = "red", add = TRUE)
#' plot(cway_catch["Total"], add = TRUE)
#' }
calc_catchment <- function(
                           polygonlayer,
                           targetlayer,
                           calccols,
                           distance = 500,
                           projection = paste0(
                             "+proj=aea +lat_1=90 +lat_2=-18.416667 ",
                             "+lat_0=0 +lon_0=10 +x_0=0 +y_0=0 +ellps=GRS80",
                             " +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
                           ),
                           retainAreaProportion = FALSE,
                           dissolve = FALSE,
                           quadsegs = NULL) {
  UseMethod(generic = "calc_catchment")
}
#' @export
calc_catchment.Spatial <- function(
                                   polygonlayer,
                                   targetlayer,
                                   calccols,
                                   distance = 500,
                                   projection = paste0(
                                     "+proj=aea +lat_1=90 +lat_2=-18.416667 ",
                                     "+lat_0=0 +lon_0=10 +x_0=0 +y_0=0 +ellps=GRS80",
                                     " +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
                                   ),
                                   retainAreaProportion = FALSE,
                                   dissolve = FALSE,
                                   quadsegs = 5) {
  if (projection != "skipproj") {
    confproj <- checkprojs.Spatial(polygonlayer = polygonlayer, targetlayer = targetlayer, projection = projection)
    polygonlayer <- confproj[["polygonlayer"]]
    targetlayer <- confproj[["targetlayer"]]
    origprojpolygon <- confproj[["origprojpolygon"]]
  }

  polygonlayer@data$calc_catchment_fullArea <-
    rgeos::gArea(polygonlayer, byid = TRUE)

  targetbuffer <- rgeos::gBuffer(targetlayer, width = distance, quadsegs = quadsegs, byid = TRUE)
  polygonlayer@data$calc_catchment_charid <- paste(row.names(polygonlayer@data), targetbuffer@polygons[[1]]@ID)
  polygonlayer@data$calc_catchment_charid2 <- as.character(row.names(polygonlayer@data))
  targetlayer@data$calc_catchment_charid <- as.character(row.names(targetlayer@data))

  targetintersect <-
    rgeos::gIntersection(polygonlayer, targetbuffer, byid = TRUE)
  targetintersect <- sp::SpatialPolygonsDataFrame(targetintersect,
    data = data.frame(
      calc_catchment_charid = sapply(targetintersect@polygons, function(x) {
        x@ID
      }),
      row.names = sapply(targetintersect@polygons, function(x) {
        x@ID
      })
    )
  )

  targetintersect@data$calc_catchment_sectArea <- rgeos::gArea(targetintersect, byid = TRUE)
  targetintersect@data <- cbind(
    targetintersect@data,
    setNames(
      as.data.frame(
        t(as.data.frame(
          strsplit(
            as.character(targetintersect@data$calc_catchment_charid),
            split = " "
          )
        ))
      ),
      c("calc_catchment_polygonid", "calc_catchment_targetid")
    )
  )
  targetintersect@data$calc_catchment_polygonid <- as.character(targetintersect@data$calc_catchment_polygonid)
  targetintersect@data$calc_catchment_targetid <- as.character(targetintersect@data$calc_catchment_targetid)
  targetintersect@data <- merge(targetintersect@data, polygonlayer@data,
    by.x = "calc_catchment_polygonid",
    by.y = "calc_catchment_charid2", all.x = TRUE
  )
  targetintersect@data <- merge(targetintersect@data, targetlayer@data,
    by.x = "calc_catchment_targetid",
    by.y = "calc_catchment_charid", all.x = TRUE
  )
  targetintersect@data$calc_catchment_propArea <- targetintersect@data$calc_catchment_sectArea / targetintersect@data$calc_catchment_fullArea
  targetintersect@data[, calccols] <- targetintersect@data[, calccols] * targetintersect@data$calc_catchment_propArea


  if (dissolve == TRUE) {
    targetintersectd <- rgeos::gUnaryUnion(targetintersect, id = targetintersect$calc_catchment_targetid)
    targetcols <- colnames(targetlayer@data)
    targetcols <- targetcols[which(targetcols != "calc_catchment_charid")]
    targetintersect@data[targetcols] <- lapply(targetcols, function(x) {
      as.character(targetintersect@data[[x]])
    })
    targetintersectd_data <- as.data.frame(
      targetintersect@data %>%
        dplyr::group_by_at(c("calc_catchment_targetid", targetcols)) %>%
        dplyr::summarise_at(
          dplyr::vars(
            c(calccols, "calc_catchment_fullArea", "calc_catchment_sectArea")
          ),
          dplyr::funs("sum", .args = list("na.rm" = TRUE))
        )
    )
    rownames(targetintersectd_data) <- as.character(unlist(lapply(targetintersectd@polygons, function(x) {
      x@ID
    })))
    targetintersectd <- sp::SpatialPolygonsDataFrame(targetintersectd,
      data = targetintersectd_data
    )
    rm(targetintersectd_data)
    targetintersectd@data$calc_catchment_propArea <- targetintersectd@data$calc_catchment_sectArea / targetintersectd@data$calc_catchment_fullArea

    targetintersectd@data$calc_catchment_charid <- "charid"
    targetintersect <- targetintersectd
    rm(targetintersectd)
  }


  targetintersect@data$calc_catchment_fullArea <- NULL
  targetintersect@data$calc_catchment_sectArea <- NULL
  targetintersect@data$calc_catchment_charid <- NULL
  targetintersect@data$calc_catchment_polygonid <- NULL
  targetintersect@data$calc_catchment_targetid <- NULL
  targetintersect@data$calc_catchment_charid.x <- NULL
  targetintersect@data$calc_catchment_charid.y <- NULL

  if (retainAreaProportion == FALSE) {
    targetintersect@data$calc_catchment_propArea <- NULL
  }

  row.names(targetintersect@data) <- 1:nrow(targetintersect@data)

  if (projection != "skipproj") {
    targetintersect <-
      sp::spTransform(targetintersect, sp::CRS(origprojpolygon))
  }
  return(targetintersect)
}
#' @export
calc_catchment.sf <- function(
                              polygonlayer,
                              targetlayer,
                              calccols,
                              distance = 500,
                              projection = paste0(
                                "+proj=aea +lat_1=90 +lat_2=-18.416667 ",
                                "+lat_0=0 +lon_0=10 +x_0=0 +y_0=0 +ellps=GRS80",
                                " +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
                              ),
                              retainAreaProportion = FALSE,
                              dissolve = FALSE,
                              quadsegs = 30) {
  if (projection != "skipproj") {
    confproj <- checkprojs.sf(polygonlayer = polygonlayer, targetlayer = targetlayer, projection = projection)
    polygonlayer <- confproj[["polygonlayer"]]
    targetlayer <- confproj[["targetlayer"]]
    origprojpolygon <- confproj[["origprojpolygon"]]
  }

  polygonlayer$calc_catchment_fullArea <- as.numeric(sf::st_area(polygonlayer))

  targetbuffer <- sf::st_buffer(targetlayer, distance, nQuadSegs = quadsegs)
  polygonlayer$calc_catchment_charid <- paste(row.names(polygonlayer), row.names(targetbuffer))

  targetintersect <- sf::st_intersection(polygonlayer, targetbuffer)


  targetintersect$calc_catchment_sectArea <- as.numeric(sf::st_area(targetintersect))
  targetintersect$calc_catchment_propArea <- targetintersect$calc_catchment_sectArea / targetintersect$calc_catchment_fullArea

  targetintersect <- dplyr::mutate_at(targetintersect, dplyr::vars(calccols), dplyr::funs(.data$. * .data$calc_catchment_propArea))

  if (dissolve == TRUE) {
    targetcols <- colnames(targetlayer)
    targetcols <- targetcols[which(targetcols != "geometry")]
    targetintersect <- targetintersect %>%
      dplyr::group_by_at(targetcols) %>%
      dplyr::summarise_at(dplyr::vars(calccols), .funs = "sum", na.rm = TRUE)
  }


  targetintersect$calc_catchment_fullArea <- NULL
  targetintersect$calc_catchment_sectArea <- NULL
  targetintersect$calc_catchment_charid <- NULL

  if (retainAreaProportion == FALSE) {
    targetintersect$calc_catchment_propArea <- NULL
  }

  row.names(targetintersect) <- 1:nrow(targetintersect)

  if (projection != "skipproj") {
    targetintersect <- sf::st_transform(targetintersect, origprojpolygon)
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
#' @param quadsegs Number of line segments to use to approximate a quarter
#' circle. Parameter passed to buffer functions, default is 5 for sp and
#' 30 for sf.
#' @family rnet
#' @export
#' @examples
#' \dontrun{
#' data_dir <- system.file("extdata", package = "stplanr")
#' unzip(file.path(data_dir, "smallsa1.zip"))
#' unzip(file.path(data_dir, "testcycleway.zip"))
#' sa1income <- readOGR(".", "smallsa1")
#' testcycleway <- readOGR(".", "testcycleway")
#' calc_catchment_sum(
#'   polygonlayer = sa1income,
#'   targetlayer = testcycleway,
#'   calccols = c("Total"),
#'   distance = 800,
#'   projection = "austalbers"
#' )
#'
#' calc_catchment_sum(
#'   polygonlayer = sa1income,
#'   targetlayer = testcycleway,
#'   calccols = c("Total"),
#'   distance = 800,
#'   projection = "austalbers"
#' )
#' }
calc_catchment_sum <- function(
                               polygonlayer,
                               targetlayer,
                               calccols,
                               distance = 500,
                               projection = paste0(
                                 "+proj=aea +lat_1=90 +lat_2=-18.416667",
                                 " +lat_0=0 +lon_0=10 +x_0=0 +y_0=0",
                                 " +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
                               ),
                               retainAreaProportion = FALSE,
                               quadsegs = NA) {
  UseMethod(generic = "calc_catchment_sum")
}
#' @export
calc_catchment_sum.Spatial <- function(
                                       polygonlayer,
                                       targetlayer,
                                       calccols,
                                       distance = 500,
                                       projection = paste0(
                                         "+proj=aea +lat_1=90 +lat_2=-18.416667",
                                         " +lat_0=0 +lon_0=10 +x_0=0 +y_0=0",
                                         " +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
                                       ),
                                       retainAreaProportion = FALSE,
                                       quadsegs = 5) {
  if (length(calccols) == 1) {
    return(sum(calc_catchment(
      polygonlayer = polygonlayer,
      targetlayer = targetlayer,
      calccols = calccols,
      distance = distance,
      projection = projection,
      retainAreaProportion = retainAreaProportion,
      dissolve = FALSE,
      quadsegs = quadsegs
    )@data[, calccols], na.rm = TRUE))
  } else {
    return(colSums(calc_catchment(
      polygonlayer = polygonlayer,
      targetlayer = targetlayer,
      calccols = calccols,
      distance = distance,
      projection = projection,
      retainAreaProportion = retainAreaProportion,
      dissolve = FALSE,
      quadsegs = quadsegs
    )@data[, calccols], na.rm = TRUE))
  }
}

#' @export
calc_catchment_sum.sf <- function(
                                  polygonlayer,
                                  targetlayer,
                                  calccols,
                                  distance = 500,
                                  projection = paste0(
                                    "+proj=aea +lat_1=90 +lat_2=-18.416667",
                                    " +lat_0=0 +lon_0=10 +x_0=0 +y_0=0",
                                    " +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
                                  ),
                                  retainAreaProportion = FALSE,
                                  quadsegs = 30) {
  if (length(calccols) == 1) {
    return(sum(as.data.frame(calc_catchment(
      polygonlayer = polygonlayer,
      targetlayer = targetlayer,
      calccols = calccols,
      distance = distance,
      projection = projection,
      retainAreaProportion = retainAreaProportion,
      dissolve = FALSE,
      quadsegs = quadsegs
    ))[, calccols], na.rm = TRUE))
  } else {
    return(colSums(as.data.frame(calc_catchment(
      polygonlayer = polygonlayer,
      targetlayer = targetlayer,
      calccols = calccols,
      distance = distance,
      projection = projection,
      retainAreaProportion = retainAreaProportion,
      dissolve = FALSE,
      quadsegs = quadsegs
    ))[, calccols], na.rm = TRUE))
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
#' @family rnet
#' @export
#' @examples
#' \dontrun{
#' data_dir <- system.file("extdata", package = "stplanr")
#' unzip(file.path(data_dir, "smallsa1.zip"))
#' unzip(file.path(data_dir, "testcycleway.zip"))
#' sa1income <- readOGR(".", "smallsa1")
#' testcycleway <- readOGR(".", "testcycleway")
#' calc_moving_catchment(
#'   polygonlayer = sa1income,
#'   targetlayer = testcycleway,
#'   calccols = c("Total"),
#'   distance = 800,
#'   projection = "austalbers"
#' )
#' }
calc_moving_catchment <- function(
                                  polygonlayer,
                                  targetlayer,
                                  calccols,
                                  distance = 500,
                                  projection = "worldalbers",
                                  retainAreaProportion = FALSE) {
  newcalccols <- paste0("sum_", calccols)

  confproj <- checkprojs.Spatial(polygonlayer = polygonlayer, targetlayer = targetlayer, projection = projection)

  polygonlayer <- confproj[["polygonlayer"]]
  targetlayer <- confproj[["targetlayer"]]
  origprojpolygon <- confproj[["origprojpolygon"]]

  targetlayer@data[, newcalccols] <- NA

  p <- dplyr::progress_estimated(nrow(targetlayer), min_time = 10)
  count <- 1
  while (count <= nrow(targetlayer)) {
    targetlayer[count, newcalccols] <- setNames(
      calc_catchment_sum(
        polygonlayer = polygonlayer,
        targetlayer = targetlayer[count, ],
        calccols = calccols,
        distance = distance,
        projection = "skipproj",
        retainAreaProportion = retainAreaProportion
      ),
      newcalccols
    )
    p$tick()$print()
    count <- count + 1
  }

  targetlayer <-
    sp::spTransform(targetlayer, sp::CRS(origprojpolygon))

  return(targetlayer)
}

#' Calculate catchment area and associated summary statistics using network.
#'
#' @section Details:
#' Calculates the catchment area of a facility (e.g., cycle path) using
#' network distance (or other weight variable) as well as summary statistics
#' from variables available in a SpatialPolygonsDataFrame with census tracts
#' or other zones. Assumes that the frequency of the variable is evenly
#' distributed throughout the zone. Returns a SpatialPolygonsDataFrame.
#'
#' @param sln The SpatialLinesNetwork to use.
#' @param polygonlayer A SpatialPolygonsDataFrame containing zones from which
#' the summary statistics for the catchment variable will be calculated.
#' Smaller polygons will increase the accuracy of the results.
#' @param targetlayer A SpatialPolygonsDataFrame, SpatialLinesDataFrame or
#' SpatialPointsDataFrame object containing the specifications of the
#' facilities and zones for which the catchment areas are being calculated.
#' @param calccols A vector of column names containing the variables in the
#' polygonlayer to be used in the calculation of the summary statistics for
#' the catchment area. If dissolve = FALSE, all other variables in the
#' original SpatialPolygonsDataFrame for zones that fall partly or entirely
#' within the catchment area will be included in the returned
#' SpatialPolygonsDataFrame but will not be adjusted for the proportion within
#' the catchment area.
#' @param maximpedance The maximum value of the network's weight attribute in
#' the units of the weight (default = 1000).
#' @param distance Defines the additional catchment area around the network
#' in the units of the projection.
#' (default = 100 metres)
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
#' @family rnet
#' @export
#' @examples
#' \dontrun{
#' data_dir <- system.file("extdata", package = "stplanr")
#' unzip(file.path(data_dir, "smallsa1.zip"), exdir = tempdir())
#' unzip(file.path(data_dir, "testcycleway.zip"), exdir = tempdir())
#' unzip(file.path(data_dir, "sydroads.zip"), exdir = tempdir())
#' sa1income <- readOGR(tempdir(), "smallsa1")
#' testcycleway <- readOGR(tempdir(), "testcycleway")
#' sydroads <- readOGR(tempdir(), "roads")
#' sydnetwork <- SpatialLinesNetwork(sydroads)
#' calc_network_catchment(
#'   sln = sydnetwork,
#'   polygonlayer = sa1income,
#'   targetlayer = testcycleway,
#'   calccols = c("Total"),
#'   maximpedance = 800,
#'   distance = 200,
#'   projection = "austalbers",
#'   dissolve = TRUE
#' )
#' }
calc_network_catchment <- function(sln,
                                   polygonlayer,
                                   targetlayer,
                                   calccols,
                                   maximpedance = 1000,
                                   distance = 100,
                                   projection = paste0(
                                     "+proj=aea +lat_1=90 +lat_2=-18.416667",
                                     " +lat_0=0 +lon_0=10 +x_0=0 +y_0=0",
                                     " +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
                                   ),
                                   retainAreaProportion = FALSE,
                                   dissolve = FALSE) {
  longlat <- ifelse(sp::is.projected(sln@sl) == TRUE, FALSE, TRUE)
  maximpedance <-
    ifelse(longlat == TRUE, maximpedance / 1000, maximpedance)

  if (is(targetlayer, "SpatialLines") |
    is(targetlayer, "SpatialLinesDataFrame") |
    is(targetlayer, "SpatialPolygons") |
    is(targetlayer, "SpatialPolygonsDataFrame") |
    is(targetlayer, "SpatialPoints") |
    is(targetlayer, "SpatialPointsDataFrame")) {
    if (sln@sl@proj4string@projargs != targetlayer@proj4string@projargs) {
      newtargetlayer <- sp::spTransform(targetlayer, sln@sl@proj4string)
    }
    else {
      newtargetlayer <- targetlayer
    }
    # targetnodes <- unique(find_network_nodes(sln, as.data.frame(coordinates(newtargetlayer))))
    if (is(targetlayer, "SpatialPoints") |
      is(targetlayer, "SpatialPointsDataFrame")) {
      targetnodes <- unique(find_network_nodes(sln, as.data.frame(unique(
        coordinates(newtargetlayer)
      ))))
    } else {
      targetnodes <- unique(find_network_nodes(sln, as.data.frame(unique(
        do.call(rbind, unlist(
          coordinates(newtargetlayer),
          recursive = FALSE
        ))
      ))))
    }
    spaths <- lapply(targetnodes, function(x) {
      igraph::get.shortest.paths(sln@g, x,
        # which(sp::spDists(
        #   x = as.matrix(data.frame(x=sln@g$x, y=sln@g$y)),
        #   y = matrix(cbind(sln@g$x,sln@g$y)[x,],ncol=2),
        #   longlat = longlat
        # ) <= maximpedance),
        output = "epath"
      )
    })

    spaths <-
      unlist(lapply(spaths, function(x) {
        x$epath
      }), recursive = FALSE)
  }

  else {
    spaths <- igraph::get.shortest.paths(sln@g,
      targetlayer,
      which(
        sp::spDists(
          x = as.matrix(data.frame(x = sln@g$x, y = sln@g$y)),
          y = matrix(cbind(sln@g$x, sln@g$y)[targetlayer, ],
            ncol =
              2
          ),
          longlat = longlat
        ) <= maximpedance
      ),
      output = "epath"
    )
    spaths <- spaths$epath
  }

  uniquesects <- unique(unlist(lapply(
    spaths,
    function(x) {
      if (length(x) > 0) {
        if (sum(sln@sl@data[x, sln@weightfield], na.rm = TRUE) <= maximpedance) {
          x
        }
      }
    }
  )))

  calc_catchment(
    polygonlayer = polygonlayer,
    targetlayer = sln@sl[uniquesects, ],
    calccols = calccols,
    distance = distance,
    projection = projection,
    retainAreaProportion = retainAreaProportion,
    dissolve = dissolve
  )
}

checkprojs.Spatial <- function(polygonlayer, targetlayer, projection) {
  # Define Named vector of known projection strings
  knownprojs <- c(
    "austalbers" = "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
    "worldalbers" = "+proj=aea +lat_1=90 +lat_2=-18.416667 +lat_0=0 +lon_0=10 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
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
    projection <- sp::proj4string(polygonlayer)
    targetlayer <- sp::spTransform(targetlayer, sp::CRS(projection))
  } else if (polyproj == TRUE & lineproj == TRUE) {
    if (sp::proj4string(polygonlayer) != sp::proj4string(targetlayer)) {
      projection <- sp::proj4string(polygonlayer)
      targetlayer <-
        sp::spTransform(targetlayer, sp::CRS(projection))
    }
  }
  return(list("polygonlayer" = polygonlayer, "targetlayer" = targetlayer, "origprojpolygon" = origprojpolygon))
}

checkprojs.sf <- function(polygonlayer, targetlayer, projection) {
  # Define Named vector of known projection strings
  knownprojs <- c(
    "austalbers" = "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
    "worldalbers" = "+proj=aea +lat_1=90 +lat_2=-18.416667 +lat_0=0 +lon_0=10 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  )

  if (sum(is.na(knownprojs[projection])) == 0) {
    projection <- knownprojs[projection]
  }

  polyproj <- !sf::st_is_longlat(polygonlayer)
  lineproj <- !sf::st_is_longlat(targetlayer)

  origprojpolygon <- sf::st_crs(polygonlayer)

  if (polyproj == FALSE & lineproj == FALSE) {
    polygonlayer <- sf::st_transform(polygonlayer, projection)
    targetlayer <- sf::st_transform(targetlayer, projection)
  } else if (polyproj == TRUE & lineproj == FALSE) {
    projection <- sf::st_crs(polygonlayer)$proj4string
    targetlayer <- sf::st_transform(targetlayer, projection)
  } else if (polyproj == TRUE & lineproj == TRUE) {
    if (sf::st_crs(polygonlayer)$proj4string != sf::st_crs(targetlayer)$proj4string) {
      projection <- sf::st_crs(polygonlayer)$proj4string
      targetlayer <- sf::st_transform(targetlayer, projection)
    }
  }
  return(list("polygonlayer" = polygonlayer, "targetlayer" = targetlayer, "origprojpolygon" = origprojpolygon))
}
