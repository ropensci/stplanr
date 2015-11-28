#' Import GTFS shapes and route data to SpatialLinesDataFrame.
#'
#' Takes a string with the file path of the zip file with the GTFS feed,
#' imports the shapes (geometry), route and agency data and returns a
#' SpatialLinesDataFrame for the GTFS feed.
#'
#' @param gtfszip String with the file path of the GTFS feed zip file
#' @export
gtfs2sldf <- function(gtfszip = "") {

  if (gtfszip == "") {
    stop("Zip file required")
  }
  if (file.exists(gtfszip) == FALSE) {
    stop("Specified zip file does not exist")
  }

  `%>%` <- magrittr::`%>%`
  gtfsfiles <- unzip(gtfszip, exdir = tempdir())

  gtfstrips <- read.csv(paste0(tempdir(),"/trips.txt"))
  gtfsroutes <- read.csv(paste0(tempdir(),"/routes.txt"))
  gtfsagency <- read.csv(paste0(tempdir(),"/agency.txt"))
  gtfsshape <- read.csv(paste0(tempdir(),"/shapes.txt"))

  unlink(gtfsfiles)

  gtfslines <- sp::SpatialLinesDataFrame((gtfsshape %>%
      dplyr::group_by(shape_id) %>%
      dplyr::arrange(shape_pt_sequence) %>%
      dplyr::do(
        gtfsline = sp::Lines(sp::Line(as.matrix(.[,c('shape_pt_lon','shape_pt_lat')])),unique(.$shape_id))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::do(
        gtfsline = sp::SpatialLines(.[[2]],
                                    proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))))[[1]][[1]],
   data=gtfstrips %>%
     dplyr::inner_join(
       gtfsroutes) %>%
     dplyr::distinct(
       route_id,
       shape_id,
       route_short_name,
       route_long_name,
       route_desc,
       route_type,
       route_color,
       route_text_color
     ) %>%
     dplyr::inner_join(
       gtfsagency
     ) %>%
     dplyr::do(
       `rownames<-`(.,.$shape_id))
  )
  rm(gtfstrips,gtfsshape,gtfsagency,gtfsroutes)
  return(gtfslines)
}