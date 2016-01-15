#' Import GTFS shapes and route data to SpatialLinesDataFrame.
#'
#' Takes a string with the file path of the zip file with the GTFS feed,
#' imports the shapes (geometry), route and agency data and returns a
#' SpatialLinesDataFrame for the GTFS feed.
#'
#' @param gtfszip String with the file path of the GTFS feed zip file
#' @export
#' @examples
#' f <- system.file("extdata", "beartransit-ca-us.zip", package = "stplanr")
#' # update file to latest version
#' # see https://code.google.com/p/googletransitdatafeed/wiki/PublicFeeds
#' u <- "http://data.trilliumtransit.com/gtfs/beartransit-ca-us/beartransit-ca-us.zip"
#' # download.file(u, f)
#' gtfs <- gtfs2sldf(gtfszip = f)
#' plot(gtfs, col = gtfs$route_long_name)
#' plot(gtfs[gtfs$route_long_name == "Central Campus",])
#' \dontrun{
#' # An example of a larger gtfs feed
#' download.file("http://www.yrt.ca/google/google_transit.zip",
#'                      paste0(tempdir(),"/gtfsfeed.zip"))
#' yrtgtfs <- gtfs2sldf(paste0(tempdir(),"/gtfsfeed.zip"))
#' sp::plot(yrtgtfs,col=paste0("#",yrtgtfs$route_color))
#' }
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
  if (all(charToRaw(substr(colnames(gtfstrips)[1],1,3)) == c(as.raw(239),as.raw(46),as.raw(46)))) {
    gtfstrips <- read.csv(paste0(tempdir(),"/trips.txt"),fileEncoding="UTF-8-BOM")
    gtfsroutes <- read.csv(paste0(tempdir(),"/routes.txt"),fileEncoding="UTF-8-BOM")
    gtfsagency <- read.csv(paste0(tempdir(),"/agency.txt"),fileEncoding="UTF-8-BOM")
    gtfsshape <- read.csv(paste0(tempdir(),"/shapes.txt"),fileEncoding="UTF-8-BOM")
  }
  else {
    gtfsroutes <- read.csv(paste0(tempdir(),"/routes.txt"))
    gtfsagency <- read.csv(paste0(tempdir(),"/agency.txt"))
    gtfsshape <- read.csv(paste0(tempdir(),"/shapes.txt"))
  }

  if (nrow(gtfsshape) == 0) {
    stop("GTFS shapes.txt file is empty.")
  }

  unlink(gtfsfiles)

  gtfslines <- sp::SpatialLinesDataFrame((gtfsshape %>%
      dplyr::group_by_(~shape_id) %>%
      dplyr::arrange_(~shape_pt_sequence) %>%
      dplyr::do_(
        gtfsline = "sp::Lines(sp::Line(as.matrix(.[,c('shape_pt_lon','shape_pt_lat')])),unique(.$shape_id))"
      ) %>%
      dplyr::ungroup() %>%
      dplyr::do_(
        gtfsline = "sp::SpatialLines(.[[2]],
                                    proj4string = sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))"))[[1]][[1]],
   data=gtfstrips %>%
     dplyr::inner_join(
       gtfsroutes
     ) %>%
     dplyr::distinct_(
       ~route_id,
       ~shape_id,
       ~route_short_name,
       ~route_long_name,
       ~route_desc,
       ~route_type,
       ~route_color,
       ~route_text_color,
       ~agency_id
     ) %>%
     dplyr::select_(~route_id,
            ~shape_id,
            ~route_short_name,
            ~route_long_name,
            ~route_desc,
            ~route_type,
            ~route_color,
            ~route_text_color,
            ~agency_id) %>%
     dplyr::inner_join(
       gtfsagency
     ) %>%
     dplyr::do_(
       "`rownames<-`(.,.$shape_id)"
     ))
  rm(gtfstrips,gtfsshape,gtfsagency,gtfsroutes)
  return(gtfslines)
}

