#' Decode Google polyline compressed string
#'
#' @section Details:
#' An implementation of the Google Maps Encoded Polyline Algorithm for decoding
#' strings. Returns a dataframe if polyline is of length 1 and a list of
#' dataframes otherwise.
#'
#' @param polyline A character string or vector of character strings containing
#' the encoded polyline to be decoded.
#' @param precision An integer indicating the number of decimals in the
#' initial encoded coordinates. Default is 6 (for OSRM default).
#' @export
#' @examples \dontrun{
#'  decode_gl("_p~iF~ps|U_ulLnnqC_mqNvxq`@@", precision = 5)
#' }
decode_gl <- function(polyline, precision=6) {

  latlngsets <- lapply(polyline, function(polyline, precision){

  binvals <- R.utils::intToBin(stringi::stri_enc_toutf32(polyline)[[1]]-63)
  lastbinvals <- which(substr(binvals,1,1)==0)

  vseq <- Vectorize(seq)

  fullstrs <- substr(binvals, 2, 6)[unlist(as.vector(mapply(vseq, lastbinvals, c(1,(lastbinvals+1)[1:length(lastbinvals)-1]))))]
  fullstrs <- strtoi(unlist(lapply(
    as.vector(mapply(vseq,  c(1,(lastbinvals+1)[1:length(lastbinvals)-1]),lastbinvals)),
    FUN=function(x){
      paste0(fullstrs[x],collapse='')
    })),base=2)
  latlngs <- (ifelse(bitwAnd(fullstrs, 1) == 0,
                     bitwShiftR(fullstrs, 1),
                     (fullstrs - (bitwShiftR(fullstrs, 1))) * (-1)
  ))*(10^(-precision))
  latlngs <- data.frame(
    lat = cumsum(latlngs[seq.int(1,length(latlngs)-1,2)]),
    lng = cumsum(latlngs[seq.int(2,length(latlngs),2)])
  )

  return(latlngs)
  },precision=precision)

  if (length(latlngsets) == 1) {
    return(latlngsets[[1]])
  } else {
    return(latlngsets)
  }
}

#' Convert json result of OSRM routing query to SpatialLinesDataFrame
#'
#' @section Details:
#' Converts the result of a (successful) OSRM routing query and returns a
#' SpatialLinesDataFrame containing the route, route summary and instructions.
#'
#' @param osrmresult String containing encoded json result of OSRM routing
#' query.
#' @export
#' @examples \dontrun{
#' library(RCurl)
#'  viaroute2sldf(
#'    RCurl::getURL(paste0(
#'    "http://router.project-osrm.org/viaroute?loc=52.503033,13.420526&",
#'    "loc=52.516582,13.429290&instructions=true"))
#'  )
#' }
viaroute2sldf <- function(osrmresult) {

  osrmjson <- jsonlite::fromJSON(osrmresult)

  routecoords <- decode_gl(osrmjson$route_geometry)

  if (exists("route_instructions",osrmjson) == TRUE) {
    if (length(osrmjson$route_instructions[[length(osrmjson$route_instructions)]]) <
        length(osrmjson$route_instructions[[1]])) {
      osrmjson$route_instructions[[length(osrmjson$route_instructions)]][9] <- 1
    }

    osrmrouteinstruct <- setNames(data.frame(lapply(data.frame(t(sapply(osrmjson$route_instructions, `[`))), unlist)),
                                  c(
                                    "directions_code",
                                    "street_name",
                                    "length",
                                    "position",
                                    "time",
                                    "formatted_length",
                                    "direction",
                                    "azimuth",
                                    "mode"
                                  )
    )
    osrmrouteinstruct[,which(names(osrmrouteinstruct) %in% c('directions_code','length','position','time','azimuth','mode'))] <-
      sapply(
        osrmrouteinstruct[,which(names(osrmrouteinstruct) %in% c('directions_code','length','position','time','azimuth','mode'))],
        as.character
      )
    osrmrouteinstruct[,which(names(osrmrouteinstruct) %in% c('directions_code','length','position','time','azimuth','mode'))] <-
      sapply(
        osrmrouteinstruct[,which(names(osrmrouteinstruct) %in% c('directions_code','length','position','time','azimuth','mode'))],
        as.numeric
      )
    osrmrouteinstruct$routesect <- 1:nrow(osrmrouteinstruct)

    osrmsldf <- sp::SpatialLinesDataFrame(
      sp::SpatialLines(
        apply(
          cbind(
            osrmrouteinstruct[1:(nrow(osrmrouteinstruct)-1),]$position+1,
            osrmrouteinstruct[2:(nrow(osrmrouteinstruct)),]$position+1,
            1:(nrow(osrmrouteinstruct)-1)
          ),
          1,
          FUN=function(x,coords){
            sp::Lines(sp::Line(coords=routecoords[x[1]:x[2],c(2,1)]),ID=x[3])
          },coords=routecoords
        ),
        proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
      ),
      data=osrmrouteinstruct[1:(nrow(osrmrouteinstruct)-1),]
    )
  } else {
    osrmsldf <- sp::SpatialLinesDataFrame(
      sp::SpatialLines(
        list(sp::Lines(sp::Line(coords=routecoords),ID=1)),
        proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
      ),
      data=data.frame(routesect = 1)
    )
  }

  osrmsldf@data <- cbind(osrmsldf@data, as.data.frame(osrmjson$route_summary))
  osrmsldf@data$route_name <- paste(osrmjson$route_name, collapse=', ')
  osrmsldf@data$routesect <- 1:nrow(osrmsldf@data)
  osrmsldf@data$routeid <- 1

  if (osrmjson$found_alternative == TRUE) {

    i <- 1
    while (i <= length(osrmjson$alternative_geometries)) {

      routecoords <- decode_gl(osrmjson$alternative_geometries[i])

      if (exists("route_instructions",osrmjson) == TRUE) {

        if (length(osrmjson$alternative_instructions[[i]][[length(osrmjson$alternative_instructions[[i]])]]) <
            length(osrmjson$alternative_instructions[[i]][[1]])) {
          osrmjson$alternative_instructions[[i]][[length(osrmjson$alternative_instructions[[i]])]][9] <- 1
        }
        osrmrouteinstruct <- setNames(data.frame(lapply(data.frame(t(sapply(osrmjson$alternative_instructions[[i]], `[`))), unlist)),
                                      c(
                                        "directions_code",
                                        "street_name",
                                        "length",
                                        "position",
                                        "time",
                                        "formatted_length",
                                        "direction",
                                        "azimuth",
                                        "mode"
                                      )
        )
        osrmrouteinstruct[,which(names(osrmrouteinstruct) %in% c('directions_code','length','position','time','azimuth','mode'))] <-
          sapply(
            osrmrouteinstruct[,which(names(osrmrouteinstruct) %in% c('directions_code','length','position','time','azimuth','mode'))],
            as.character
          )
        osrmrouteinstruct[,which(names(osrmrouteinstruct) %in% c('directions_code','length','position','time','azimuth','mode'))] <-
          sapply(
            osrmrouteinstruct[,which(names(osrmrouteinstruct) %in% c('directions_code','length','position','time','azimuth','mode'))],
            as.numeric
          )
        osrmrouteinstruct$routesect <- 1:nrow(osrmrouteinstruct)

        osrmsldfalt <- sp::SpatialLinesDataFrame(
          sp::SpatialLines(
            apply(
              cbind(
                osrmrouteinstruct[1:(nrow(osrmrouteinstruct)-1),]$position+1,
                osrmrouteinstruct[2:(nrow(osrmrouteinstruct)),]$position+1,
                (1:(nrow(osrmrouteinstruct)-1))+nrow(osrmsldf@data)
              ),
              1,
              FUN=function(x,coords){
                sp::Lines(sp::Line(coords=routecoords[x[1]:x[2],c(2,1)]),ID=x[3])
              },coords=routecoords
            ),
            proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
          ),
          data=data.frame(osrmrouteinstruct[1:(nrow(osrmrouteinstruct)-1),],
                          row.names = (1:(nrow(osrmrouteinstruct)-1))+nrow(osrmsldf@data))
        )
      } else {
        osrmsldfalt <- sp::SpatialLinesDataFrame(
          sp::SpatialLines(
            list(sp::Lines(sp::Line(coords=routecoords),ID=1+i)),
            proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
          ),
          data=data.frame(routesect = 1, row.names = c(1+i))
        )
      }

      osrmsldfalt@data <- cbind(osrmsldfalt@data, osrmjson$alternative_summaries[i,])
      osrmsldfalt@data$route_name <- paste(osrmjson$alternative_names[[i]], collapse=', ')
      osrmsldfalt@data$routesect <- 1:nrow(osrmsldfalt@data)
      osrmsldfalt@data$routeid <- 1+i

      osrmsldf <- maptools::spRbind(osrmsldf, osrmsldfalt)

      i <- i + 1
    }

  }

  return(osrmsldf)

}