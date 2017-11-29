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
#' @param forceline Boolean value indicating if the returned coordinates should
#' be a line (i.e., minimum two points) Default is TRUE.
#' @export
#' @examples \dontrun{
#'  decode_gl("_p~iF~ps|U_ulLnnqC_mqNvxq`@@", precision = 5)
#' }
decode_gl <- function(polyline, precision = 6, forceline = TRUE) {

  latlngsets <- lapply(polyline, function(polyline, precision){

  binvals <- R.utils::intToBin(stringi::stri_enc_toutf32(polyline)[[1]]-63)
  lastbinvals <- which(substr(binvals,1,1)==0)

  vseq <- Vectorize(seq)

  fullstrs <- substr(binvals, 2, 6)[unlist(as.vector(mapply(vseq, lastbinvals, c(1,(lastbinvals+1)[1:length(lastbinvals)-1]))))]
  fullstrs <- strtoi(unlist(lapply(
    as.vector(mapply(vseq,  c(1,(lastbinvals+1)[1:length(lastbinvals)-1]),lastbinvals, SIMPLIFY = FALSE)),
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

  if (forceline == TRUE) {
    latlngsets <- lapply(latlngsets, function(x, forceline){
      if (forceline == TRUE & nrow(x) == 1) {
        x <- rbind(x,x+0.0000000001)
      }
      return(x)
    }, forceline)
  }

  if (length(latlngsets) == 1) {
    return(latlngsets[[1]])
  } else {
    return(latlngsets)
  }
}

#' Query OSRM service and return json string result
#'
#' @section Details:
#' Constructs the query URL used with the OSRM HTTP API and returns a string
#' or vector of strings with the json-encoded results. Can be used in
#' conjunction with the viaroute2sldf function.
#'
#' @param startlat A single value or vector containing latitude(s) of the start
#' of routes.
#' @param startlng A single value or vector containing longitude(s) of the end
#' of routes.
#' @param endlat A single value or vector containing latitude(s) of the end of
#' routes.
#' @param endlng A single value or vector containing longitude(s) of the end
#' of routes.
#' @param viapoints A list of dataframes containing latitude (first column),
#' longitude (second) column for points to use for each route. Optionally a
#' third column containing a boolean value indicating if u-turns are allowed
#' at each viapoint.
#' @param api An integer value containing the OSRM API version (either 4 or 5).
#' Default is 5.
#' @param profile OSRM profile to use (for API v5), defaults to "driving".
#' @param protocol The protocol to use for the API (for v5), defaults to "v1".
#' @param osrmurl URL for OSRM sservice, e.g. an osrm instance running on
#' localhost. By default this is \code{"http://router.project-osrm.org"}.
#' @param zoom Zoom level for route geometry (0 to 18) for API v4
#' (default = 18). Higher values are more detailed.
#' @param instructions Boolean value to return instructions (default = TRUE).
#' @param alt Boolean value to return alternative routes (default = TRUE).
#' @param geometry Boolean value to return route geometries (default = TRUE).
#' @param uturns Boolean value to allow uturns at via points (default = TRUE).
#'
#' @export
#' @examples \dontrun{
#'   exroutes <- viaroute(50, 0, 51, 1)
#'   r <- viaroute2sldf(exroutes)
#'   plot(r)
#'   lngs <- c(-33.5,-33.6,-33.7)
#'   lats <- c(150,150.1,150.2)
#'   exroutes <- viaroute(viapoints = list(data.frame(x = lngs, y = lats)))
#'   r <- viaroute2sldf(exroutes)
#'   plot(r)
#' }
viaroute <- function(startlat = NULL, startlng = NULL, endlat = NULL,
                     endlng = NULL, viapoints = NULL,
                     api = 5, profile="driving", protocol = "v1",
                     osrmurl = "http://router.project-osrm.org", zoom=18,
                     instructions=TRUE, alt=TRUE, geometry=TRUE, uturns="default") {

  if (api == 4) {
    path <- "/viaroute"
  } else {
    path <- paste0("/route/",protocol,"/",profile,"/")
  }
  returnval <- c()

  instructions <- ifelse(instructions==TRUE,"true","false")
  alt <- ifelse(alt==TRUE,"true","false")
  geometry <- ifelse(geometry==TRUE,"true","false")
  uturns <- ifelse(uturns=="default",
                   ifelse(api == 5, "default", "true"),
                   ifelse(uturns==TRUE,"true","false"))

  if (missing(viapoints) == FALSE) {

    if (!is(viapoints,"list")) {
      stop("viapoints is not a list.")
    }

    i <- 1
    while (i <= length(viapoints)) {
      if(ncol(viapoints[[i]]) == 3) {
        if (api == 4) {

          httrreq <- httr::GET(
            url = osrmurl,
            path = path,
            query =
              c(unlist(
                apply(viapoints[[1]], 1, function(x){
                  list("loc"=paste0(x[1],",",x[2]), "u"=x[3])
                }),recursive=FALSE),
              list(
              "z"=zoom,
              "instructions"=instructions,
              "alt"=alt,
              "geometry"=geometry,
              "uturns"=uturns
            ))
          )
          if (grepl('application/json',httrreq$headers$`content-type`) == FALSE &
              grepl('js',httrreq$headers$`content-type`) == FALSE) {
            returnval[i] <- ''
          } else {
            returnval[i] <- gsub('\\\\\\\\\"','\\\\\\"',gsub('\\\\','\\\\\\\\',
                  httr::content(httrreq, as = "text", encoding = "UTF-8")))
          }
        } else {
          httrreq <- httr::GET(
            url = osrmurl,
            path = paste0(path,paste0(viapoints[[i]][,2],',',viapoints[[i]][,1],collapse=';')),
            query = list(
              "overview"="full",
              "steps"=instructions,
              "alternatives"=alt,
              "continue_straight"=uturns
            )
          )
          if (grepl('application/json',httrreq$headers$`content-type`) == FALSE &
              grepl('js',httrreq$headers$`content-type`) == FALSE) {
            returnval[i] <- ''
          } else {
            returnval[i] <- gsub('\\\\','\\\\\\\\',
              httr::content(httrreq, as = "text", encoding = "UTF-8"))
          }
        }
      }
      else {
        if (api == 4) {

          httrreq <- httr::GET(
            url = osrmurl,
            path = path,
            query =
              c(unlist(
                apply(viapoints[[1]], 1, function(x){
                  list("loc"=paste0(x[1],",",x[2]))
                }),recursive=FALSE),
                list(
                  "z"=zoom,
                  "instructions"=instructions,
                  "alt"=alt,
                  "geometry"=geometry,
                  "uturns"=uturns
                ))
          )
          if (grepl('application/json',httrreq$headers$`content-type`) == FALSE &
              grepl('js',httrreq$headers$`content-type`) == FALSE) {
            returnval[i] <- ''
          } else {
            returnval[i] <- gsub('\\\\\\\\\"','\\\\\\"',gsub('\\\\','\\\\\\\\',
                                                             httr::content(httrreq, as = "text", encoding = "UTF-8")))
          }
        } else {
          httrreq <- httr::GET(
            url = osrmurl,
            path = paste0(path,paste0(viapoints[[i]][,2],',',viapoints[[i]][,1],collapse=';')),
            query = list(
              "overview"="full",
              "steps"=instructions,
              "alternatives"=alt,
              "continue_straight"=uturns
            )
          )
          if (grepl('application/json',httrreq$headers$`content-type`) == FALSE &
              grepl('js',httrreq$headers$`content-type`) == FALSE) {
            returnval[i] <- ''
          } else {
            returnval[i] <- gsub('\\\\','\\\\\\\\',
                                 httr::content(httrreq, as = "text", encoding = "UTF-8"))
          }
        }
      }
      i <- i + 1
    }

  }
  else if (missing(startlat) == FALSE & missing(startlng) == FALSE &
           missing(endlat) == FALSE & missing(endlng) == FALSE) {

          if (length(startlat) != (sum(length(startlat), length(startlng), length(endlat), length(endlng))/4)) {
            stop("Length of vectors not equal")
          }

          if (length(startlat) == 1) {
            if (api == 4) {
              httrreq <- httr::GET(
                url = osrmurl,
                path = path,
                query = list(
                      "loc"=paste0(startlat,",",startlng),
                      "loc"=paste0(endlat,",",endlng),
                      "z"=zoom,
                      "instructions"=instructions,
                      "alt"=alt,
                      "geometry"=geometry,
                      "uturns"=uturns
                    )
              )
              if (grepl('application/json',httrreq$headers$`content-type`) == FALSE &
                  grepl('js',httrreq$headers$`content-type`) == FALSE) {
                returnval <- ''
              } else {
                returnval <- gsub('\\\\\\\\\"','\\\\\\"',gsub('\\\\','\\\\\\\\',
                                     httr::content(httrreq, as = "text", encoding = "UTF-8")))
              }
            } else {
              httrreq <- httr::GET(
                url = osrmurl,
                path = paste0(path,startlng,',',startlat,';',endlng,',',endlat),
                query = list(
                  "overview"="full",
                  "steps"=instructions,
                  "alternatives"=alt,
                  "continue_straight"=uturns
                )
              )
              if (grepl('application/json',httrreq$headers$`content-type`) == FALSE &
                  grepl('js',httrreq$headers$`content-type`) == FALSE) {
                returnval <- ''
              } else {
                returnval <- gsub('\\\\','\\\\\\\\',
                                     httr::content(httrreq, as = "text", encoding = "UTF-8"))
              }
            }
          }
          else {
            i <- 1
            while (i <= length(startlat)) {
              if (api == 4) {
                httrreq <- httr::GET(
                  url = osrmurl,
                  path = path,
                  query = list(
                    "loc"=paste0(startlat[i],",",startlng[i]),
                    "loc"=paste0(endlat[i],",",endlng[i]),
                    "z"=zoom,
                    "instructions"=instructions,
                    "alt"=alt,
                    "geometry"=geometry,
                    "uturns"=uturns
                  )
                )
                if (grepl('application/json',httrreq$headers$`content-type`) == FALSE &
                    grepl('js',httrreq$headers$`content-type`) == FALSE) {
                  returnval[i] <- ''
                } else {
                  returnval[i] <- gsub('\\\\\\\\\"','\\\\\\"',gsub('\\\\','\\\\\\\\',
                                                                httr::content(httrreq, as = "text", encoding = "UTF-8")))
                }
              } else {
                httrreq <- httr::GET(
                  url = osrmurl,
                  path = paste0(path,startlng[i],',',startlat[i],';',endlng[i],',',endlat[i]),
                  query = list(
                    "overview"="full",
                    "steps"=instructions,
                    "alternatives"=alt,
                    "continue_straight"=uturns
                  )
                )
                if (grepl('application/json',httrreq$headers$`content-type`) == FALSE &
                    grepl('js',httrreq$headers$`content-type`) == FALSE) {
                  returnval[i] <- ''
                } else {
                  returnval[i] <- gsub('\\\\','\\\\\\\\',
                                    httr::content(httrreq, as = "text", encoding = "UTF-8"))
                }
              }
              i <- i + 1
            }
          }

  }
  else {
    stop("Missing viapoints coordinates")
  }
  if (length(returnval) > 1) {
    removerowlist <- c()
    for (i in 1:length(returnval)) {
      if (returnval[i] == "") {
        removerowlist <- c(removerowlist,i)
      }
    }
    if (length(removerowlist) == length(returnval)) {
      stop("OSRM server returned empty result for all routes")
    } else if(length(removerowlist) > 0) {
      warning(paste0("Routes ",paste0(removerowlist, collapse=', '), " returned empty result, removing from result"))
      returnval <- returnval[-removerowlist]
    }
  } else {
    if (returnval == "") {
      stop("OSRM server returned empty result")
    }
  }

  return(returnval)

}

#' Convert json result of OSRM routing query to SpatialLinesDataFrame
#'
#' @section Details:
#' Converts the result of a (successful) OSRM routing query and returns a
#' SpatialLinesDataFrame containing the route, route summary and instructions.
#'
#' @param osrmresult Single character string or character vector containing
#' encoded json result(s) of OSRM routing queries.
#' @param return_sf Boolean value if this function should return an sf object, if
#' FALSE returns sp object (default FALSE).
#' @export
#' @examples \dontrun{
#'  viaroute2sldf(
#'    viaroute(startlat = 52.503033,
#'             startlng = 13.420526,
#'             endlat = 52.516582,
#'             endlng = 13.429290)
#'  )
#' }
viaroute2sldf <- function(osrmresult, return_sf = FALSE) {

  osrmsldf <- c()
  for (curroute in 1:length(osrmresult)) {
    osrmjson <- jsonlite::fromJSON(osrmresult[curroute])

    if (is(osrmjson$code,"character")) {
      api <- 5
    }
    else {
      api <- 4
    }

    if (api == 4) {
      routecoords <- decode_gl(osrmjson$route_geometry)

      osrmsldfalt <- viaroute2sldf_instruct(
                             ifelse(exists("route_instructions",osrmjson) == TRUE,
                                    list(osrmjson$route_instructions),
                                    FALSE),
                             osrmjson$route_summary,
                             routecoords,
                             routename = osrmjson$route_name,
                             existrow = 0,
                             routeid = ifelse(curroute == 1, 1, nrow(osrmsldf@data)+1),
                             return_sf = return_sf)

      if(is(osrmsldf, "SpatialLinesDataFrame") == FALSE) {
        osrmsldf <- osrmsldfalt
      } else {
        osrmsldf <- raster::bind(osrmsldf, osrmsldfalt)
      }

      if (osrmjson$found_alternative == TRUE) {

        i <- 1
        while (i <= length(osrmjson$alternative_geometries)) {

          routecoords <- decode_gl(osrmjson$alternative_geometries[i])

          osrmsldfalt <- viaroute2sldf_instruct(
                             ifelse(exists("alternative_instructions",osrmjson) == TRUE,
                                    list(osrmjson$alternative_instructions[[i]]),
                                    FALSE),
                             osrmjson$alternative_summaries[i,],
                             routecoords,
                             routename = osrmjson$alternative_names[[i]],
                             existrow = nrow(osrmsldf@data),
                             routeid = ifelse(curroute == 1, 1, nrow(osrmsldf@data)+1)+i,
                             return_sf = return_sf)

          if (return_sf == FALSE) {
            osrmsldf <- raster::bind(osrmsldf, osrmsldfalt)
          } else {
            osrmsldf <- rbind(osrmsldf, osrmsldfalt)
          }

          i <- i + 1
        }

      }

    } else {

      for (i in 1:length(osrmjson$routes$geometry)) {
        osrmjson$routes$geometry[i] <- gsub('\\\\\\\\','\\\\',osrmjson$routes$geometry[i])
      }
      for (i in 1:length(osrmjson$routes$legs)) {
        for (j in 1:length(osrmjson$routes$legs[[i]]$steps)) {
          osrmjson$routes$legs[[i]]$steps[[j]]$geometry <- gsub('\\\\\\\\','\\\\',osrmjson$routes$legs[[i]]$steps[[j]]$geometry)
        }
      }
      osrmsldfalt <- viaroute2sldf_instructv5(osrmjson, ifelse(curroute == 1, 1, nrow(osrmsldf@data)+1), return_sf = return_sf)
      if(is(osrmsldf, "SpatialLinesDataFrame") == FALSE & is(osrmsldf, "sf") == FALSE) {
        osrmsldf <- osrmsldfalt
      } else {
        if (return_sf == FALSE) {
          osrmsldf <- raster::bind(osrmsldf, osrmsldfalt)
        } else {
          osrmsldf <- rbind(osrmsldf, osrmsldfalt)
        }

      }

    }

  }

  return(osrmsldf)

}

viaroute2sldf_instruct <- function(routeinst, routesum, routecoords, routename = "", existrow = 0, routeid = 1, return_sf = FALSE) {

  if (class(routeinst) == "list") {
    routeinst <- unlist(routeinst, recursive = FALSE)
    if (length(routeinst[[1]]) == 11) {
      if (length(routeinst[[length(routeinst)]]) <
          length(routeinst[[1]])) {
        routeinst[[length(routeinst)]][length(routeinst[[1]])] <- routeinst[[length(routeinst)]][length(routeinst[[1]])-1]
        routeinst[[length(routeinst)]][length(routeinst[[1]])-1] <- routeinst[[length(routeinst)]][length(routeinst[[1]])-2]
        routeinst[[length(routeinst)]][length(routeinst[[1]])-2] <- routeinst[[length(routeinst)-1]][length(routeinst[[1]])-2]
      }
      dfnames <- c(
        "directions_code",
        "street_name",
        "length",
        "position",
        "time",
        "formatted_length",
        "direction",
        "azimuth",
        "mode",
        "preturn_direction",
        "preturn_azimuth"
      )
    }
    else {
      if (length(routeinst[[length(routeinst)]]) <
          length(routeinst[[1]])) {
        routeinst[[length(routeinst)]][length(routeinst[[1]])] <- routeinst[[length(routeinst)-1]][length(routeinst[[1]])]
      }

      dfnames <- c(
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

    }

    osrmrouteinstruct <- setNames(data.frame(lapply(data.frame(t(sapply(routeinst, `[`))), unlist)),
                                  dfnames
    )
    osrmrouteinstruct[,which(names(osrmrouteinstruct) %in% c('directions_code','length','position','time','azimuth','mode','preturn_azimuth'))] <-
      sapply(
        osrmrouteinstruct[,which(names(osrmrouteinstruct) %in% c('directions_code','length','position','time','azimuth','mode','preturn_azimuth'))],
        function(x) {
          as.numeric(as.character(x))
        }
      )
    osrmrouteinstruct$routesect <- 1:nrow(osrmrouteinstruct)

    osrmsldf <- sp::SpatialLinesDataFrame(
      sp::SpatialLines(
        apply(
          cbind(
            osrmrouteinstruct[1:(nrow(osrmrouteinstruct)-1),]$position+1,
            osrmrouteinstruct[2:(nrow(osrmrouteinstruct)),]$position+1,
            (1:(nrow(osrmrouteinstruct)-1)) + existrow
          ),
          1,
          FUN=function(x,coords){
            sp::Lines(sp::Line(coords=routecoords[x[1]:x[2],c(2,1)]),ID=x[3])
          },coords=routecoords
        ),
        proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
      ),
      data=data.frame(osrmrouteinstruct[1:(nrow(osrmrouteinstruct)-1),],
                      row.names = (1:(nrow(osrmrouteinstruct)-1))+existrow)
    )
  } else {
    osrmsldf <- sp::SpatialLinesDataFrame(
      sp::SpatialLines(
        list(sp::Lines(sp::Line(coords=routecoords),ID=1)),
        proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
      ),
      data=data.frame(routesect = 1, row.names = c(1+existrow))
    )
  }

  osrmsldf@data <- cbind(osrmsldf@data, as.data.frame(routesum))
  osrmsldf@data$route_name <- paste(routename, collapse=', ')
  osrmsldf@data$routesect <- 1:nrow(osrmsldf@data)
  osrmsldf@data$routeid <- routeid

  if (return_sf == TRUE) {
    osrmsldf <- sf::st_as_sf(osrmsldf)
  }

  return(osrmsldf)

}

viaroute2sldf_instructv5 <- function(routeinst, startrouteid = 1, return_sf = FALSE) {

  if (length(routeinst$routes$legs[[1]]$steps[[1]]$geometry) == 0) {
    osrmsldf <- sp::SpatialLinesDataFrame(
      sp::SpatialLines(
        lapply(1:length(routeinst$routes$geometry), function(i,x){
          sp::Lines(sp::Line(coords = decode_gl(x$routes$geometry[i], 5)),ID = i)
        },routeinst),
        proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")),
      dplyr::bind_rows(lapply(1:length(routeinst$routes$geometry), function(i,x){
        data.frame(routenum = i,
                   origin = x$waypoints$name[1],
                   dest = x$waypoints$name[2],
                   routedesc = "",
                   duration = x$routes$duration[i],
                   distance = x$routes$distance[i],
                   row.names = i)
      },routeinst))
    )
  } else {
    osrmsldf <- sp::SpatialLinesDataFrame(
      sp::SpatialLines(unlist(unlist(lapply(1:length(routeinst$routes$legs),
                                            function(i,x){
                                              lapply(1:length(x$routes$legs[[i]]$steps), function(j,x,i){
                                                lapply(
                                                  1:length(x$routes$legs[[i]]$steps[[j]]$geometry),
                                                  function(k,x,i,j,prevrows) {
                                                    sp::Lines(sp::Line(coords = decode_gl(x$routes$legs[[i]]$steps[[j]]$geometry[k],5)[,c(2,1)]),ID = startrouteid-1+prevrows+k)
                                                  },
                                                  x,i,j,
                                                  ifelse(i == 1, 0, sum(unlist(lapply(1:(i-1), function(i,x){length(x$routes$legs[[i]]$steps[[1]]$geometry)},x)))) +
                                                    ifelse(j == 1, 0, sum(unlist(lapply(1:(j-1), function(j,x,i){length(x$routes$legs[[i]]$steps[[j]]$geometry)},x,i))))
                                                )
                                              },x,i)
                                            },
                                            routeinst),recursive = FALSE), recursive = FALSE),
                       proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")),
      data.frame(dplyr::bind_rows(lapply(1:length(routeinst$routes$legs), function(i,x){
        dplyr::bind_rows(
          lapply(1:length(x$routes$legs[[i]]$summary), function(j,x,i){

            prevrows <- ifelse(i == 1, 0, sum(unlist(lapply(1:(i-1), function(i,x){length(x$routes$legs[[i]]$steps[[1]]$geometry)},x)))) +
              ifelse(j == 1, 0, sum(unlist(lapply(1:(j-1), function(i, x){length(x[[i]]$geometry)}, x = x$routes$legs[[i]]$steps))))

            if (length(x$routes$legs[[i]]$steps[[j]]$maneuver$exit) > 0) {
              exitvals <- x$routes$legs[[i]]$steps[[j]]$maneuver$exit
            } else {
              exitvals <- NA
            }
            if (length(x$routes$legs[[i]]$steps[[j]]$mode) > 0) {
              modevals <- x$routes$legs[[i]]$steps[[j]]$mode
            } else {
              modevals <- NA
            }

            data.frame(
              routenum = startrouteid-1+i,
              legnum = j,
              routedesc = as.character(rep(x$routes$legs[[i]]$summary[j],times=length(x$routes$legs[[i]]$steps[[j]]$distance))),
              streetname = as.character(x$routes$legs[[i]]$steps[[j]]$name),
              bearing_after = x$routes$legs[[i]]$steps[[j]]$maneuver$bearing_after,
              bearing_before = x$routes$legs[[i]]$steps[[j]]$maneuver$bearing_before,
              type = as.character(x$routes$legs[[i]]$steps[[j]]$maneuver$type),
              modifier = as.character(x$routes$legs[[i]]$steps[[j]]$maneuver$modifier),
              exit = as.character(exitvals),
              duration = x$routes$legs[[i]]$steps[[j]]$duration,
              distance = x$routes$legs[[i]]$steps[[j]]$distance,
              destination = as.character(ifelse(is.null(x$routes$legs[[i]]$steps[[j]]$destination), NA, x$routes$legs[[i]]$steps[[j]]$destination)),
              mode = as.character(modevals),
              rowid = startrouteid-1+(1:length(x$routes$legs[[i]]$steps[[j]]$distance))+prevrows,
              stringsAsFactors = FALSE
            )

          }, x, i)
        )},routeinst)),row.names = "rowid")
    )
    osrmsldf@data <- osrmsldf@data[,-ncol(osrmsldf@data)]
  }

  if (return_sf == TRUE) {
    osrmsldf <- sf::st_as_sf(osrmsldf)
  }

  return(osrmsldf)

}

#' Generate nearest point on the route network of a point from OSRM locate service
#'
#' @section Details:
#' Retrieve coordinates of the node(s) on the network mapped from coordinates
#' passed to functions.
#'
#' @param lat Numeric vector containing latitude coordinate for each coordinate
#' to map. Also accepts dataframe with latitude in the first column and
#' longitude in the second column.
#' @param lng Numeric vector containing longitude coordinate for each
#' coordinate to map.
#' @param number Number of locations to return (API v5 only)
#' @param api An integer value containing the OSRM API version (either 4 or 5).
#' Default is 5.
#' @param profile OSRM profile to use (for API v5), defaults to "driving".
#' @param protocol The protocol to use for the API (for v5), defaults to "v1".
#' @param osrmurl Base URL of the OSRM service
#' @param return_sf Boolean value if this function should return an sf object, if
#' FALSE returns sp object (default FALSE).
#' @export
#' @examples \dontrun{
#'  nearest_osm(
#'    lat = 50.3,
#'    lng = 13.2
#'  )
#' }
nearest_osm <- function(lat, lng, number = 1,
                        api = 5, profile="driving", protocol = "v1",
                        osrmurl = "http://router.project-osrm.org",
                        return_sf = FALSE){
  if (is(lat,"data.frame")) {
    lng = lat[,2]
    lat = lat[,1]
  }
  if (api == 4) {
    url = paste0(osrmurl, "/nearest?loc=", lat, ",", lng)
    thisspdf <- sp::SpatialPointsDataFrame(coords = matrix(unlist(lapply(
                             url, function(x){
                               matrix(jsonlite::fromJSON(x)$mapped_coordinate, ncol=2)}
                             ),recursive = FALSE),ncol=2,byrow = TRUE),
                           data = data.frame(orig_lat = lat, orig_lng = lng),
                           proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
    if (return_sf == TRUE) {
      sf::st_as_sf(thisspdf)
    } else {
      thisspdf
    }
  } else {
    if(number < 1) {
      stop("number must be greater than 0")
    }
    url = paste0(osrmurl,"/nearest/",protocol,"/",profile,"/",lng,",",lat,"?number=",number)
    jsondata <- lapply(url, function(x){
      jsonlite::fromJSON(x)
    })

    thisspdf <- sp::SpatialPointsDataFrame(coords = matrix(unlist(lapply(
      jsondata, function(x){
        matrix(x$waypoints$location[[1]], ncol=2)}
      ),recursive = FALSE),ncol=2,byrow = TRUE),
        data = cbind(data.frame(orig_lat = lat, orig_lng = lng),
        dplyr::bind_rows(lapply(jsondata, function(x){
          data.frame(distance = x$waypoints$distance,
                     name = x$waypoints$name)
        }),.id = "locnum")),
        proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

    if (return_sf == TRUE) {
      sf::st_as_sf(thisspdf)
    } else {
      thisspdf
    }

  }
}

#' Return SpatialPointsDataFrame with located points from OSRM locate service
#'
#' @section Details:
#' Retrieve coordinates of the node(s) on the network mapped from coordinates
#' passed to functions using OSRM API v4 only. For API v5, use nearest_osm.
#'
#' @param lat Numeric vector containing latitude coordinate for each coordinate
#' to map. Also accepts dataframe with latitude in the first column and
#' longitude in the second column.
#' @param lng Numeric vector containing longitude coordinate for each
#' coordinate to map.
#' @param osrmurl Base URL of the OSRM service
#' @param return_sf Boolean value if this function should return an sf object, if
#' FALSE returns sp object (default FALSE).
#' @export
#' @examples \dontrun{
#'  locate2spdf(
#'    lat = c(50.3, 50.2),
#'    lng = c(13.2, 13.1)
#'  )
#' }
#'
locate2spdf <- function(lat, lng = lng, osrmurl = "http://router.project-osrm.org", return_sf = FALSE) {

  if (return_sf == TRUE) {
    return(sf::st_as_sf(getlocnear(lat = lat, lng = lng, osrmurl = osrmurl, "locate")))
  } else {
    return(getlocnear(lat = lat, lng = lng, osrmurl = osrmurl, "locate"))
  }

}

#' Return SpatialPointsDataFrame with nearest street from OSRM nearest service
#'
#' @section Details:
#' Retrieve coordinates and name of the node(s) on the network mapped from
#' coordinates passed to functions using OSRM API v4 only. For API v5,
#' use nearest_osm.
#'
#' @param lat Numeric vector containing latitude coordinate for each coordinate
#' to map. Also accepts dataframe with latitude in the first column and
#' longitude in the second column.
#' @param lng Numeric vector containing longitude coordinate for each
#' coordinate to map.
#' @param osrmurl Base URL of the OSRM service
#' @param return_sf Boolean value if this function should return an sf object, if
#' FALSE returns sp object (default FALSE).
#' @export
#' @examples \dontrun{
#'  nearest2spdf(
#'    lat = c(50.3, 50.2),
#'    lng = c(13.2, 13.1)
#'  )
#' }
#'
nearest2spdf <- function(lat, lng, osrmurl = "http://router.project-osrm.org", return_sf = FALSE) {

  if (return_sf == TRUE) {
    return(sf::st_as_sf(getlocnear(lat = lat, lng = lng, osrmurl = osrmurl, "nearest")))
  } else {
    return(getlocnear(lat = lat, lng = lng, osrmurl = osrmurl, "nearest"))
  }

}

getlocnear <- function(lat, lng, osrmurl = "http://router.project-osrm.org", service = "locate") {
  if(is(lat,"data.frame")) {
    lng <- lat[,2]
    lat <- lat[,1]
  }
  if(length(lat) != length(lng)) {
    stop("Error - Lengths of vectors not equal.")
  }

  if (service == "locate") {
    coorddf <- data.frame(origlat = lat, origlng = lng, mappedlat = NA, mappedlng = NA, status = NA)
  }
  else {
    coorddf <- data.frame(origlat = lat, origlng = lng, mappedlat = NA, mappedlng = NA, street = NA, status = NA)
  }

  i <- 1
  p <- dplyr::progress_estimated(length(lat), min_time = 5)
  while (i <= length(lat)) {
    httrreq <- httr::GET(
      url = osrmurl,
      path = service,
      query = list("loc"=paste0(coorddf[i,]$origlat,',',coorddf[i,]$origlng))
    )
    locatedata2 <- jsonlite::fromJSON(httr::content(httrreq, as = "text"))
    if (locatedata2$status == 0 | locatedata2$status == 200) {
      if (service == "locate") {
        coorddf[i,c("mappedlat","mappedlng","status")] <- c(locatedata2$mapped_coordinate,0)
      }
      else {
        coorddf[i,c("mappedlat","mappedlng","street","status")] <- c(locatedata2$mapped_coordinate,locatedata2$name,0)
      }
    }
    else {
      coorddf[i,"status"] <- locatedata2$status
    }
    p$tick()$print()
    i <- i + 1
  }
  coorddf$mappedlat <- as.numeric(as.character(coorddf$mappedlat))
  coorddf$mappedlng <- as.numeric(as.character(coorddf$mappedlng))

  osrmspdf <- sp::SpatialPointsDataFrame(
    sp::SpatialPoints(
      coords=coorddf[,c('mappedlng','mappedlat')],
      proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    ),
    data=coorddf[,which(!names(coorddf) %in% c('mappedlng','mappedlat'))]
  )

  return(osrmspdf)

}

#' Return Matrix containing travel times between origins and destinations
#'
#' @section Details:
#' Return a matrix containing travel times between origins and destinations
#'
#' @param lat Numeric vector containing latitude coordinate for each coordinate
#' to calculate travel times. Also accepts dataframe with latitude in the first
#' column and longitude in the second column.
#' @param lng Numeric vector containing longitude coordinate for each
#' coordinate to calculate travel times.
#' @param destlat Numeric vector containing destination latitude coordinate
#' for each coordinate to calculate travel times. Also accepts dataframe with
#' latitude in the first column and longitude in the second column. Default
#' is value of lat.
#' @param destlng Numeric vector containing longitude coordinate for each
#' destination coordinate to calculate travel times. Default is value of lng.
#' @param api An integer value containing the OSRM API version (either 4 or 5).
#' Default is 5.
#' @param profile OSRM profile to use (for API v5), defaults to "driving".
#' @param protocol The protocol to use for the API (for v5), defaults to "v1".
#' @param osrmurl Base URL of the OSRM service
#' @export
#' @examples \dontrun{
#'  table2matrix(seq(from=50,to=52,by=0.1),seq(from=12,to=14,by=0.1))
#' }
#'
table2matrix <- function(lat, lng, destlat = NA, destlng = NA,
                         api = 5, profile="driving", protocol = "v1",
                         osrmurl="http://router.project-osrm.org") {

  if(is(lat,"data.frame")) {
    lng <- lat[,2]
    lat <- lat[,1]
  }
  if(length(lat) != length(lng)) {
    stop("Error - Lengths of vectors not equal.")
  }

  if(is.na(destlat[1]) == TRUE) {
    destlat <- lat
    destlng <- lng
  } else {
    if (is(destlat,"data.frame")) {
      destlng <- destlat[,2]
      destlat <- destlat[,1]
    }
    if (length(destlat) != length(destlng)) {
      stop("Error - Length of destination vectors not equal")
    }
  }

  if (api == 5) {

    locations <- rbind(
      data.frame(lng=lng,lat=lat),
      data.frame(lng=destlng,lat=destlat)
    )
    locations <- unique(locations)
    sources <- which(locations$lng %in% lng & locations$lat %in% lat)-1
    destinations <- which(locations$lng %in% destlng & locations$lat %in% destlat)-1

    httrreq <- httr::GET(
      url = osrmurl,
      path = paste0("table/",protocol,"/",profile,"/",
                    paste0(paste(locations$lng, locations$lat, sep=','), collapse=';')),
      query = list(
        "sources" = paste0(sources, collapse = ";"),
        "destinations" = paste0(destinations, collapse = ";")
      )
    )
    tabledata2 <- jsonlite::fromJSON(httr::content(httrreq, "text"))
    return(tabledata2$durations)
  } else {
    httrreq <- httr::GET(
      url = osrmurl,
      path = "table",
      query = unlist(
        apply(data.frame(lat = lat, lng = lng), 1, function(x){
          list("loc" = paste0(x, collapse = ","))
        }), recursive = FALSE
      )
    )
    tabledata2 <- jsonlite::fromJSON(httr::content(httrreq, "text"))
    return(tabledata2$distance_table)
  }

}
#' Plan a route with OSRM
#'
#' This is a wrapper around \code{viaroute} that returns a single route between A and B.
#'
#' @param singleline Should a single line be returned? Default is \code{TRUE}
#' @param ... Arguments passed to viaroute()
#' @inheritParams route_cyclestreet
#' @inheritParams viaroute
#' @export
#' @examples
#' \dontrun{
#' from = c(-1.55, 53.80) # geo_code("leeds")
#' to = c(-1.76, 53.80) # geo_code("bradford uk")
#' r = route_osrm(from, to)
#' plot(r)
#' r_many <- line2route(flowlines_sf[2:9,], route_osrm)
#' plot(cents)
#' plot(r_many, add = TRUE)
#' }
route_osrm <- function(from, to, alt = FALSE, ..., singleline = TRUE) {

  v <- viaroute(startlat = from[2], startlng = from[1], endlat = to[2], endlng = to[1], ...)
  r <- viaroute2sldf(v)

  if(!alt) {
    r <- r[1:min(which(r$type == "arrive")), ]
  }
  if(singleline) {
    r <- sp::SpatialLinesDataFrame(rgeos::gLineMerge(r),
                                   data.frame(distance = sum(r$distance),
                                              duration = sum(r$duration)))
  }

  return(r)

}
