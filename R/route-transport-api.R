#' Plan a single route with TransportAPI.com
#'
#' Provides an R interface to the TransportAPI.com public transport API.
#' The function returns a SpatialLinesDataFrame object representing the
#' public route.
#' Currently only works for the United Kingdom.
#' See \url{https://developer.transportapi.com/documentation}for more information.
#'
#' @param from Text string or coordinates (a numeric vector of
#'  \code{length = 2} representing latitude and longitude) representing a point
#'  on Earth.
#'
#' @param to Text string or coordinates (a numeric vector of
#'  \code{length = 2} representing latitude and longitude) representing a point
#'  on Earth. This represents the destination of the trip.
#'
#' @param silent Logical (default is FALSE). TRUE hides request sent.
#'
#' @details
#'
#' This function uses the online routing service
#' TransportAPI.com to find public routes
#' between origins and destinations. It does not require
#' any key to access the API.
#'
#' Note that if \code{from} and \code{to} are supplied as
#' character strings (instead of lon/lat pairs), Google's
#' geo-coding services are used via \code{RgoogleMaps::getGeoCode()}.
#'
#' @inheritParams line2route
#' @export
#' @seealso line2route
#' @examples
#'
#' \dontrun{
#' # Plan the 'public' route from Hereford to Leeds
#' rqh <- route_transportapi_public(from = "Hereford", to = "Leeds")
#' plot(rq_hfd)
#' }
#'
# Aim plan public transport routes with transportAPI

route_transportapi_public <- function(from, to, silent = FALSE){

  # Convert sp object to lat/lon vector
  if(class(from) == "SpatialPoints" | class(from) == "SpatialPointsDataFrame" )
    from <- coordinates(from)
  if(class(to) == "SpatialPoints" | class(to) == "SpatialPointsDataFrame" )
    to <- coordinates(to)

  # Convert character strings to lon/lat if needs be
  if(is.character(from))
    from <- rev(RgoogleMaps::getGeoCode(from))
  if(is.character(to))
    to <- rev(RgoogleMaps::getGeoCode(to))

  orig <- paste0(from, collapse = ",")
  dest <- paste0(to, collapse = ",")
  api_base = "http://fcc.transportapi.com/v3/uk/public/journey"
  ft_string <- paste0("/from/lonlat:", orig, "/to/lonlat:", dest)
  request <- paste0(api_base, ft_string,
                    ".json?region=southeast&")
  if (silent == FALSE) {
    print(paste0("The request sent to transportapi was: ",
                 request))
  }

  txt <- httr::content(httr::GET(request), as = "text")
  obj <- jsonlite::fromJSON(txt)#

  coords <- obj$routes$route_parts[[1]]$coordinates
  coords <- do.call(rbind, coords)
  route <- sp::SpatialLines(list(sp::Lines(list(sp::Line(coords)), ID = 1)))
  proj4string(route) <- CRS("+init=epsg:4326")

  # for the future: add summary data on the route
  route
}