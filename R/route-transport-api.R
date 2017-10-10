#' Plan a single route with TransportAPI.com
#'
#' Provides an R interface to the TransportAPI.com public transport API.
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
#' @param output Type of output. Either linestrings, multilinestrings or data.frame
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
#' geo-coding services are used via \code{geo_code}.
#'
#' @inheritParams line2route
#' @export
#' @seealso line2route
#' @examples
#'
#' \dontrun{
#' r = route_transportapi("hereford uk", "leeds uk")
#' mapview::mapview(r)
#' }
#'
route_transportapi <- function(from, to, silent = FALSE,
                               base_url = 'http://fcc.transportapi.com/v3/uk',
                               output = "linestrings",
                               mode = "bus"){

  base_url = paste0(base_url, "/public/journey/")

  # Convert sp object to lat/lon vector
  if(class(from) == "SpatialPoints" | class(from) == "SpatialPointsDataFrame" )
    from <- coordinates(from)
  if(class(to) == "SpatialPoints" | class(to) == "SpatialPointsDataFrame" )
    to <- coordinates(to)

  # Convert character strings to lon/lat if needs be
  if(is.character(from))
    from <- geo_code(from)
  if(is.character(to))
    to <- geo_code(to)

  orig <- paste0(from, collapse = ",")
  dest <- paste0(to, collapse = ",")

    q_url <- paste0(base_url, "from/lonlat:", orig, "/to/lonlat:", dest, ".json?", "mode=", mode)

  res <- httr::GET(q_url)

  txt <- httr::content(res, as = "text", encoding = "UTF-8")

  if (txt == "") {
    stop("Error: Transportapi did not return a valid result")
  }

  obj <- jsonlite::fromJSON(txt, simplifyDataFrame = TRUE)
  class(obj)
  names(obj)
  head(obj$routes$route_parts[[1]]$duration)

  if(output == "linestrings") {

    res_df = data.frame(
      mode = obj$routes$route_parts[[1]]$mode,
      from_point_name = obj$routes$route_parts[[1]]$from_point_name,
      to_point_name = obj$routes$route_parts[[1]]$to_point_name,
      destination = obj$routes$route_parts[[1]]$destination,
      line_name = obj$routes$route_parts[[1]]$line_name,
      duration = obj$routes$route_parts[[1]]$duration,
      departure_time = obj$routes$route_parts[[1]]$departure_time,
      arrival_time = obj$routes$route_parts[[1]]$arrival_time
      )

    coords <- lapply(obj$routes$route_parts[[1]]$coordinates, sf::st_linestring)
    route <- coords %>%
      sf::st_sfc() %>%
      sf::st_sf(crs = 4326, res_df)
  }

  # for the future: add summary data on the route
  route
}