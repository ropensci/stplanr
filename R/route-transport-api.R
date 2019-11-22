#' Plan a single route with TransportAPI.com
#'
#' Provides an R interface to the TransportAPI.com public transport API.
#' The function returns a SpatialLinesDataFrame object representing the
#' public route.
#' Currently only works for the United Kingdom.
#' See <https://developer.transportapi.com/documentation>for more information.
#'
#' @param from Text string or coordinates (a numeric vector of
#'  `length = 2` representing latitude and longitude) representing a point
#'  on Earth.
#'
#' @param to Text string or coordinates (a numeric vector of
#'  `length = 2` representing latitude and longitude) representing a point
#'  on Earth. This represents the destination of the trip.
#'
#' @param silent Logical (default is FALSE). TRUE hides request sent.
#' @param region String for the active region to use for journey plans.
#' Possible values are 'southeast' (default) or 'tfl'.
#' @param modes Vector of character strings containing modes to use. Default is
#' to use all modes.
#' @param not_modes Vector of character strings containing modes not to use.
#' Not used if `modes` is set.
#'
#' @details
#'
#' This function uses the online routing service
#' TransportAPI.com to find public routes
#' between origins and destinations. It does not require
#' any key to access the API.
#'
#' Note that if `from` and `to` are supplied as
#' character strings (instead of lon/lat pairs), Google's
#' geo-coding services are used via `geo_code`.
#'
#' Note: there is now a dedicated transportAPI package:
#' https://github.com/ITSLeeds/transportAPI
#'
#' @family routes
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
#' # Aim plan public transport routes with transportAPI
route_transportapi_public <- function(from, to, silent = FALSE,
                                      region = "southeast", modes = NA, not_modes = NA) {

  # Convert sp object to lat/lon vector
  if (class(from) == "SpatialPoints" | class(from) == "SpatialPointsDataFrame") {
    from <- coordinates(from)
  }
  if (class(to) == "SpatialPoints" | class(to) == "SpatialPointsDataFrame") {
    to <- coordinates(to)
  }

  # Convert character strings to lon/lat if needs be
  if (is.character(from)) {
    from <- geo_code(from)
  }
  if (is.character(to)) {
    to <- geo_code(to)
  }

  orig <- paste0(from, collapse = ",")
  dest <- paste0(to, collapse = ",")

  api_base <- "http://fcc.transportapi.com"
  ft_string <- paste0("/from/lonlat:", orig, "/to/lonlat:", dest)

  queryattrs <- list(region = region)
  if (is.na(modes) == FALSE) {
    queryattrs[["modes"]] <- paste0(modes, collapse = "-")
  } else {
    if (is.na(not_modes) == FALSE) {
      queryattrs[["not_modes"]] <- paste0(not_modes, collapse = "-")
    }
  }

  httrreq <- httr::GET(
    url = api_base,
    path = paste0("/v3/uk/public/journey", ft_string, ".json"),
    query = queryattrs
  )

  if (silent == FALSE) {
    print(paste0("The request sent to transportapi was: ", httrreq$request$url))
  }

  if (grepl("application/json", httrreq$headers$`content-type`) == FALSE &
    grepl("js", httrreq$headers$`content-type`) == FALSE) {
    stop("Error: Transportapi did not return a valid result")
  }

  txt <- httr::content(httrreq, as = "text", encoding = "UTF-8")

  if (txt == "") {
    stop("Error: Transportapi did not return a valid result")
  }

  obj <- jsonlite::fromJSON(txt)

  coords <- obj$routes$route_parts[[1]]$coordinates
  coords <- do.call(rbind, coords)
  route <- sp::SpatialLines(list(sp::Lines(list(sp::Line(coords)), ID = 1)))
  proj4string(route) <- sp::CRS("+init=epsg:4326")

  # for the future: add summary data on the route
  route
}
