#' Plan a single route with CycleStreets.net
#'
#' Provides an R interface to the CycleStreets.net cycle planning API,
#' a route planner made by cyclists for cyclists.
#' The function returns a SpatialLinesDataFrame object representing the
#' an estimate of the fastest, quietest or most balance route.
#' Currently only works for the United Kingdom and part of continental Europe.
#' See \url{https://api.cyclestreets.net} and
#' \url{https://api.cyclestreets.net/v2/} for more information.
#'
#' @param from Text string or coordinates (a numeric vector of
#'  \code{length = 2} representing latitude and longitude) representing a point
#'  on Earth.
#'
#' @param to Text string or coordinates (a numeric vector of
#'  \code{length = 2} representing latitude and longitude) representing a point
#'  on Earth. This represents the destination of the trip.
#'
#' @details
#'
#' This function uses the online routing service
#' CycleStreets.net to find routes suitable for cyclists
#' between origins and destinations. Requires an
#' internet connection, a CycleStreets.net API key
#' and origins and destinations within the UK to run.
#'
#' Note that if \code{from} and \to{to} are supplied as
#' character strings (instead of lon/lat pairs), Google's
#' geo-coding services are used via \code{ggmap::geocode()}.
#'
#' @inheritParams gLines2CyclePath
#'
#' @export
#'
#' @seealso gLines2CyclePath
#'
#' @examples
#'
#' \dontrun{
#' # Plan the 'fastest' route between two points in Manchester
#' rf_mcr <- route_cyclestreet(from = "M3 4EE", to = "M1 4BT", plan = "fastest")
#' plot(rf_mcr)
#'
#' # Plan the 'quietest' route from Hereford to Leeds
#' rqh <- route_cyclestreet(from = "Hereford", to = "Leeds", plan = "quietest")
#' plot(rq_hfd)
#'
#' # Plan a 'balanced' route from Pedaller's Arms in Leeds
#' to the University of Leeds
#'
#' rb_pa <- route_cyclestreet(from = "Pedaller's Arms, Leeds", to = "University of Leeds", plan = "balanced")
#'
#' library(leaflet) # requires the leaflet CRAN package
#' # display the route on an interactive map
#' leaflet() %>% addTiles() %>%
#'   addPolylines(data = rb_pa)
#'
#' woodys_route = route_cyclestreet(from = "Stokesley", plan = "fastest", to = "Leeds")
#'
#' # Plan a route between two lat/lon pairs in the UK
#'
#' p1 <- c(-2, 52)
#' p2 <- p1 + 1 # add a degree!
#' route_cyclestreet(p1, p2, "fastest")
#' }
route_cyclestreet <- function(from = "M3 4EE", to = "M1 4BT", plan = "fastest"){
  if(!Sys.getenv('CYCLESTREET') == ""){
    cckey <- Sys.getenv('CYCLESTREET')
  }
  if(is.null(cckey)){
    stop("You must have a CycleStreets.net api key saved as 'cckey'")
  }

  # Convert character strings to lon/lat if needs be
  if(is.character(from) | is.character(to)){
    from <- ggmap::geocode(from)
    to <- ggmap::geocode(to)
  }

  orig <- paste0(from, collapse = ",")
  dest <- paste0(to, collapse = ",")
  api_base <- sprintf("https://api.cyclestreets.net/v2/")
  ft_string <- paste(orig, dest, sep = "|")
  journey_plan <- sprintf("journey.plan?waypoints=%s&plan=%s", ft_string, plan)
  request <- paste0(api_base, journey_plan)
  request <- paste0(request, "&key=", cckey)

  obj <- jsonlite::fromJSON(request)

  df <- data.frame(matrix(NA, ncol = 6))
  names(df) <- c("plan", "start", "finish", "length", "time", "waypoint")

  route <- SpatialLines(list(Lines(list(Line(obj$features[1,]$geometry$coordinates[[1]])), ID = 1)))

  df <- obj$features[1,]$properties
  row.names(df) <- route@lines[[1]]@ID
  route <- sp::SpatialLinesDataFrame(route, df)
  route
}
