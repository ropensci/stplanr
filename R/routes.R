#' Plan a single route with CycleStreets.net
#'
#' Provides an R interface to the CycleStreets.net cycle planning API,
#' a route planner made by cyclists for cyclists.
#' The function returns a SpatialLinesDataFrame object representing the
#' an estimate of the fastest, quietest or most balance route.
#' Currently only works for the United Kingdom and part of continental Europe.
#' See \url{http://www.cyclestreets.net/api/}for more information.
#'
#' @param from Text string or coordinates (a numeric vector of
#'  \code{length = 2} representing latitude and longitude) representing a point
#'  on Earth.
#'
#' @param to Text string or coordinates (a numeric vector of
#'  \code{length = 2} representing latitude and longitude) representing a point
#'  on Earth. This represents the destination of the trip.
#'
#' @param plan Text strong of either "fastest", "quietest" or "balanced"
#' @param silent Logical (default is FALSE). TRUE hides request sent.
#' @param pat The API key used - this is usually aquired automatically through a helper
#' function
#'
#' @details
#'
#' This function uses the online routing service
#' CycleStreets.net to find routes suitable for cyclists
#' between origins and destinations. Requires an
#' internet connection, a CycleStreets.net API key
#' and origins and destinations within the UK to run.
#'
#' Note that if \code{from} and \code{to} are supplied as
#' character strings (instead of lon/lat pairs), Google's
#' geo-coding services are used via \code{RgoogleMaps::getGeoCode()}.
#'
#' You need to have an api key for this code to run.
#' Loading a locally saved copy of the api key text string
#' before running the function, for example, will ensure it
#' is available on any computer:
#'
#' \code{
#' mytoken <- readLines("~/Dropbox/dotfiles/cyclestreets-api-key-rl")
#' Sys.setenv(CYCLESTREET = mytoken)
#' }
#'
#' if you want the API key to be available in future
#' sessions, set it using the .Renviron file
#' e.g. on Linux machines in bash via:
#'
#' \code{
#' echo "CYCLESTREET=f3fe3d078ac34737" >> ~/.Renviron
#' }
#'
#' Read more about the .Renviron here: \code{?.Renviron}
#'
#'
#' @inheritParams line2route
#'
#' @export
#'
#' @seealso line2route
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
#' rb_pa <- route_cyclestreet("Pedaller's Arms, Leeds", "University of Leeds", "balanced")
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
#'
route_cyclestreet <- function(from, to, plan = "fastest", silent = TRUE, pat = cyclestreet_pat()){

  # Convert character strings to lon/lat if needs be
  if(is.character(from) | is.character(to)){
    from <- rev(RgoogleMaps::getGeoCode(from))
    to <- rev(RgoogleMaps::getGeoCode(to))
  }

  orig <- paste0(from, collapse = ",")
  dest <- paste0(to, collapse = ",")
  api_base <- sprintf("https://api.cyclestreets.net/v2/")
  ft_string <- paste(orig, dest, sep = "|")
  journey_plan <- sprintf("journey.plan?waypoints=%s&plan=%s", ft_string, plan)
  request <- paste0(api_base, journey_plan)
  request <- paste0(request, "&key=", pat)

  if(silent == FALSE){
    print(paste0("The request sent to cyclestreets.net was: ", request))
  }

  txt <- httr::content(httr::GET(request), as = "text")
  obj <- jsonlite::fromJSON(txt)

  route <- sp::SpatialLines(list(sp::Lines(list(sp::Line(obj$features[3,]$geometry$coordinates)), ID = 1)))

  df <- data.frame(
    plan = obj$features[3,]$properties$plan,
    start = obj$properties$start,
    finish = obj$properties$finish,
    length = obj$features[3,]$properties$length,
    time = obj$features[3,]$properties$time,
    waypoint = nrow(route@lines[[1]]@Lines[[1]]@coords)
  )

  row.names(df) <- route@lines[[1]]@ID
  route <- sp::SpatialLinesDataFrame(route, df)
  route
}

#' Plan a route with the graphhopper routing engine
#'
#' Provides an R interface to the graphhopper routing engine,
#' an open source route planning service.
#'
#' The function returns a SpatialLinesDataFrame object.
#' See \url{https://github.com/graphhopper} for more information.
#'
#' @param vehicle A text string representing the vehicle. Can be bike, bike2, car
#' or foot.
#'
#' @details
#'
#' To test graphopper is working for you, try something like this, but with
#' your own API key:
#' To use this function you will need to obtain an API key from
#' \url{https://graphhopper.com/#directions-api}.
#' It is assumed that you have set your api key as a system environment
#' for security reasons (so you avoid typing the API key in your code).
#' Do this by adding the following to your .Renviron file (see \code{?.Renviron}
#' or \url{https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html}
#' for more on this):
#'
#' \code{GRAPHHOPPER='FALSE-Key-eccbf612-214e-437d-8b73-06bdf9e6877d'}.
#'
#' (Note: key not real, use your own key.)
#'
#' \code{obj <- jsonlite::fromJSON(url)}
#'
#' Where \code{url} is an example api request from
#'  \url{https://github.com/graphhopper/directions-api/blob/master/docs-routing.md}.
#'
#' @inheritParams route_cyclestreet
#'
#' @export
#'
#' @seealso route_cyclestreet
#'
#' @examples
#'
#' \dontrun{
#' r <- route_graphhopper("Leeds", "Dublin", vehicle = "bike")
#' r2 <- route_graphhopper("New York", "Washington", vehicle = "foot")
#' library(leaflet)
#'
#' leaflet() %>% addTiles() %>% addPolylines(data = r)
#' }

route_graphhopper <- function(from, to, vehicle = "bike", silent = TRUE, pat = graphhopper_pat()){

  # Convert character strings to lon/lat if needs be
  if(is.character(from) | is.character(to)){
    from <- rev(RgoogleMaps::getGeoCode(from))
    to <- rev(RgoogleMaps::getGeoCode(to))
  }

  api_base <- "https://graphhopper.com/api/1/route?"
  orig <- paste0(from[2:1], collapse = "%2C")
  dest <- paste0(to[2:1], collapse = "%2C")
  ft_string <- paste0("point=", orig, "&point=", dest)
  veh <- paste0("&vehicle=", vehicle)

  request <- paste0(api_base, ft_string, veh,
                    "&locale=en-US&debug=true&points_encoded=false&key=", pat)

  if(vehicle == "bike"){
    request <- paste0(request, "&elevation=true")
  }

  if(silent == FALSE){
    print(paste0("The request sent was: ", request))
  }

  txt <- httr::content(httr::GET(request), as = "text")
  obj <- jsonlite::fromJSON(txt)

  route <- sp::SpatialLines(list(sp::Lines(list(sp::Line(obj$paths$points[[1]][[1]][,1:2])), ID = "1")))

  climb <- NA # to set elev variable up

  # get elevation data if it was a bike trip
  if(vehicle == "bike"){
    elevs <- obj$paths$points[[1]][[1]][,3]
    climb <- elevs[-1] - elevs[-length(elevs)]
    climb <- sum(climb[climb > 0])
  }

  # Attribute data for the route
  df <- data.frame(

    time = obj$paths$time / (1000 * 60),
    dist = obj$paths$distance,
    climb = climb

  )

  route <- sp::SpatialLinesDataFrame(route, df)

  route

}

#' Retrieve personal access token.
#'
#' A personal access token.
#'
#' @keywords internal
#' @export
cyclestreet_pat <- function(force = FALSE) {
  env <- Sys.getenv('CYCLESTREET')
  if (!identical(env, "") && !force) return(env)

  if (!interactive()) {
    stop("Please set env var CYCLESTREET to your github personal access token",
         call. = FALSE)
  }

  message("Couldn't find env var CYCLESTREET. See ?cyclestreet_pat for more details.")
  message("Please enter your API key you requested from https://www.cyclestreets.net/api/apply/,  and press enter:")
  pat <- readline(": ")

  if (identical(pat, "")) {
    stop("Personal access token entry failed", call. = FALSE)
  }

  message("Updating GRAPHHOPPER env var to API key. Save this to your .Renviron")
  Sys.setenv(CYCLESTREET = pat)

  pat

}

#' Retrieve personal access token.
#'
#' A personal access token.
#'
#' @keywords internal
#' @export
graphhopper_pat <- function(force = FALSE) {
  env <- Sys.getenv('GRAPHHOPPER')
  if (!identical(env, "") && !force) return(env)

  if (!interactive()) {
    stop("Please set env var GRAPHHOPPER to your github personal access token",
         call. = FALSE)
  }

  message("Couldn't find env var GRAPHHOPPER. See ?cyclestreet_pat for more details.")
  message("Please enter your API key, e.g. from https://graphhopper.com/dashboard/#/api-keys , and press enter:")
  pat <- readline(": ")

  if (identical(pat, "")) {
    stop("Personal access token entry failed", call. = FALSE)
  }

  message("Updating GRAPHHOPPER env var to API key")
  Sys.setenv(GRAPHHOPPER = pat)

  pat

}
