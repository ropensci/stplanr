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
#'  @param plan Text strong of either "fastest", "quietest" or "balanced"
#'
#' @param silent Logical (default is FALSE). TRUE hides request sent.
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
#' # mytoken <- "f3fe3d078ac34737" # warning: not a real key - keep yours private
#' mytoken <- readLines("~/Dropbox/dotfiles/cyclestreets-api-key-rl")
#' Sys.setenv(CYCLESTREET = mytoken)
#' }
#'
#' \code{
#' cckey <- Sys.getenv('CYCLESTREET')
#' }
#'
#' or on Linux
#'
#' \code{
#' sudo -i # become route
#' echo "export CYCLESTREET='f3fe3d078ac34737'" >> /etc/environment
#' }
#'
#' Then restart so the envrionment variable is set and can be viewed with
#' \code{
#' $CYCLESTREET
#' }
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
route_cyclestreet <- function(from, to, plan = "fastest", silent = FALSE){
  if(!Sys.getenv('CYCLESTREET') == ""){
    cckey <- Sys.getenv('CYCLESTREET')
  }
  if(!exists("cckey")){
    stop("You must have a CycleStreets.net api key saved as 'cckey'")
  }

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
  request <- paste0(request, "&key=", cckey)

  if(silent == FALSE){
    print(paste0("The request sent to cyclestreets.net was: ", request))
  }

  obj <- jsonlite::fromJSON(request)

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
#' To use this function you will need to obtain an API key from
#' \url{https://graphhopper.com/#directions-api}.
#' It is assumed that you have set your api key as a system environment
#' for security reasons (so you avoid typing the API key in your code).
#' Do this with \code{GRAPHHOPPER}, e.g. by typing \code{
#' Sys.setenv(GRAPHHOPPER = 'eccbf612-214e-437d-8b73-06bdf9e6877d')
#' }. (Note: key not real, use your own key.)
#'
#' The function returns a SpatialLinesDataFrame object.
#' See \url{https://github.com/graphhopper} for more information.
#'
#' @param vehicle A text string representing the vehicle. Can be bike, bike2, car
#' or foot.
#'
#'
#' @details
#'
#' To test graphopper is working for you, try something like this, but with
#' your own API key:
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
#'
#' library(leaflet)
#'
#' leaflet() %>% addTiles() %>% addPolylines(data = r)
#' }

route_graphhopper <- function(from, to, vehicle = "bike"){
  if(!Sys.getenv('GRAPHHOPPER') == ""){
    key <- Sys.getenv('GRAPHHOPPER')
  }
  if(is.null(key)){
    stop("You must have a an api key saved as 'key'")
  }

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
                    "&locale=en-US&debug=true&points_encoded=false&key=", key)

  if(vehicle == "bike"){
    request <- paste0(request, "&elevation=true")
  }

  print(paste0("The request sent was: ", request))

  obj <- jsonlite::fromJSON(request)

  route <- SpatialLines(list(Lines(list(sp::Line(obj$paths$points[[1]][[1]][,1:2])), ID = "1")))

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

  route <- SpatialLinesDataFrame(route, df)

  route

}
