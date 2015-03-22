#' Convert flow data to SpatialLinesDataFrame
#'
#' @section Details:
#' Origin-destination ('OD') flow data is often provided
#' in the form of 1 line per flow with zone codes of origin and destination
#' centroids. This can be tricky to plot and link-up with geographical data.
#' This function makes the task easier.
#'
#' @references
#' Rae, A. (2009). From spatial interaction data to spatial interaction information? Geovisualisation and spatial structures of migration from the 2001 UK census. Computers, Environment and Urban Systems, 33(3), 161–178. doi:10.1016/j.compenvurbsys.2009.01.007
#'
#'
gFlow2line <- function(flow, zones){
  l <- vector("list", nrow(flow))
  for(i in 1:nrow(flow)){
    from <- zones@data[,1] %in% flow[i, 1]
    to <- zones@data[,1] %in% flow[i, 2]
    x <- coordinates(zones[from, ])
    y <- coordinates(zones[to, ])
    l[[i]] <- Lines(list(Line(rbind(x, y))), as.character(i))
  }
  l <- sp::SpatialLines(l)
  l <- sp::SpatialLinesDataFrame(l, data = flow, match.ID = F)
  l
}

#' Convert straight SpatialLinesDataFrame from flow data into routes in the UK
#' using the CycleStreets.net API.
#'
#' @section Details:
#'
#' You will need to have a cyclestreets api key for this function to work, e.g.:
#'
#'\code{
#' mytoken <- readLines("~/Dropbox/dotfiles/cyclestreets-api-key-rl")
#' Sys.setenv(CYCLESTREET = mytoken) # see http://www.cyclestreets.net/api/
#' cckey <- Sys.getenv('CYCLESTREET')
#' }
#' @param plan A text string. Must be either "balanced", "fastest" (default)
#' or "quietest"
#'
#' @param l A SpatialLinesDataFrame object composed of straight lines. l may be
#' created using the \code{\link{gFlow2line}} function.
#'
#' @examples
#' v = data(package = "stplanr")
#' data(list = v$results[,3])
#' plot(l)
#'
#' \dontrun{
#' Sys.setenv(CYCLESTREET = mytoken) # see http://www.cyclestreets.net/api/
#' cckey <- Sys.getenv('CYCLESTREET')
#' routes_fast <- gLines2CyclePath(l)
#' routes_slow <- gLines2CyclePath(l, "quietest")
#' }
#' plot(routes_fast, col = "red", add = TRUE) # previously saved from l
#' plot(routes_slow, col = "green", add = TRUE)
gLines2CyclePath <- function(l, plan = "fastest"){
  if(is.null(cckey)) stop("You must have a CycleStreets.net api key saved as 'cckey'")
  coord_list <- lapply(slot(l, "lines"), function(x) lapply(slot(x, "Lines"),
    function(y) slot(y, "coords")))
  output <- vector("list", length(coord_list))
  api_base <- sprintf("https://%s@api.cyclestreets.net/v2/", cckey)
  for(i in 1:length(output)){
    from <- coord_list[[i]][[1]][1, ]
    to <- coord_list[[i]][[1]][2, ]
    from_string <- paste(from, collapse = ",")
    to_string <- paste(to, collapse = ",")
    ft_string <- paste(from_string, to_string, sep = "|")
    journey_plan <- sprintf("journey.plan?waypoints=%s&plan=%s", ft_string, plan)
    request <- paste0(api_base, journey_plan)

    # Thanks to barry Rowlingson for this part:
    obj <- jsonlite::fromJSON(request)
    route <- SpatialLines(list(Lines(list(Line(obj$features[1,]$geometry$coordinates[[1]])), ID = row.names(l[i,]))))
    df <- obj$features[1,]$properties
    row.names(df) <- row.names(l[i,])
    route <- SpatialLinesDataFrame(route, df)

    # Status checker: % downloaded
    if(i == 10)
      print("The first 10 routes have been saved, be patient. I'll say when 10% have been loaded.")
    if(i %% round(nrow(l) / 10) == 0)
      print(paste0(round(100 * i/nrow(flow)), " % out of ", nrow(flow),
        " distances calculated")) # print % of distances calculated

    if(i == 1){
      output <- route
    }
    else{
      output <- maptools::spRbind(output, route)
    }
  }
  proj4string(output) <- CRS("+init=epsg:4326")
  output
}
