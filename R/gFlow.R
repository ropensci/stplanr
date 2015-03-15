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
  l <- SpatialLines(l)
  l <- SpatialLinesDataFrame(l, data = flow, match.ID = F)
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
#' data(l)
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
    obj <- getURL(request)
    writeLines(obj, "/tmp/obj.geojson")
    obj <- readLines("/tmp/obj.geojson")
    just_lines <- obj[14:(length(obj) - 28)]
    just_lines[1] <- paste0("{",  just_lines[1])
    just_lines[length(just_lines)] <- "}"
    writeLines(just_lines, "/tmp/just_lines.geojson")
    route <- readOGR("/tmp/just_lines.geojson", layer = "OGRGeoJSON")
    spChFIDs(route) <- i
    route@data <- cbind(route@data, l@data[i, ])
    route$line_num <- row.names(l[i,])
    if(i == 1){
      output <- route
    }
    else{
      output <- spRbind(output, route)
    }
  }
  output
}
