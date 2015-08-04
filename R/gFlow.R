#' Convert flow data to SpatialLinesDataFrame
#'
#' @section Details:
#' Origin-destination ('OD') flow data is often provided
#' in the form of 1 line per flow with zone codes of origin and destination
#' centroids. This can be tricky to plot and link-up with geographical data.
#' This function makes the task easier.
#'
#' @param flow A data frame representing the flow between two points
#' or zones. The first two columns of this data frame should correspond
#' to the first column of the data in the zones. Thus in \code{\link{cents}},
#' the first column is geo_code. This corresponds to the first two columns
#' of \code{\link{flow}}.
#' @param zones A SpatialPolygonsDataFrame or SpatialPointsDataFrame
#' representing origins and destinations of travel flows.
#' @references
#' Rae, A. (2009). From spatial interaction data to spatial interaction information? Geovisualisation and spatial structures of migration from the 2001 UK census. Computers, Environment and Urban Systems, 33(3). doi:10.1016/j.compenvurbsys.2009.01.007
#' @import sp
#' @export
#' @examples \dontrun{
#' data(flow) # load data frame of od flows between zones
#' data(cents) # load centroids data
#' newflowlines <- gFlow2line(flow = flow, zones = cents)
#' plot(cents)
#' lines(newflowlines)
#' }
gFlow2line <- function(flow, zones){
  l <- vector("list", nrow(flow))
  for(i in 1:nrow(flow)){
    from <- zones@data[,1] %in% flow[i, 1]
    to <- zones@data[,1] %in% flow[i, 2]
    x <- sp::coordinates(zones[from, ])
    y <- sp::coordinates(zones[to, ])
    l[[i]] <- sp::Lines(list(Line(rbind(x, y))), as.character(i))
  }
  l <- sp::SpatialLines(l)
  l <- sp::SpatialLinesDataFrame(l, data = flow, match.ID = F)
  proj4string(l) <- proj4string(zones)
  l
}

#' Convert straight SpatialLinesDataFrame from flow data into routes in the UK
#' using the CycleStreets.net API.
#'
#' @section Details:
#'
#' You will need to have a cyclestreets api key for this function to work, e.g.:
#'
#' \code{
#' mytoken <- "f3fe3d078ac34737" # warning: not a real key
#' }
#'
#' \code{
#' Sys.setenv(CYCLESTREET = mytoken) # see http://www.cyclestreets.net/api/
#' }
#'
#' \code{
#' cckey <- Sys.getenv('CYCLESTREET')
#' }
#'
#' @param plan A text string. Must be either "balanced", "fastest" (default)
#' or "quietest"
#'
#' @param l A SpatialLinesDataFrame object composed of straight lines. l may be
#' created using the \code{\link{gFlow2line}} function.
#'
#' @inheritParams route_cyclestreet
#'
#' @export
#'
#' @examples
#' library(rgdal)
#' data(flowlines) # load demo flowlines dataset
#' flowlines_84 <- sp::spTransform(flowlines, CRS("+init=epsg:4326"))
#' plot(flowlines_84)
#'
#' \dontrun{
#' # set cyclestreets api key: see http://www.cyclestreets.net/api/
#' cckey <- "f3fe3d078ac34737" # example key (not real)
#' # plot save the routes
#'
#' routes_fast <- gLines2CyclePath(flowlines_84)
#' routes_slow <- gLines2CyclePath(flowlines_84, "quietest")
#' }
#' if(!exists("routes_fast")){
#'   data(routes_fast, routes_slow) # load routes
#' }
#' lines(routes_fast, col = "red")
#' lines(routes_slow, col = "green")
#' # Plot for a single line to compare 'fastest' and 'quietest' route
#' n = 20
#' plot(flowlines_84[n,])
#' lines(routes_fast[n,], col = "red")
#' lines(routes_slow[n,], col = "green")

gLines2CyclePath <- function(l, plan = "fastest"){
  if(!Sys.getenv('CYCLESTREET') == ""){
    cckey <- Sys.getenv('CYCLESTREET')
  }
  if(is.null(cckey)){
    stop("You must have a CycleStreets.net api key saved as 'cckey'")
  }
  coord_list <- lapply(slot(l, "lines"), function(x) lapply(slot(x, "Lines"),
    function(y) slot(y, "coords")))
  output <- vector("list", length(coord_list))
  api_base <- sprintf("https://api.cyclestreets.net/v2/")
  for(i in 1:length(output)){
    from <- coord_list[[i]][[1]][1, ]
    to <- coord_list[[i]][[1]][2, ]

    if(identical(from, to)){
      # if the length is zero...
      route <-
        sp::SpatialLines(list(Lines(list(Line(rbind(from, to))),
          row.names(l[i,]))))
      df <- data.frame(matrix(NA, ncol = 6))
      names(df) <- c("plan", "start", "finish", "length", "time", "waypoint")
      row.names(df) <- row.names(l[i,])
      route <- SpatialLinesDataFrame(route, df)
    }else{
      # save lines that have a distance
      route <- route_cyclestreet(from, to, plan = plan, silent = TRUE)
    }
    sp::spChFIDs(route) <- i

    # Status checker: % downloaded
    if(i == 10)
      print("The first 10 routes have been saved, be patient. I'll say when 10% have been loaded.")
    perc_temp <- i %% round(nrow(l) / 10)
    if(!is.na(perc_temp) & perc_temp == 0){
      print(paste0(round(100 * i/nrow(l)), " % out of ", nrow(l),
        " distances calculated")) # print % of distances calculated
    }

    if(i == 1){
      output <- route
    }else{
      output <- maptools::spRbind(output, route)
    }
  }
  proj4string(output) <- CRS("+init=epsg:4326")

  output

}
