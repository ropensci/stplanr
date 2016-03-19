#' Function which estimates the flow between points/zones using the 'radiation model'
#'
#' @param p A SpatialPoints dataframe, the first column of which contains it's unique ID
#' @param pop_var A character string representing the variable that corresponds
#' to the population of the zone/point
#' @export
#' @examples
#' data(cents)
#' plot(cents)
#' class(cents)
#' # Create test population to model flows
#' set.seed(2050)
#' cents$population <- runif(n = nrow(cents), min = 100, max = 1000)
#' flowlines_radiation <- od_radiation(cents, pop_var = "population")
#' plot(flowlines_radiation, lwd = flowlines_radiation$flow * 10)
od_radiation <- function(p, pop_var = "population"){
  l <- points2flow(p)
  l$flow <- NA
  for(i in 1:nrow(p)){
    for(j in 1:nrow(p)){
      if(i == j) next()
      m <- p[[pop_var]][i]
      n <- p[[pop_var]][j]
      # create circle the radius of which is the distance between i and j centered on i
      s_circle <- stplanr::buff_geo(p[i,], width = geosphere::distHaversine(p[i,], p[j,]))
      # select all points within the circle
      ps <- p[s_circle,]
      s <- sum(ps[[pop_var]])
      l$flow[l$O == p@data[i,1] & l$D == p@data[j,1]] <- # next: simini's famous 'flux' formula
        m * n / ((m + s) * (m + n + s))
    }
  }
  l
}
