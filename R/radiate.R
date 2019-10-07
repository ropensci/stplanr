#' Function that estimates flow between points or zones using the radiation model
#'
#' This is an implementation of the radiation model proposed in a paper
#' by Simini et al. (2012).
#'
#' @param p A SpatialPoints dataframe, the first column of which contains a unique ID
#' @param pop_var A character string representing the variable that corresponds
#' to the population of the zone or point
#' @param proportion A number representing the proportion of the population who
#' commute (1, the default, means 100 percent of the population commute to work)
#' @references
#' Simini, F., Gonzalez, M.C., Maritan, A., Barabasi, A.L., 2012. A universal model for
#' mobility and migration patterns. Nature. doi:10.1038/nature10856
#' @family od
#' @export
#' @examples
#' # load some points data
#' data(cents)
#' # plot the points to check they make sense
#' plot(cents)
#' class(cents)
#' # Create test population to model flows
#' set.seed(2050)
#' cents$population <- runif(n = nrow(cents), min = 100, max = 1000)
#' # estimate
#' flowlines_radiation <- od_radiation(cents, pop_var = "population")
#' flowlines_radiation$flow
#' sum(flowlines_radiation$flow, na.rm = TRUE) # the total flow in the system
#' sum(cents$population) # the total inter-zonal flow
#' plot(flowlines_radiation, lwd = flowlines_radiation$flow / 100)
#' points(cents, cex = cents$population / 100)
od_radiation <- function(p, pop_var = "population", proportion = 1) {
  l <- points2flow(p)
  l$flow <- NA
  for (i in 1:nrow(p)) {
    for (j in 1:nrow(p)) {
      if (i == j) next()
      m <- p[[pop_var]][i]
      n <- p[[pop_var]][j]
      sel_flow <- which(l$O == p@data[i, 1] & l$D == p@data[j, 1])
      # create circle the radius of which is the distance between i and j centered on i
      radius <- gprojected(shp = l[sel_flow, ], fun = rgeos::gLength)
      s_circle <- geo_buffer(shp = p[i, ], width = radius)
      ps <- p[-c(i, j), ][s_circle, ]
      s <- sum(ps[[pop_var]])
      l$flow[sel_flow] <-
        p[[pop_var]][i] * proportion * ((m * n) / ((m + s) * (m + n + s)))
    }
  }
  l
}
