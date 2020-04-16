#' Calculate rolling average gradient from elevation data at segment level
#'
#' @param elevations Elevations, e.g. those provided by the `cyclestreets` package
#' @param distances Distances, e.g. those provided by the `cyclestreets` package
#' @inheritParams route_rolling_average
#' @inheritParams route_rolling_diff
#' @export
#' @examples
#' r1 <- od_data_routes[od_data_routes$route_number == 2, ]
#' y <- r1$elevations
#' distances <- r1$distances
#' route_rolling_gradient(y, distances)
#' route_rolling_gradient(y, distances, n = 3)
#' route_rolling_gradient(y, distances, n = 4)
#' r1$elevations_diff_1 <- route_rolling_diff(y, lag = 1)
#' r1$rolling_gradient <- route_rolling_gradient(y, distances, n = 2)
#' r1$rolling_gradient3 <- route_rolling_gradient(y, distances, n = 3)
#' r1$rolling_gradient4 <- route_rolling_gradient(y, distances, n = 4)
#' d <- cumsum(r1$distances) - r1$distances / 2
#' diff_above_mean <- r1$elevations_diff_1 + mean(y)
#' par(mfrow = c(2, 1))
#' plot(c(0, cumsum(r1$distances)), c(y, y[length(y)]), ylim = c(80, 130))
#' lines(c(0, cumsum(r1$distances)), c(y, y[length(y)]))
#' points(d, diff_above_mean )
#' abline(h = mean(y))
#' rg = r1$rolling_gradient
#' rg[is.na(rg)] <- 0
#' plot(c(0, d), c(0, rg), ylim = c(0, 0.2))
#' points(c(0, d), c(0, r1$rolling_gradient3), col = "blue")
#' points(c(0, d), c(0, r1$rolling_gradient4), col = "grey")
#' par(mfrow = c(1, 1))
route_rolling_gradient <- function(elevations, distances, lag = 1, n = 2) {
  changes_elevation = route_rolling_diff(elevations, lag = lag)
  mean_distances = route_rolling_average(distances, n = n)
  changes_elevation / mean_distances
}

#' Return smoothed averages of vector
#'
#' This function calculates a simple rolling mean in base R.
#' It is useful for calculating route characteristics such as mean
#' distances of segments and changes in gradient.
#'
#' @param x Numeric vector to smooth
#' @param n The window size of the smoothing function.
#' The default, 3, will take the mean of values before, after and including
#' each value.
#' @export
#' @examples
#' y = od_data_routes$elevations[od_data_routes$route_number == 2]
#' y
#' route_rolling_average(y)
#' route_rolling_average(y, n = 1)
#' route_rolling_average(y, n = 2)
#' route_rolling_average(y, n = 3)
route_rolling_average <- function(x, n = 3) {
  as.numeric(stats::filter(x, rep(1 / n, n), sides = 2))
}

#' Return smoothed differences between vector values
#'
#' This function calculates a simple rolling mean in base R. It is useful for
#' calculating route characteristics such as mean distances of segments and
#' changes in gradient.
#'
#' @param x Numeric vector to smooth
#' @param lag The window size of the smoothing function. The default, 3, will take
#'   the mean of values before, after and including each value.
#' @export
#' @examples
#' r1 <- od_data_routes[od_data_routes$route_number == 2, ]
#' y <- r1$elevations
#' route_rolling_diff(y, lag = 1)
#' route_rolling_diff(y, lag = 2)
#' r1$elevations_diff_1 <- route_rolling_diff(y, lag = 1)
#' d <- cumsum(r1$distances) - r1$distances / 2
#' diff_above_mean <- r1$elevations_diff_1 + mean(y)
#' plot(c(0, cumsum(r1$distances)), c(y, y[length(y)]), ylim = c(80, 130))
#' lines(c(0, cumsum(r1$distances)), c(y, y[length(y)]))
#' points(d, diff_above_mean )
#' abline(h = mean(y))
route_rolling_diff = function(x, lag = 1) {
  abs(c(diff(x = x, lag = lag), rep(NA, lag)))
}
