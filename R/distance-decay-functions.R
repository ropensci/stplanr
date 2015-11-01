#' log-linear distance-decay function
#'
#' 'log-linear' distance decay function with two parameters:
#' implementation of the formula used to fit distance to probability of
#' travelling by a given mode by Iacono et al. (2010).
#'
#' @param x A positive vector representing distances (often in km)
#' @param a Positive number (usually between 0 and 1) representing
#'   the intercept of the distance decay curve with the y axis, when x = 0.
#' @param b1 Number representing the 'beta' distance decay parameter.
#' Larger negative values make the initial decay steeper.
#'
#' @references
#' Iacono, M., Krizek, K. J. and El-Geneidy, A. (2010).
#' Measuring non-motorized accessibility: issues, alternatives, and execution. Journal of Transport Geography, 18(1).
#' @export
#' @examples
#' x <- 0:10 # vector of distances
#' a = 0.3 # default alpha value
#' b = 0.2 # default beta value
#' (res <- dd_loglin(x, a, b))
#' plot(x, res)
#'
#' x = seq(0, 50, 0.1)
#' plot(x, dd_loglin(x))
#' lines(x, dd_loglin(x, a = 0.1, b1 = 0.15))
dd_loglin <- function(x, a = 0.3, b1 = -0.2){
  a * exp(b1 * x)
}

#' log-linear-square-root distance decay
#'
#' @param x A positive vector representing distances (often in km)
#' @param a Positive number (usually between 0 and 1) representing
#'   the intercept of the distance decay curve with the y axis, when x = 0.
#' @param b1 Number representing the 'beta' distance decay parameter.
#' Higher values make the decay steeper.
#' @param b2 Number representing the second 'beta' distance decay parameter.
#' Higher values make the decay steeper.
#'
#' log-linear distance decay (Iacono et al., 2010) with an additional
#' square-root term to make the curve more flexible. The additional
#' term (b2 in the function below) describes the short-term response:
#' positive b2 values result in a unimodal distance decay
#' with a peak after x = 0;
#' negative b2 values lead to rapid decay in cycling under 2 km.
#'
#' @references
#'
#' Iacono, M., Krizek, K. J. and El-Geneidy, A. (2010).
#' Measuring non-motorized accessibility: issues, alternatives, and execution. Journal of Transport Geography, 18(1).
#' @export
#' @examples
#' x = seq(0, 50, 0.1)
#' plot(x, dd_logsqrt(x, a = 0.3, b1 = -0.2, b2 = -0.5), ylim = c(0, 0.5))
#' lines(x, dd_logsqrt(x, a = 0.3, b1 = -0.2, b2 = 0.5))
dd_logsqrt <- function(x, a, b1, b2){
  a_log <- log(a)
  log_p <- a_log + b1 * x + b2 * x^0.5
  p <- exp(log_p)
  p
}

#' Cubic logarithmic decay function
#'
#' Function for converting distance of a trip into the probability of travel by
#' a particular mode.
#'
#' @param x A positive vector representing distances (often in km)
#' @param a Positive number (usually between 0 and 1) representing
#'   the intercept of the distance decay curve with the y axis, when x = 0.
#' @param b1 The linear term of distance decay
#' @param b2 The square term of distance decay
#' @param b3 The cubic term of distance decay - should be negative to converge to 0
#' @export
#' @examples
#' x <- 0:10 # vector of distances
#' (res <- dd_logcub(x = x, a = 0.3, b1 = 0.003, b2 = -0.002, b3 = -0.001))
#' plot(x, res)
#'
#' p1 <- c(0.05, -0.1489583, -0.002273, 0.0001945) # male distance decay
#' p2 <- c(0.10, -0.8396819, 0.0655261, -0.0017932) # female distance decay
#' x = seq(0, 20, 0.1)
#' ym <- dd_logcub(x, p1[1], p1[2], p1[3], p1[4])
#' yf <- dd_logcub(x, p2[1], p2[2], p2[3], p2[4])
#' plot(x, ym, ylim = c(0, 0.1)) # test plots
#' lines(x, yf) # test plots
dd_logcub <- function(x, a, b1, b2, b3){
  a_log <- log(a)
  log_d <- log(x)
  log_p <- a_log + b1 * x + b2 * x^2 + b3 * x^3
  p <- exp(log_p)
  p
}
