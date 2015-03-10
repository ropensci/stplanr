#' log-linear distance-decay function (Iacono et al. 2011)
#'
#' @section Details:
#' Implementation of the formula used to fit distance to probability of
#'  travelling by a given mode by Iacono et al. (2010)
#'
#' @references
#' Iacono, M., Krizek, K. J., & El-Geneidy, A. (2010).
#' Measuring non-motorized accessibility: issues, alternatives, and execution. Journal of Transport Geography, 18(1), 133–140. doi:10.1016/j.jtrangeo.2009.02.002
#'
#' @examples
#' d <- 0:10 # vector of distances
#' params <- c(0, -0.002, -0.15, -3)
#' (res <- dd_iac(d, c(0, -0.002, -0.15, -3)))
#' plot(d, res)
#'
#' d = seq(0, 50, 0.1)
#' plot(d, dd_iac(d))
#' lines(d, dd_iac(d, b = 0.15))
dd_iac <- function(d, a = 0.3, b = 0.2){
  a * exp(-b * d)
}
dd_iac(1:10)

#' log-linear-square-root distance decay
#'
#' @section Details:
#' log-linear distance decay (Iacono et al., 2010) with an additional
#' square-root term to make the curve much more flexible. The additional
#' term (c in the function below) describes the short-term response:
#' positive c values result in a unimodal distance decay with a peak after d = 0;
#' negative c values lead to rapid decay in cycling under 2 km.
#'
#' @references
#' Iacono, M., Krizek, K. J., & El-Geneidy, A. (2010).
#' Measuring non-motorized accessibility: issues, alternatives, and execution. Journal of Transport Geography, 18(1), 133–140. doi:10.1016/j.jtrangeo.2009.02.002
#'
#' @examples
#' d <- 0:10 # vector of distances
#' (res <- dd_logsqr(d, 0.3, -0.2, 0.5)
#' plot(d, res)
#'
#' d = seq(0, 50, 0.1)
#' plot(d, dd_logsqr(d, 0.3, -0.2, -0.5))
#' lines(d, dd_logsqr(d, b = 0.15))
#'
dd_logsqr <- function(d, a, b, c = 0){
  exp(log(a) + b * d + c * d^0.5)
}
#' Cubic logarithmic decay function
#'
#' @section Details:
#' Function for converting distance of a trip into the probability of travel by
#' a particular mode.
#'
#' @examples
#' d <- 0:10 # vector of distances
#' params <- c(0, -0.002, -0.15, -3)
#' (res <- dd_logcub(d, c(0, -0.002, -0.15, -3)))
#' plot(d, res)
#'
#' par_male_urb <- c(0.0001945, -0.002273, -0.1489583, -2.923221)
#' par_female_urb <- c(-0.0017932, 0.0655261, -0.8396819, -2.892149)
#' d = seq(0, 50, 0.1)
#' plot(d, dd_logcub(d, par = par_male_urb), ylim = c(0, 0.1)) # test plots
#' lines(d, dd_logcub(d, par = par_female_urb), ylim = c(0, 0.1)) # test plots
dd_logcub <- function(d, par){
  log_d <- log(d)
  log_p <- par[1] * d^3 + par[2] * d^2 + par[3] * d + par[4]
  p <- exp(log_p)
  p
}