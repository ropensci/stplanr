#' Cubic logarithmic decay function
#'
#' @section Details:
#' Function for converting distance of a trip into the probability of travel by a particular mode.
#' #' @examples
#' d <- 0:10 # vector of distances
#' params <- c(0, -0.002, -0.15, -3)
#' (res <- log_cubic(d, c(0, -0.002, -0.15, -3)))
#' plot(d, res)
log_cubic <- function(d, par){
  log_d <- log(d)
  log_p <- par[1] * d^3 + par[2] * d^2 + par[3] * d + par[4]
  p <- exp(log_p)
  p
}

#' Estimate propensity to cycle
#'
#' Distance-decay function (Iacono et al. 2011)
#'
#' @section Details:
#' Implementation of the formula used to fit distance to probability of
#'  travelling by a given mode by Iacono et al. (2010)
#'
#' @references
#' Iacono, M., Krizek, K. J., & El-Geneidy, A. (2010).
#' Measuring non-motorized accessibility: issues, alternatives, and execution. Journal of Transport Geography, 18(1), 133–140. doi:10.1016/j.jtrangeo.2009.02.002
#'
iac <- function(x, a = 0.3, b = 0.2){
  a * exp(1)^(-b * x)
}
iac(1:10)
