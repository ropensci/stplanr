#' Sample integer number from given continuous vector of line lengths and probabilities, with total n
#'
#' @param n Sum of integer values returned
#' @param l_lengths Numeric vector of line lengths
#' @param weights Relative probabilities of samples on lines
#' @family lines
#' @export
#' @examples
#' n <- 10
#' l_lengths <- 1:5
#' weights <- 9:5
#' (res <- n_sample_length(n, l_lengths, weights))
#' sum(res)
#' n <- 100
#' l_lengths <- c(12, 22, 15, 14)
#' weights <- c(38, 10, 44, 34)
#' (res <- n_sample_length(n, l_lengths, weights))
#' sum(res)
#' # more examples:
#' n_sample_length(5, 1:5, c(0.1, 0.9, 0, 0, 0))
#' n_sample_length(5, 1:5, c(0.5, 0.3, 0.1, 0, 0))
#' l <- flowlines[2:6, ]
#' l_lengths <- line_length(l)
#' n <- n_sample_length(10, l_lengths, weights = l$All)
n_sample_length <- function(n, l_lengths, weights) {
  # generate length-adjusted weights equal to 1
  l_lengths_rel <- l_lengths * weights / (sum(l_lengths * weights))
  n_vec <- round(n * l_lengths_rel)
  if (sum(n_vec) != n) {
    n_diff <- n - sum(n_vec)
    probs <- l_lengths_rel - (n_vec / n) # how much less of each is needed
    if (n_diff < 0) {
      probs <- probs * -1
    }
    probs[probs < 0] <- 0
    sel <- sample(length(l_lengths), size = abs(n_diff), prob = probs)
    n_vec[sel] <- n_vec[sel] + sign(n_diff)
  }
  n_vec
}

#' Sample n points along lines with density proportional to a weight
#'
#' @param n The total number of points to sample
#' @param l The SpatialLines object along which to create sample points
#' @param weights The relative probabilities of lines being samples
#' @family lines
#' @export
#' @examples
#' l <- flowlines[2:5, ]
#' n <- 100
#' l_lengths <- line_length(l)
#' weights <- l$All
#' p <- line_sample(l, 50, weights)
#' plot(p)
#' p <- line_sample(l, 50, weights = 1:length(l))
#' plot(p)
line_sample <- function(l, n, weights) {
  not_projected <- !sp::is.projected(l)
  if (not_projected) {
    crs_orig <- sp::proj4string(l)
    if (is.na(crs_orig)) {
      crs_orig <- sp::CRS("+init=epsg:4326")
    }
    crs_new <- geo_select_aeq(l)
    l <- sp::spTransform(l, CRSobj = crs_new)
  }
  lsf <- sf::st_as_sf(l, "SpatialLinesDataFrame")
  l_lengths <- sf::st_length(lsf)
  l_lengths <- as.vector(l_lengths) # convert to numeric vector
  n_vec <- n_sample_length(n, l_lengths, weights = weights)
  psf <- sf::st_line_sample(lsf, n = n_vec)

  # aim: group point collection into points - to update if possible
  # class(psf) # its a MULIPOINT
  psf_point <- as(psf, "Spatial")
  psp <- as(psf, "Spatial")
  psp <- sp::SpatialPoints(matrix(sp::coordinates(psp), ncol = 2), proj4string = sp::CRS(proj4string(psp)))
  if (not_projected) {
    psp <- sp::spTransform(psp, crs_orig)
  }
  return(psp)
}
