#' Match two sets of lines based on similarity
#'
#' This function is a wrapper around gDistance that matches lines based on the Hausdorff distance
#'
#' @param l1 A spatial object
#' @param l2 A spatial object
#' @param threshold The threshold for a match - distances greater than this will not count as matches
#' @param return_sp Should the function return a spatial result (FALSE by default)
#' @export
#' @examples
#' x1 <- 2:4
#' x2 <- 3:5
#' match(x1, x2) # how the base function works
#' l1 <- flowlines[2:4, ]
#' l2 <- routes_fast[3:5, ]
#' (lmatches <- line_match(l1, l2)) # how the stplanr version works
#' l2matched <- l2[lmatches[!is.na(lmatches)], ]
#' plot(l1)
#' plot(l2, add = TRUE)
#' plot(l2matched, add = TRUE, col = "red") # showing matched routes
#' l2matched2 <- line_match(l1, l2, return_sp = TRUE)
#' identical(l2matched, l2matched2)
#' # decreasing the match likelihood via the threshold
#' line_match(l1, l2, threshold = 0.003)
line_match <- function(l1, l2, threshold = 0.01, return_sp = FALSE) {
  dist_mat <- rgeos::gDistance(l1, l2, byid = TRUE, hausdorff = TRUE)
  closest <- apply(dist_mat, 2, which.min)
  closest_values <- apply(dist_mat, 2, min)
  closest[closest_values > threshold] <- NA
  sel_na <- is.na(closest)
  if (return_sp) {
    l2matched <- l2[closest[!sel_na], ]
    match_num <- names(closest)[which(!sel_na)]
    if (is(l2, "SpatialLinesDataFrame")) {
      l2matched$match <- match_num
    } else {
      l2matched <- sp::SpatialLinesDataFrame(l2matched, data = data.frame(match_num), match.ID = FALSE)
    }
    return(l2matched)
  } else {
    return(unname(closest))
  }
}
