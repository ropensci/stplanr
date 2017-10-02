#' Convert function on sp data to take sf data in and convert to sf
#'
#' @param input Input object - an sf or sp object
#' @param FUN A function that works on sp data
#' @param ... Arguments passed to \code{FUN}
as_sf_fun <- function(input, FUN, ...) {
  if(is(object = input, class2 = "sf")) {
    input <- as(object = input, Class = "Spatial")
  }
  res <- FUN(input)
  if(is(object = res, class2 = "Spatial")) {
    res <- sf::st_as_sf(res)
  }
  return(res)
}

#' Convert function on sf data to take sp data in and convert to sp
#'
#' @param input Input object - an sf or sp object
#' @param FUN A function that works on sf data
#' @param ... Arguments passed to \code{FUN}
as_sf_fun <- function(input, FUN, ...) {
  if(is(object = input, class2 = "Spatial")) {
    input <- sf::st_as_sf(input)
  }
  res <- FUN(input)
  if(is(object = res, class2 = "sf")) {
    res <- as(res, "Spatial")
  }
  return(res)
}