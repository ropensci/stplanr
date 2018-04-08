#' General function to create a matrix representing origins and destinations
#'
#' This function takes a wide range of input data types (spatial lines, points or text strings)
#' and returns a matrix of coordinates representing origin (fx, fy) and destination (tx, ty) points.
#'
#' @param from An object representing origins
#' (if lines are provided as the first argument, from is assigned to \code{l})
#' @param to An object representing destinations
#' @param l Only needed if from and to are empty, in which case this
#' should be a spatial object representing desire lines
#' @export
#' @examples
#' od_coords(from = c(0, 52), to = c(1, 53)) # lon/lat coordinates
#' od_coords(from = cents[1, ], to = cents[2, ]) # Spatial points
#' od_coords(cents_sf[1:3, ], cents_sf[2:4, ]) # sf points
#' # od_coords("Hereford", "Leeds") # geocode locations
#' od_coords(flowlines[1:3, ])
#' od_coords(flowlines_sf[1:3, ])
od_coords <- function(from = NULL, to = NULL, l = NULL) {

  if(is(object = from, class2 = "sf")) {
    is_sf_line <- all(sf::st_geometry_type(from) == "LINESTRING")
  } else {
    is_sf_line <- FALSE
  }

  if(is_sf_line | any(grepl(pattern = "Line", x = class(from)))) {
    l <- from
  }

  if(!is.null(l)) {
    coord_matrix <- line2df(l) %>%
      dplyr::select("fx", "fy", "tx", "ty")
  }

  else {
    # Convert sp object to lat/lon vector
    if(is(object = from, "Spatial")) from <- sp::coordinates(from)
    if(is(object = to, "Spatial")) to <- sp::coordinates(to)

    # sf objects
    if(is(object = from, "sf")) from <- sf::st_coordinates(from)
    if(is(object = to, "sf")) to <- sf::st_coordinates(to)

    # Convert character strings to lon/lat if needs be
    if(is.character(from)) from <- matrix(geo_code(from), ncol = 2)
    if(is.character(to)) to <- matrix(geo_code(to), ncol = 2)
    if(is.vector(from) & is.vector(to)) {
      coord_matrix = matrix(c(from, to), ncol = 4)
    } else {
      coord_matrix <- cbind(from, to)
    }
    colnames(coord_matrix) <- c("fx", "fy", "tx", "ty")
  }

  coord_matrix

}
