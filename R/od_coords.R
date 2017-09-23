#' General function to create a matrix representing origins and destinations
#'
#' This function takes a wide range of input data types (spatial lines, points or text strings)
#' and returns a matrix of coordinates representing origin (fx, fy) and destination (tx, ty) points.
#'
#' @param from An object representing origins
#' @param to An object representing destinations
#' @param l If from and to are empty, this can be a spatial line object represent desire lines
#' @export
#' @examples
#' od_coords(from = cents[1:3, ], to = cents[2:4, ]) # Spatial points
#' od_coords(cents_sf[1:3, ], cents_sf[2:4, ]) # sf points
#' od_coords("Bristol", "Gloucester")
#' # od_coords(c("Bristol", "Temple Meads"), c("Gloucester", "Bristol")) # not working
#' od_coords(l = flowlines)
#' # od_coords(l = flowlines_sf) # todo
od_coords <- function(from = NULL, to = NULL, l = NULL) {

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

    coord_matrix <- cbind(from, to)
    colnames(coord_matrix) <- c("fx", "fy", "tx", "ty")
  }

  coord_matrix

}
