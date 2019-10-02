#' Cleanr a sf object with LINESTRING geometry such that
#'
#' @param rnet a sf LINESTRING object that ...
#'
#' @return sf object
#' @export
#'
#' @examples
#' library(sf)
#' par(mar = rep(0, 4))
#'
#' # Check for roundabout
#' plot(rnet_roundabout$geometry, lwd = 2, col = rainbow(nrow(rnet_roundabout)))
#'
#' rnet_roundabout_clean <- rnet_clean_vertices(rnet_roundabout)
#' plot(rnet_roundabout_clean$geometry, lwd = 2, col = rainbow(nrow(rnet_roundabout_clean)))
#' # Check for overpasses
#' plot(rnet_overpass$geometry, lwd = 2, col = rainbow(nrow(rnet_overpass)))
#'
#' rnet_overpass_clean <- rnet_clean_vertices(rnet_overpass)
#' plot(rnet_overpass_clean$geometry, lwd = 2, col = rainbow(nrow(rnet_overpass_clean)))
#' #' \donttest{
#' mapview::mapview(rnet_overpass_clean)
#' }
#' # Check for intersection with no node
#' plot(rnet_cycleway_intersection$geometry, lwd = 2, col = rainbow(nrow(rnet_cycleway_intersection)))
#'
#' rnet_cycleway_intersection_clean <- rnet_clean_vertices(rnet_cycleway_intersection)
#' plot(rnet_cycleway_intersection_clean$geometry, lwd = 2, col = rainbow(nrow(rnet_cycleway_intersection_clean)))
#'
#' # Bigger example
#' \donttest{
#' library(geofabric)
#' iow <- geofabric::get_geofabric("isle wight")
#'
#' key_roads_text = "primary|secondary|tertiary|cycleway|trunk|motorway"
#' iow_small <- iow[grepl(pattern = key_roads_text, x = iow$highway), ]
#' system.time(iow_clean <- rnet_clean_vertices(iow_small))
#'
#' plot(iow_small$geometry)
#' plot(iow_clean$geometry)
#' }

rnet_clean_vertices <- function(rnet) {
  rnet_nodes <- sf::st_geometry(line2points(rnet))
  rnet_internal_vertexes <- sf::st_geometry(line2vertices(rnet))

  # For the first part of the procedure I don't need duplicated nodes or
  # duplicated vertexes so I can extract their unique values
  unique_rnet_nodes <- do.call("c", unique(rnet_nodes))
  unique_rnet_internal_vertexes <- do.call("c", unique(rnet_internal_vertexes))

  # Intersection between nodes and internal vertexes
  intersection_point <- sf::st_intersection(unique_rnet_nodes, unique_rnet_internal_vertexes)

  if (length(intersection_point) > 0) {
    rnet_clean_collection <- lwgeom::st_split(rnet, intersection_point)
    rnet_clean <- sf::st_collection_extract(rnet_clean_collection, "LINESTRING")
  } else {
    rnet_clean <- rnet
  }

  # Split again at the duplicated internal vertexes
  rnet_internal_vertexes_duplicated <- rnet_internal_vertexes[duplicated(rnet_internal_vertexes)]

  if (length(rnet_internal_vertexes_duplicated) > 0) {
    rnet_clean_collection <- lwgeom::st_split(rnet, rnet_internal_vertexes_duplicated)
    rnet_clean <- sf::st_collection_extract(rnet_clean_collection, "LINESTRING")
  }

  rnet_clean
}
