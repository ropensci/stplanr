#' Break up an sf object with LINESTRING geometry by vertex/nodes intersections
#'
#' This function breaks-up a single linestrings into multiple linestring at points
#' where vertices from other linestrings in the network intersect with vertices in the original linestring.
#' See [github.com/ropensci/stplanr/issues/282](https://github.com/ropensci/stplanr/issues/282) for details.
#'
#' @param rnet An sf LINESTRING object representing a route network.
#' @param breakup_internal_vertex_matches Should breaks be made at internal
#'   vertex matches? `TRUE` by default. Internal vertices are vertices (but not
#'   start or end points) of two or more different linestrings that meet at the
#'   same point.
#' @return The same sf LINESTRING object with more rows (the result of the
#'   splitting) when there are intersecting (and internal) vertices.
#' @export
#'
#' @examples
#' library(sf)
#' par(mar = rep(0, 4))
#'
#' # Check for roundabout
#' plot(rnet_roundabout$geometry, lwd = 2, col = rainbow(nrow(rnet_roundabout)))
#'
#' rnet_roundabout_clean <- rnet_breakup_vertices(rnet_roundabout)
#' plot(rnet_roundabout_clean$geometry, lwd = 2, col = rainbow(nrow(rnet_roundabout_clean)))
#' # Check for overpasses
#' plot(rnet_overpass$geometry, lwd = 2, col = rainbow(nrow(rnet_overpass)))
#'
#' rnet_overpass_clean <- rnet_breakup_vertices(rnet_overpass)
#' plot(rnet_overpass_clean$geometry, lwd = 2, col = rainbow(nrow(rnet_overpass_clean)))
#' # mapview(rnet_overpass_clean) # to see interactively
#' # Check for intersection with no node
#' plot(rnet_cycleway_intersection$geometry, lwd = 2,
#'      col = rainbow(nrow(rnet_cycleway_intersection)))
#'
#' rnet_cycleway_intersection_clean <- rnet_breakup_vertices(rnet_cycleway_intersection)
#' plot(rnet_cycleway_intersection_clean$geometry,
#'      lwd = 2, col = rainbow(nrow(rnet_cycleway_intersection_clean)))
rnet_breakup_vertices <- function(rnet, breakup_internal_vertex_matches = TRUE) {
  rnet_nodes <- sf::st_geometry(line2points(rnet))
  rnet_internal_vertexes <- sf::st_geometry(line2vertices(rnet))

  # For the first part of the procedure I don't need duplicated nodes or
  # duplicated vertexes so I can extract their unique values
  unique_rnet_nodes <- do.call("c", unique(rnet_nodes))
  unique_rnet_internal_vertexes <- do.call("c", unique(rnet_internal_vertexes))

  # Intersection between nodes and internal vertexes
  # The following code is the same as
  # intersection_point <- sf::st_intersection(unique_rnet_nodes, unique_rnet_internal_vertexes)
  # but faster since we are dealing only with points

  rbind_nodes_internal_vertexes <- rbind(unique_rnet_nodes, unique_rnet_internal_vertexes)
  index_intersection_points <- duplicated(rbind_nodes_internal_vertexes)

  if (any(index_intersection_points)) {

    intersection_points <- sf::st_as_sf(
      data.frame(rbind_nodes_internal_vertexes[index_intersection_points, , drop = FALSE]),
      coords = c("x_coords", "y_coords"),
      crs = sf::st_crs(rnet)
    )

    message("Splitting rnet object at the intersection points between nodes and internal vertexes")
    rnet_breakup_collection <- lwgeom::st_split(rnet, intersection_points$geometry)
    rnet_clean <- sf::st_collection_extract(rnet_breakup_collection, "LINESTRING")
  } else {
    rnet_clean <- rnet
  }

  # Split again at the duplicated internal vertexes
  rnet_internal_vertexes_duplicated <- rnet_internal_vertexes[duplicated(rnet_internal_vertexes)]

  if (length(rnet_internal_vertexes_duplicated) > 0 & breakup_internal_vertex_matches) {
    message("Splitting rnet object at the duplicated internal vertexes")
    rnet_breakup_collection <- lwgeom::st_split(rnet_clean, rnet_internal_vertexes_duplicated)
    rnet_clean <- sf::st_collection_extract(rnet_breakup_collection, "LINESTRING")
  }

  rnet_clean
}

