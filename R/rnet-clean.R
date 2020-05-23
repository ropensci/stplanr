#' Break up an `sf` object with LINESTRING geometry.
#'
#' This function breaks up a single LINESTRING geometry into multiple
#' LINESTRING(s) for preserving routability of an `sfNetwork` object created
#' by [SpatialLinesNetwork()] function with Open Street Map data. See details
#' and [stplanr/issues/282](https://github.com/ropensci/stplanr/issues/282).
#'
#' A LINESTRING geometry is broken-up when one of the following conditions is
#' met:
#' 1. two or more LINESTRINGS share a POINT that lies in the union of their
#' boundaries (see the rnet_roundabout example);
#' 2. two or more LINESTRINGS share a POINT which is not in the boundary of any
#' LINESTRING (see the rnet_cycleway_intersection example).
#'
#' The problem with the first example is that, according to algorithm behind
#' [SpatialLinesNetwork()], two LINESTRINGS are connected if and only
#' if they share at least one point in their boundaries. The roads and the
#' roundabout are clearly connected in the "real" world but the corresponding
#' LINESTRING objects do not share any boundary point. In fact, by Open Street
#' Map standards, a roundabout is represented as a closed and circular
#' LINESTRING and this implies that the roundabout is not connected to the other
#' roads according to [SpatialLinesNetwork()] definition. By the same reasoning,
#' the roads in the second example are clearly connected in the "real" world,
#' but they do not share any point in their boundaries. This function is used to
#' solve this type of problem.
#'
#' @param rnet An sf object with LINESTRING geometry representing a route
#'   network.
#' @param breakup_internal_vertex_matches Boolean. Should breaks be made at
#'   shared internal points? `TRUE` by default. Internal points are points that
#'   do not lie in the boundary of the LINESTRING.
#' @return An sf object with LINESTRING geometry created after breaking up the
#'   input object.
#' @family rnet
#' @export
#'
#' @examples
#' library(sf)
#' def_par = par(no.readonly = TRUE)
#' par(mar = rep(0, 4))
#'
#' # Check the geometry of the roundabout example. The dots represent the
#' # boundary points of the LINESTRINGS. The "isolated" red point in the top-left
#' # is the boundary point of the roundabout, and it is not shared with any
#' # other street.
#' plot(st_geometry(rnet_roundabout), lwd = 2, col = rainbow(nrow(rnet_roundabout)))
#' boundary_points <- st_geometry(line2points(rnet_roundabout))
#' points_cols <- rep(rainbow(nrow(rnet_roundabout)), each = 2)
#' plot(boundary_points, pch = 16, add = TRUE, col = points_cols)
#'
#' # Clean the roundabout example.
#' rnet_roundabout_clean <- rnet_breakup_vertices(rnet_roundabout)
#' plot(st_geometry(rnet_roundabout_clean), lwd = 2, col = rainbow(nrow(rnet_roundabout_clean)))
#' boundary_points <- st_geometry(line2points(rnet_roundabout_clean))
#' points_cols <- rep(rainbow(nrow(rnet_roundabout_clean)), each = 2)
#' plot(boundary_points, pch = 16, add = TRUE, col = points_cols)
#' # The roundabout is now routable since it was divided into multiple pieces
#' # (one for each colour), which, according to SpatialLinesNetwork() function,
#' # are connected to the other streets.
#'
#' # Check the geometry of the overpasses example. This example is used to test
#' # that this function does not create any spurious intersection.
#' plot(st_geometry(rnet_overpass), lwd = 2, col = rainbow(nrow(rnet_overpass)))
#' boundary_points <- st_geometry(line2points(rnet_overpass))
#' points_cols <- rep(rainbow(nrow(rnet_overpass)), each = 2)
#' plot(boundary_points, pch = 16, add = TRUE, col = points_cols)
#' # At the moment the network is not routable since one of the underpasses is
#' # not connected to the other streets.
#'
#' # Check interactively.
#' # mapview::mapview(rnet_overpass)
#'
#' # Clean the network. It should not create any spurious intersection between
#' # roads located at different heights.
#' rnet_overpass_clean <- rnet_breakup_vertices(rnet_overpass)
#' plot(st_geometry(rnet_overpass_clean), lwd = 2, col = rainbow(nrow(rnet_overpass_clean)))
#' # Check interactively.
#' # mapview::mapview(rnet_overpass)
#'
#' # Check the geometry of the cycleway_intersection example. The black dots
#' # represent the boundary points and we can see that the two roads are not
#' # connected according to SpatialLinesNetwork() function.
#' plot(rnet_cycleway_intersection$geometry, lwd = 2,
#'      col = rainbow(nrow(rnet_cycleway_intersection)))
#' plot(st_geometry(line2points(rnet_cycleway_intersection)), pch = 16, add = TRUE)
#' # Check interactively
#' # mapview::mapview(rnet_overpass)
#'
#' # Clean the rnet object and plot the result.
#' rnet_cycleway_intersection_clean <- rnet_breakup_vertices(rnet_cycleway_intersection)
#' plot(rnet_cycleway_intersection_clean$geometry,
#'      lwd = 2, col = rainbow(nrow(rnet_cycleway_intersection_clean)))
#' plot(st_geometry(line2points(rnet_cycleway_intersection_clean)), pch = 16, add = TRUE)
#'
#' par(def_par)

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

    message("Splitting rnet object at the shared boundary points.")
    rnet_breakup_collection <- lwgeom::st_split(rnet, intersection_points$geometry)
    rnet_clean <- sf::st_collection_extract(rnet_breakup_collection, "LINESTRING")
  } else {
    rnet_clean <- rnet
  }

  # Split again at the duplicated internal vertexes
  rnet_internal_vertexes_duplicated <- rnet_internal_vertexes[duplicated(rnet_internal_vertexes)]

  if (length(rnet_internal_vertexes_duplicated) > 0 & breakup_internal_vertex_matches) {
    message("Splitting rnet object at the shared internal points.")
    rnet_breakup_collection <- lwgeom::st_split(rnet_clean, rnet_internal_vertexes_duplicated)
    rnet_clean <- sf::st_collection_extract(rnet_breakup_collection, "LINESTRING")
  }

  rnet_clean
}

