# See https://rdatatable.gitlab.io/data.table/articles/datatable-importing.html#data-table-in-imports-but-nothing-imported-1
.datatable.aware = TRUE
# FIXME
utils::globalVariables(c(".N", "linestring_id", "new_linestring_id"))

#' Break up an sf object with LINESTRING geometry.
#'
#' This function breaks up a LINESTRING geometry into multiple LINESTRING(s). It
#' is used mainly for preserving routability of an `sfNetwork` object that is
#' created using Open Street Map data. See details,
#' [stplanr/issues/282](https://github.com/ropensci/stplanr/issues/282), and
#' [stplanr/issues/416](https://github.com/ropensci/stplanr/issues/416).
#'
#' A LINESTRING geometry is broken-up when one of the two following conditions
#' are met:
#' 1. two or more LINESTRINGS share a POINT which is a boundary point for some
#' LINESTRING(s), but not all of them (see the rnet_roundabout example);
#' 2. two or more LINESTRINGS share a POINT which is not in the boundary of any
#' LINESTRING (see the rnet_cycleway_intersection example).
#'
#' The problem with the first example is that, according to algorithm behind
#' [SpatialLinesNetwork()], two LINESTRINGS are connected if and only if they
#' share at least one point in their boundaries. The roads and the roundabout
#' are clearly connected in the "real" world but the corresponding LINESTRING
#' objects do not share two distinct boundary points. In fact, by Open Street
#' Map standards, a roundabout is represented as a closed and circular
#' LINESTRING, and this implies that the roundabout is not connected to the
#' other roads according to [SpatialLinesNetwork()] definition. By the same
#' reasoning, the roads in the second example are clearly connected in the
#' "real" world, but they do not share any point in their boundaries. This
#' function is used to solve this type of problem.
#'
#' @param rnet An sf object with LINESTRING geometry representing a route
#'   network.
#' @param verbose Boolean. If TRUE, the function prints additional messages.
#' @return An sf object with LINESTRING geometry created after breaking up the
#'   input object.
#' @family rnet
#' @export
#'
#' @examples
#' library(sf)
#' def_par <- par(no.readonly = TRUE)
#' par(mar = rep(0, 4))
#'
#' # Check the geometry of the roundabout example. The dots represent the
#' # boundary points of the LINESTRINGS. The "isolated" red point in the
#' # top-left is the boundary point of the roundabout, and it is not shared
#' # with any other street.
#' plot(st_geometry(rnet_roundabout), lwd = 2, col = rainbow(nrow(rnet_roundabout)))
#' boundary_points <- st_geometry(line2points(rnet_roundabout))
#' points_cols <- rep(rainbow(nrow(rnet_roundabout)), each = 2)
#' plot(boundary_points, pch = 16, add = TRUE, col = points_cols, cex = 2)
#'
#' # Clean the roundabout example.
#' rnet_roundabout_clean <- rnet_breakup_vertices(rnet_roundabout)
#' plot(st_geometry(rnet_roundabout_clean), lwd = 2, col = rainbow(nrow(rnet_roundabout_clean)))
#' boundary_points <- st_geometry(line2points(rnet_roundabout_clean))
#' points_cols <- rep(rainbow(nrow(rnet_roundabout_clean)), each = 2)
#' plot(boundary_points, pch = 16, add = TRUE, col = points_cols)
#' # The roundabout is now routable since it was divided into multiple pieces
#' # (one for each colour), which, according to SpatialLinesNetwork() function,
#' # are connected.
#'
#' # Check the geometry of the overpasses example. This example is used to test
#' # that this function does not create any spurious intersection.
#' plot(st_geometry(rnet_overpass), lwd = 2, col = rainbow(nrow(rnet_overpass)))
#' boundary_points <- st_geometry(line2points(rnet_overpass))
#' points_cols <- rep(rainbow(nrow(rnet_overpass)), each = 2)
#' plot(boundary_points, pch = 16, add = TRUE, col = points_cols, cex = 2)
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
#' plot(
#'   rnet_cycleway_intersection$geometry,
#'   lwd = 2,
#'   col = rainbow(nrow(rnet_cycleway_intersection)),
#'   cex = 2
#' )
#' plot(st_geometry(line2points(rnet_cycleway_intersection)), pch = 16, add = TRUE)
#' # Check interactively
#' # mapview::mapview(rnet_overpass)
#'
#' # Clean the rnet object and plot the result.
#' rnet_cycleway_intersection_clean <- rnet_breakup_vertices(rnet_cycleway_intersection)
#' plot(
#'   rnet_cycleway_intersection_clean$geometry,
#'   lwd = 2,
#'   col = rainbow(nrow(rnet_cycleway_intersection_clean)),
#'   cex = 2
#' )
#' plot(st_geometry(line2points(rnet_cycleway_intersection_clean)), pch = 16, add = TRUE)
#'
#' par(def_par)

rnet_breakup_vertices <- function(rnet, verbose = FALSE) {
  # A few safety checks
  if (!inherits(rnet, "sf")) {
    stop(
      "At the moment this function was tested only using sf objects.",
      call. = FALSE
    )
  }
  if (!all(sf::st_is(rnet, "LINESTRING"))) {
    stop(
      "The input rnet must have LINESTRING geometry. See also ?sf::st_cast.",
      call. = FALSE
    )
  }
  if (!is.null(sf::st_z_range(rnet)) || !is.null(sf::st_m_range(rnet))) {
    warning(
      "The Z/M dimensions will be lost.",
      call. = FALSE,
      immediate. = TRUE
    )
  }

  # Check if we need to rebuild tbl_df at the end
  rebuild_tbl <- FALSE
  if (inherits(rnet, "sf") && inherits(rnet, "tbl_df")) {
    rebuild_tbl <- TRUE
  }

  # step 1 - Find points that are both boundary points and internal points.
  # 1a - Find internal points (i.e. points that are not in the boundary)
  internal_points <- data.table::data.table(
    sfheaders::sf_to_df(line2vertices(rnet))
  )
  data.table::setkey(internal_points, x, y)
  # I don't need duplicated pairs of coordinates
  internal_points <- unique(internal_points)
  if (verbose) {
    message("Extracted the (unique) internal points")
  }
  # 1b - Find points that lie in the boundaries
  boundary_points <- data.table::data.table(
    sfheaders::sf_to_df(rnet_boundary_unique(rnet))
  )
  data.table::setkey(boundary_points, x, y)
  if (verbose) {
    message("Extracted the (unique) boundary points")
  }

  # Consider only the points that are both internal and boundaries
  shared_internal_points_data_table <- boundary_points[
    # The following [ ] is used to filter the boundary points which are also
    # internal points. The operation checks that the x and y coordinates
    # are identical
    stats::na.omit(boundary_points[internal_points, which = TRUE]),
  ]

  if (nrow(shared_internal_points_data_table) > 0) {
    # step 2 - Split at shared internal points
    message("Splitting the input object at the shared internal points.")
    rnet <- my_st_split(rnet, shared_internal_points_data_table)
  }

  # step 3 - Find duplicated internal points
  internal_points <- data.table::data.table(
    sfheaders::sf_to_df(line2vertices(rnet))
  )
  data.table::setkey(internal_points, x, y)
  duplicated_internal_points <- internal_points[duplicated(internal_points)]

  # 4 - Split at duplicated internal points
  if (nrow(duplicated_internal_points) > 0) {
    message("Splitting rnet object at the duplicated internal points.")
    rnet <- my_st_split(rnet, duplicated_internal_points)
  }

  # 5 - Maybe we need to rebuild the tbl_df structure
  if (rebuild_tbl) {
    rnet <- sf::st_as_sf(dplyr::as_tibble(rnet))
  }

  rnet
}

# Not sure if it should be exported (maybe yes). In any case: ADD DOCS
# FIXME: ADD DOCS
my_st_split <- function(rnet, points) {
  # Extract coordinates from rnet and points
  # rnet must be an sf data.frame with LINESTRING geometry (no ZM) while points must be an
  # sfc with MULTIPOINT geometry
  rnet_coordinates <- data.table::data.table(sfheaders::sf_to_df(rnet))

  # Check Z/M dimensions
  if (any(c("z", "m") %in% colnames(rnet_coordinates))) {
    warning("The Z/M dimensions will be lost.", call. = FALSE, immediate. = TRUE)
  }

  # I cannot split a LINESTRING at a boundary point (since there is nothing "at
  # the other side" of the boundary point), so I need to find ID of all shared
  # internal points
  id_not_boundary <- rnet_coordinates[
    duplicated(linestring_id) & duplicated(linestring_id, fromLast = TRUE),
    which = TRUE
  ]
  id_shared <- rnet_coordinates[points, on = c("x", "y"), which = TRUE]
  id <- intersect(id_not_boundary, id_shared)

  # Now I need to duplicate the coordinates of the shared internal points (since
  # they will become boundary points for the new LINESTRING(s)). I use sort
  # since I want to preserve the order within the points.
  rnet_coordinates <- rnet_coordinates[sort(c(1:.N, id))]

  # I will build the new sfc LINESTRING using sfheaders::sfc_linestring so I
  # need to build an ID for each new LINESTRING. The ID for the new
  # LINESTRING(s) is created starting from the L1 column in rnet_coordinates and
  # incrementing the id by 1 at each break point.
  break_id <- double(nrow(rnet_coordinates))
  break_id[id + 1:length(id)] <- 1L # The ID is shifted by 1 at each break point
  break_id <- cumsum(break_id)
  rnet_coordinates$new_linestring_id <- rnet_coordinates$linestring_id + break_id

  # Now I can create the new LINESTRING sfc
  new_linestring <- sfheaders::sfc_linestring(
    obj = rnet_coordinates,
    x = "x",
    y = "y",
    linestring_id = "new_linestring_id"
  )

  # Exclude Z and/or M dimension
  attr(new_linestring, "z_range") <- NULL
  attr(new_linestring, "m_range") <- NULL

  # Determine the ID(s) of the old LINESTRING(s) (i.e. I need to create an sf
  # object with the new geometry column and the old fields)
  old_rnet_id <- rnet_coordinates[
    ,
    list(linestring_id = unique(linestring_id)),
    by = new_linestring_id
  ]$linestring_id

  # Create the new object:
  sf::st_sf(
    sf::st_drop_geometry(rnet)[old_rnet_id, , drop = FALSE],
    geometry = new_linestring,
    crs = sf::st_crs(rnet),
    agr = sf::st_agr(rnet),
    precision = sf::st_precision(rnet)
  )
}
