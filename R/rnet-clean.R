# See https://rdatatable.gitlab.io/data.table/articles/datatable-importing.html#data-table-in-imports-but-nothing-imported-1
.datatable.aware = TRUE
utils::globalVariables(c("linestring_id", "new_linestring_id"))

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
#' @param rnet An sf or sfc object with LINESTRING geometry representing a route
#'   network.
#' @param verbose Boolean. If TRUE, the function prints additional messages.
#' @return An sf or sfc object with LINESTRING geometry created after breaking
#'   up the input object.
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
  if (!inherits(rnet, c("sf", "sfc"))) {
    stop(
      "Sorry, at the moment this function works only with sf and sfc objects",
      call. = FALSE
    )
  }
  if (!all(sf::st_is(rnet, "LINESTRING"))) {
    stop(
      "The input object must have LINESTRING geometry. See also ?sf::st_cast.",
      call. = FALSE
    )
  }

  # Check if we need to rebuild the tbl_df structure at the end
  rebuild_tbl <- FALSE
  if (inherits(rnet, "sf") && inherits(rnet, "tbl_df")) {
    rebuild_tbl <- TRUE
  }

  # Step 1 - We need to split all LINESTRING(s) using the points that are both
  # duplicated points and not in the boundary

  # 1a - Extract all points and convert to data.table format
  rnet_points <- data.table::data.table(
    sfheaders::sfc_to_df(sf::st_geometry(rnet))
  )
  # The existing_dimensions vector is used to perform the following operations using x,y; x,y,z and so on.
  existing_dimensions <- intersect(c("x", "y", "z", "m"), colnames(rnet_points))

  # 1b - Extract id of boundary points
  id_boundary_points <- rnet_points[
    !duplicated(linestring_id) | !duplicated(linestring_id, fromLast = TRUE),
    which = TRUE
  ]
  if (verbose) {
    message("Extracted the ID(s) of the boundary points")
  }

  # 1c - Subset all duplicated points
  id_duplicated_points <- rnet_points[
    duplicated(rnet_points, by = existing_dimensions) |
    duplicated(rnet_points, by = existing_dimensions, fromLast = TRUE),
    which = TRUE
  ]
  if (verbose) {
    message("Extracted the ID(s) of the duplicated points")
  }

  # 1d - Anti-join to find the set difference between duplicated_points and
  # boundary_points
  id_split_points <- setdiff(id_duplicated_points, id_boundary_points)

  if (length(id_split_points) == 0L) {
    message(
      "The input data doesn't have any duplicated point which is also a boundary point.",
      "Return the input data."
    )
    return(rnet)
  }

  # Now I need to duplicate the coordinates of the internal points that are also
  # boundary points (since they will become boundary points for the new
  # LINESTRING(s)). I use sort since I want to preserve the order within the
  # points.
  rnet_points <- rnet_points[
    sort(c(seq_len(nrow(rnet_points)), id_split_points))
  ]

  # I will build the new LINESTRING using sfheaders::sfc_linestring so I need to
  # build an ID for each new LINESTRING. The ID for the new LINESTRING(s) is
  # created starting from the linestring_id column in rnet_points and
  # incrementing the id by 1 at each break point.
  break_id <- double(nrow(rnet_points))
  # The ID is shifted by 1 at each break point
  break_id[id_split_points + 1:length(id_split_points)] <- 1L
  break_id <- cumsum(break_id)
  rnet_points[["new_linestring_id"]] <- rnet_points[["linestring_id"]] + break_id

  # Now I can create the new LINESTRING sfc
  keep_columns <- c(existing_dimensions, "new_linestring_id")
  new_linestring <- sfheaders::sfc_linestring(
    rnet_points[, keep_columns, with = FALSE],
    linestring_id = "new_linestring_id"
  )

  # Add CRS and precision
  new_linestring_sfc <- sf::st_sfc(
    new_linestring,
    crs = sf::st_crs(rnet),
    precision = sf::st_precision(rnet)
  )

  # If the input is sfc then I just need to return new_linestring_sfc, otherwise
  # I have to rebuild the sf structure.
  if (!inherits(rnet, "sf")) {
    return(new_linestring_sfc)
  }

  # Determine the ID(s) of the old LINESTRING(s) (i.e. I need to create an sf
  # object with the new geometry column and the old fields)
  old_rnet_id <- rnet_points[
    j = list(linestring_id = unique(linestring_id)),
    by = new_linestring_id
  ][["linestring_id"]]

  rnet <- sf::st_sf(
    sf::st_drop_geometry(rnet)[old_rnet_id, , drop = FALSE],
    geometry = new_linestring_sfc,
    agr = sf::st_agr(rnet)
  )

  # 5 - Maybe we need to rebuild the tbl_df structure
  if (rebuild_tbl) {
    rnet <- sf::st_as_sf(dplyr::as_tibble(rnet))
  }

  rnet
}
