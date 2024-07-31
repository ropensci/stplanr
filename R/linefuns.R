# Line functions

#' Retrieve the number of vertices in sf objects
#'
#' Returns a vector of the same length as the number of sf objects.
#'
#' @param l An sf object with LINESTRING geometry
#' @family lines
#' @export
#' @examples
#' l <- routes_fast_sf
#' n_vertices(l)
#' n_vertices(zones_sf)
n_vertices <- function(l) {
  UseMethod("n_vertices")
}
#' @export
n_vertices.sf <- function(l) {
  sapply(sf::st_geometry(l), function(x) nrow(sf::st_coordinates(x)))
}

#' Identify lines that are points
#'
#' OD matrices often contain 'intrazonal' flows, where the origin is the same point as the
#' destination. This function can help identify such intrazonal OD pairs, using 2 criteria:
#' the total number of vertices (2 or fewer) and whether the origin and destination are the same.
#'
#' @details
#' Returns a boolean vector. TRUE means that the associated line is in fact a point
#' (has no distance). This can be useful for removing data that will not be plotted.
#'
#' @inheritParams line2df
#' @family lines
#' @export
#' @examples
#' islp <- is_linepoint(flowlines_sf)
#' nrow(flowlines_sf)
#' sum(islp)
#' # Remove invisible 'linepoints'
#' nrow(flowlines_sf[!islp, ])
is_linepoint <- function(l) {
  nverts <- n_vertices(l)
  sel <- nverts <= 2
  ldf <- line2df(l)
  ldf$fx == ldf$tx & ldf$fy & ldf$ty & sel
}
#' Find the bearing of straight lines
#'
#' This function returns the
#' bearing (in degrees relative to north) of lines.
#'
#' @details
#' Returns a boolean vector. TRUE means that the associated line is in fact a point
#' (has no distance). This can be useful for removing data that will not be plotted.
#'
#' @inheritParams line2df
#' @param bidirectional Should the result be returned in a bidirectional format?
#' Default is FALSE. If TRUE, the same line in the oposite direction would have the same bearing
#' @family lines
#' @export
#' @examples
#' l <- flowlines_sf[1:5, ]
#' bearings_sf_1_9 <- line_bearing(l)
#' bearings_sf_1_9 # lines of 0 length have NaN bearing
#' b <- line_bearing(l, bidirectional = TRUE)
#' r <- routes_fast_sf[1:5, ]
#' b2 <- line_bearing(r, bidirectional = TRUE)
#' plot(b, b2)
line_bearing <- function(l, bidirectional = FALSE) {
  # Convert to lon/lat data if not already
  is_longlat <- sf::st_is_longlat(l)
  if (!is_longlat) {
    l <- sf::st_transform(l, "EPSG:4326")
  }
  odc <- od::od_coordinates(l)
  bearing <- geosphere::bearing(
    p1 = odc[, 1:2],
    p2 = odc[, 3:4]
  )
  if (bidirectional) {
    bearing <- make_bidirectional(bearing)
  }
  bearing
}
#' Calculate the angular difference between lines and a predefined bearing
#'
#' This function was designed to find lines that are close to parallel and perpendicular
#' to some pre-defined route. It can return results that are absolute (contain information
#' on the direction of turn, i.e. + or - values for clockwise/anticlockwise),
#' bidirectional (which mean values greater than +/- 90 are impossible).
#'
#' Building on the convention used in in the `bearing()` function from the
#' `geosphere` package and in many applications,
#' North is definied as 0, East as 90 and West as -90.
#'
#' @inheritParams line_bearing
#' @param absolute If TRUE (the default) only positive values can be returned
#' @param angle an angle in degrees relative to North, with 90 being East and -90 being West.
#'  (direction of rotation is ignored).
#' @family lines
#'
#' @export
#' @examples
#' lib_versions <- sf::sf_extSoftVersion()
#' lib_versions
#' # fails on some systems (with early versions of PROJ)
#' if (lib_versions[3] >= "6.3.1") {
#'   # Find all routes going North-South
#'   lines_sf <- od2line(od_data_sample, zones = zones_sf)
#'   angle_diff(lines_sf[2, ], angle = 0)
#'   angle_diff(lines_sf[2:3, ], angle = 0)
#' }
angle_diff <- function(l, angle, bidirectional = FALSE, absolute = TRUE) {
  if (is(object = l, "sf")) {
    line_angles <- line_bearing(l)
  } else {
    line_angles <- l
  }
  angle_diff <- angle - line_angles
  angle_diff[angle_diff <= -180] <- angle_diff[angle_diff <= -180] + 180
  angle_diff[angle_diff >= 180] <- angle_diff[angle_diff >= 180] - 180
  if (bidirectional) {
    angle_diff[angle_diff <= -90] <- 180 + angle_diff[angle_diff <= -90]
    angle_diff[angle_diff >= 90] <- 180 - angle_diff[angle_diff >= 90]
  }
  if (absolute) {
    angle_diff <- abs(angle_diff)
  }
  angle_diff
}
#' Find the mid-point of lines
#'
#' @inheritParams line2df
#' @param tolerance The tolerance used to break lines at verteces.
#'   See [lwgeom::st_linesubstring()].
#' @family lines
#' @export
#' @examples
#' l <- routes_fast_sf[2:5, ]
#' plot(l$geometry, col = 2:5)
#' midpoints <- line_midpoint(l)
#' plot(midpoints, add = TRUE)
#' # compare with sf::st_point_on_surface:
#' midpoints2 <- sf::st_point_on_surface(l)
#' plot(midpoints2, add = TRUE, col = "red")
line_midpoint <- function(l, tolerance = NULL) {
  if (is.null(tolerance)) {
    sub <- lwgeom::st_linesubstring(x = l, from = 0, to = 0.5)
  } else {
    sub <- lwgeom::st_linesubstring(x = l, from = 0, to = 0.5, tolerance = tolerance)
  }
  lwgeom::st_endpoint(sub)
}

#' Divide an sf object with LINESTRING geometry into regular segments
#'
#' This function keeps the attributes.
#' Note: results differ when `use_rsgeo` is `TRUE`:
#' the `{rsgeo}` implementation will be faster.
#' Results may not always keep returned linestrings below
#' the `segment_length` value.
#' The `{rsgeo}` implementation does not always
#' return the number of segments requested due to an upstream issue in the
#' `geo` Rust crate.
#'
#' Note: we recommend running these functions on projected data.
#'
#' @inheritParams line2df
#' @param segment_length The approximate length of segments in the output (overrides n_segments if set)
#' @param n_segments The number of segments to divide the line into.
#'   If there are multiple lines, this should be a vector of the same length.
#' @param use_rsgeo Should the `rsgeo` package be used?
#'  If `rsgeo` is available, this faster implementation is used by default.
#'  If `rsgeo` is not available, the `lwgeom` package is used.
#' @param debug_mode Should debug messages be printed? Default is FALSE.
#' @family lines
#' @export
#' @examples
#' library(sf)
#' l <- routes_fast_sf[2:4, "ID"]
#' l_seg_multi <- line_segment(l, segment_length = 1000, use_rsgeo = FALSE)
#' l_seg_n <- line_segment(l, n_segments = 2)
#' l_seg_n <- line_segment(l, n_segments = c(1:3))
#' # Number of subsegments
#' table(l_seg_multi$ID)
#' plot(l_seg_multi["ID"])
#' plot(l_seg_multi$geometry, col = seq_along(l_seg_multi), lwd = 5)
#' round(st_length(l_seg_multi))
#' # rsgeo implementation (default if available):
#' if (rlang::is_installed("rsgeo")) {
#'   rsmulti = line_segment(l, segment_length = 1000, use_rsgeo = TRUE)
#'   plot(rsmulti["ID"])
#' }
#' # Check they have the same total length, to nearest mm:
#' # round(sum(st_length(l_seg_multi)), 3) == round(sum(st_length(rsmulti)), 3)
#' # With n_segments for 1 line (set use_rsgeo to TRUE to use rsgeo):
#' l_seg_multi_n <- line_segment(l[1, ], n_segments = 3, use_rsgeo = FALSE)
#' l_seg_multi_n <- line_segment(l$geometry[1], n_segments = 3, use_rsgeo = FALSE)
#' # With n_segments for all 3 lines:
#' l_seg_multi_n <- line_segment(l, n_segments = 2)
#' nrow(l_seg_multi_n) == nrow(l) * 2
line_segment <- function(
    l,
    segment_length = NA,
    n_segments = NA,
    use_rsgeo = NULL,
    debug_mode = FALSE) {
  # Defensive programming:
  if (any(is.na(segment_length)) && any(is.na(n_segments))) {
    rlang::abort(
      "segment_length or n_segments must be set.",
      call = rlang::caller_env()
    )
  }
  UseMethod("line_segment")
}
#' @export
line_segment.sf <- function(
    l,
    segment_length = NA,
    n_segments = NA,
    use_rsgeo = NULL,
    debug_mode = FALSE
  ) {
  # Get n_segments if not provided:
  if (all(is.na(n_segments))) {
    segment_lengths <- as.numeric(sf::st_length(l))
    n_segments <- n_segments(segment_lengths, segment_length)
  } else {
    if (length(n_segments) != nrow(l)) {
      if (length(n_segments) == 1) {
        message("Setting n_segments to ", n_segments, " for all lines")
        n_segments <- rep.int(n_segments, nrow(l))
      }
    }
  }
  # Decide whether to use rsgeo or lwgeom, if not set:
  if (is.null(use_rsgeo)) {
    use_rsgeo <- use_rsgeo(l)
  }
  if (use_rsgeo) {
    # If using rsgeo, we can do the whole thing in one go:
    res <- line_segment_rsgeo(l, n_segments = n_segments)
    return(res)
  }
  # lwgeom implementation:
  n_row_l <- nrow(l)
  if (n_row_l > 1) {
    res_list <- pbapply::pblapply(seq(n_row_l), function(i) {
      if (debug_mode) {
        message(paste0("Processing row ", i, " of ", n_row_l))
      }
      l_segmented <- line_segment1(l[i, ], n_segments = n_segments[i], segment_length = NA)
      res_names <- names(sf::st_drop_geometry(l_segmented))
      # Work-around for https://github.com/ropensci/stplanr/issues/531
      if (i == 1) {
        res_names <<- names(sf::st_drop_geometry(l_segmented))
      }
      l_segmented <- l_segmented[res_names]
      l_segmented
    })
    res <- bind_sf(res_list)
  } else {
    # If there's only one row:
    res <- line_segment1(l, n_segments = n_segments)
  }
  res
}

#' @export
line_segment.sfc_LINESTRING <- function(
    l,
    segment_length = NA,
    n_segments = NA,
    use_rsgeo = NULL,
    debug_mode = FALSE) {
  l <- sf::st_as_sf(l)
  res <- line_segment(l, segment_length = segment_length, n_segments = n_segments, use_rsgeo, debug_mode)
  sf::st_geometry(res)
}

#' Segment a single line, using lwgeom or rsgeo
#'
#' @inheritParams line_segment
#' @param n_segments The number of segments to divide the line into
#' @family lines
#' @export
#' @examples
#' l <- routes_fast_sf[2, ]
#' l_seg2 <- line_segment1(l = l, n_segments = 2)
#' # Test with rsgeo (must be installed):
#' # l_seg2_rsgeo = line_segment1(l = l, n_segments = 2)
#' # waldo::compare(l_seg2, l_seg2_rsgeo)
#' l_seg3 <- line_segment1(l = l, n_segments = 3)
#' l_seg_100 <- line_segment1(l = l, segment_length = 100)
#' l_seg_1000 <- line_segment1(l = l, segment_length = 1000)
#' plot(sf::st_geometry(l_seg2), col = 1:2, lwd = 5)
#' plot(sf::st_geometry(l_seg3), col = 1:3, lwd = 5)
#' plot(sf::st_geometry(l_seg_100), col = seq(nrow(l_seg_100)), lwd = 5)
#' plot(sf::st_geometry(l_seg_1000), col = seq(nrow(l_seg_1000)), lwd = 5)
line_segment1 <- function(
    l,
    n_segments = NA,
    segment_length = NA
    ) {
  UseMethod("line_segment1")
}
#' @export
line_segment1.sf <- function(
    l,
    n_segments = NA,
    segment_length = NA) {
  if (is.na(n_segments) && is.na(segment_length)) {
    rlang::abort(
      "`n_segment` or `segment_length` must be set.",
      call = rlang::caller_env()
    )
  }
  if (is.na(n_segments)) {
    l_length <- as.numeric(sf::st_length(l))
    n_segments <- max(round(l_length / segment_length), 1)
  }
  if (n_segments == 1) {
    return(l)
  }

  res <- line_segment_lwgeom(l, n_segments)

  res
}
#' @export
line_segment1.sfc_LINESTRING <- function(
    l,
    n_segments = NA,
    segment_length = NA) {
  l <- sf::st_as_sf(l)
  res <- line_segment1(l, n_segments, segment_length = segment_length)
  sf::st_geometry(res)
}

make_bidirectional <- function(bearing) {
  is_na_bearings <- is.na(bearing)
  non_na_bearings <- bearing[!is_na_bearings]
  non_na_bearings[non_na_bearings > 90] <- non_na_bearings[non_na_bearings > 90] - 180
  non_na_bearings[non_na_bearings < -90] <- non_na_bearings[non_na_bearings < -90] + 180
  bearing[!is_na_bearings] <- non_na_bearings
  bearing
}

#' Rapid row-binding of sf objects
#'
#' @param x List of sf objects to combine
#' @return An sf data frame
#' @family geo
bind_sf <- function(x) {
  if (length(x) == 0) stop("Empty list")
  geom_name <- attr(x[[1]], "sf_column")
  x <- data.table::rbindlist(x, use.names = FALSE)
  # x = collapse::unlist2d(x, idcols = FALSE, recursive = FALSE)
  x[[geom_name]] <- sf::st_sfc(x[[geom_name]], recompute_bbox = TRUE)
  x <- sf::st_as_sf(x)
  x
}

use_rsgeo <- function(shp) {
  rsgeo_installed <- rlang::is_installed("rsgeo", version = "0.1.6")
  if (!rsgeo_installed) {
    warning("rsgeo not installed, using lwgeom")
    return(FALSE)
  }
  TRUE
}

line_segment_rsgeo <- function(l, n_segments) {

  crs <- sf::st_crs(l)

  # extract geometry and convert to rsgeo
  geo <- rsgeo::as_rsgeo(sf::st_geometry(l))

  # segmentize the line strings
  res_rsgeo <- rsgeo::line_segmentize(geo, n_segments)

  # sf linestring:
  res_sfc_ml = sf::st_as_sfc(res_rsgeo)
  n_segments_rsgeo = lengths(res_sfc_ml)
  if (! all(n_segments == n_segments_rsgeo)) {
    sum_segments <- sum(n_segments)
    sum_segments_rsgeo <- sum(n_segments_rsgeo)
    msg = paste0(
      "Requested number of segments (", sum_segments, ") ",
      "does not match the number of segments returned by rsgeo (", sum_segments_rsgeo, ")."
    )
    message(msg)
  }
  # make them into sfc_LINESTRING
  res <- sf::st_cast(res_sfc_ml, "LINESTRING")

  # give them them CRS
  res <- sf::st_set_crs(res, crs)

  # calculate the number of original geometries
  n_lines <- length(geo)
  # create index ids to grab rows from
  ids <- rep.int(seq_len(n_lines), n_segments_rsgeo)

  # index the original sf object
  res_tbl <- sf::st_drop_geometry(l)[ids, , drop = FALSE]

  # convert to sf and return
  res_sf <- sf::st_as_sf(
    res_tbl,
    geometry = res,
    crs = crs
    )
  res_sf
}

line_segment_lwgeom <- function(l, n_segments) {
  from_to_sequence <- seq(from = 0, to = 1, length.out = n_segments + 1)
  suppressWarnings({
    line_segment_list <- lapply(seq(n_segments), function(i) {
      lwgeom::st_linesubstring(
        x = l,
        from = from_to_sequence[i],
        to = from_to_sequence[i + 1]
      )
    })
  })
  res <- bind_sf(line_segment_list)
  res
}

#' Vectorised function to calculate number of segments given a max segment length
#' @param line_length The length of the line
#' @param max_segment_length The maximum length of each segment
#' @family lines
#' @export
#' @examples
#' n_segments(50, 10)
#' n_segments(50.1, 10)
#' n_segments(1, 10)
#' n_segments(1:9, 2)
n_segments <- function(line_length, max_segment_length) {
  pmax(ceiling(line_length / max_segment_length), 1)
}
