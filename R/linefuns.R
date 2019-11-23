# Line functions

#' Retrieve the number of vertices from a SpatialLines or SpatialPolygons object
#'
#' Returns a vector of the same length as the number of lines,
#' with the number of vertices per line or polygon.
#'
#' See <http://gis.stackexchange.com/questions/58147/> for more information.
#'
#' @param l A SpatialLines or SpatalPolygons object
#' @family lines
#' @export
#' @examples
#' n_vertices(routes_fast)
#' n_vertices(routes_fast_sf)
n_vertices <- function(l) {
  UseMethod("n_vertices")
}
#' @export
n_vertices.Spatial <- function(l) {
  sapply(l@lines, function(x) nrow(x@Lines[[1]]@coords))
}
#' @export
n_vertices.sf <- function(l) {
  geoms <- sf::st_coordinates(l)
  L1 <- rlang::quo(L1)
  geoms %>%
    dplyr::as_data_frame() %>%
    dplyr::group_by(!!L1) %>%
    dplyr::summarise(n_vertices = dplyr::n()) %>%
    dplyr::pull(n_vertices)
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
#' data(flowlines)
#' islp <- is_linepoint(flowlines)
#' nrow(flowlines)
#' sum(islp)
#' # Remove invisible 'linepoints'
#' nrow(flowlines[!islp, ])
is_linepoint <- function(l) {
  nverts <- n_vertices(l)
  sel <- nverts <= 2
  ldf <- line2df(l)
  ldf$fx == ldf$tx & ldf$fy & ldf$ty & sel
}
#' Find the bearing of straight lines
#'
#' This is a simple wrapper around the geosphere function [bearing()] to return the
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
#' bearings_sf_1_9 <- line_bearing(flowlines_sf[1:5, ])
#' bearings_sf_1_9 # lines of 0 length have NaN bearing
#' bearings_sp_1_9 <- line_bearing(flowlines[1:5, ])
#' bearings_sp_1_9
#' plot(bearings_sf_1_9, bearings_sp_1_9)
#' line_bearing(flowlines_sf[1:5, ], bidirectional = TRUE)
#' line_bearing(flowlines[1:5, ], bidirectional = TRUE)
line_bearing <- function(l, bidirectional = FALSE) {
  UseMethod("line_bearing")
}
#' @export
line_bearing.Spatial <- function(l, bidirectional = FALSE) {
  ldf <- line2df(l)
  bearing <- geosphere::bearing(as.matrix(ldf[, c("fx", "fy")]), as.matrix(ldf[, c("tx", "ty")]))
  if (bidirectional) {
    bearing <- make_bidirectional(bearing)
  }
  bearing
}
#' @export
line_bearing.sf <- function(l, bidirectional = FALSE) {
  p <- sf::st_geometry(line2points(l))
  i_s <- 1:length(sf::st_geometry(l)) * 2 - 1
  bearing_radians <- sapply(i_s, function(i) lwgeom::st_geod_azimuth(p[i:(i + 1)]))
  bearing = bearing_radians * 180 / (pi)
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
#' Building on the convention used in [bearing()] and in many applications,
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
#' # Find all routes going North-South
#' a <- angle_diff(flowlines, angle = 0, bidirectional = TRUE, absolute = TRUE)
#' plot(flowlines)
#' plot(flowlines[a < 15, ], add = TRUE, lwd = 3, col = "red")
#' # East-West
#' plot(flowlines[a > 75, ], add = TRUE, lwd = 3, col = "green")
#' angle_diff(flowlines_sf[2, ], angle = 0)
angle_diff <- function(l, angle, bidirectional = FALSE, absolute = TRUE) {
  UseMethod("angle_diff")
}
#' @export
angle_diff.Spatial <- function(l, angle, bidirectional = FALSE, absolute = TRUE) {
  if (is(object = l, "Spatial")) {
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
#' @export
angle_diff.sf <- function(l, angle, bidirectional = FALSE, absolute = TRUE) {
  l_sp <- as(l, "Spatial")
  angle_diff.Spatial(l_sp, angle, bidirectional = FALSE, absolute = TRUE)
}
#' Find the mid-point of lines
#'
#' This is a wrapper around [SpatialLinesMidPoints()] that allows it to find the midpoint
#' of lines that are not projected, which have a lat/long CRS.
#' @inheritParams line2df
#' @family lines
#' @export
#' @examples
#' data(routes_fast)
#' line_midpoint(routes_fast[2:5, ])
line_midpoint <- function(l) {
  UseMethod("line_midpoint")
}
#' @export
line_midpoint.Spatial <- function(l) {
  gprojected(l, maptools::SpatialLinesMidPoints)
}
#' @export
line_midpoint.sf <- function(l) {
  l <- as(l, "Spatial")
  res_sp <- line_midpoint.Spatial(l)
  sf::st_as_sf(l)
}
#' Calculate length of lines in geographic CRS
#' @inheritParams line2df
#' @param byid Logical determining whether the length is returned per object (default is true)
#' @export
line_length <- function(l, byid = TRUE) {
  gprojected(l, rgeos::gLength, byid = byid)
}

#' Divide SpatialLines dataset into regular segments
#' @inheritParams line2df
#' @param n_segments The number of segments to divide the line into
#' @param segment_length The approximate length of segments in the output (overides n_segments if set)
#' @family lines
#' @export
#' @examples
#' data(routes_fast)
#' l <- routes_fast[2, ]
#' library(sp)
#' l_seg2 <- line_segment(l = l, n_segments = 2)
#' plot(l_seg2, col = l_seg2$group, lwd = 50)
line_segment <- function(l, n_segments, segment_length = NA) {
  if (!is.na(segment_length)) {
    l_length <- line_length(l)
    n_segments <- round(l_length / segment_length)
  }
  if (n_segments == 2) {
    pseg <- line_midpoint(l)
  } else {
    pseg <- sp::spsample(x = l, n = n_segments - 1, type = "regular")
  }
  l_geom <- raster::geom(l)
  l_coords <- l_geom[, c("x", "y")]
  knn_res <- nabor::knn(data = l_coords, query = sp::coordinates(pseg), k = 1)
  sel_nearest <- c(knn_res$nn.idx)
  for (i in 1:(length(sel_nearest) + 1)) {
    ids <- c(1, sel_nearest, nrow(l))
    if (i == 1) {
      l_seg <- points2line(l_coords[ids[i]:ids[(i + 1)], ])
      sp::spChFIDs(l) <- i
    } else if (i == length(sel_nearest) + 1) {
      l_temp <- points2line(l_coords[ids[i]:nrow(l_coords), ])
      sp::spChFIDs(l_temp) <- i
      l_seg <- raster::bind(l_seg, l_temp)
    } else {
      l_temp <- points2line(l_coords[ids[i]:ids[(i + 1)], ])
      sp::spChFIDs(l_temp) <- i
      l_seg <- raster::bind(l_seg, l_temp)
    }
  }
  l_seg <- sp::SpatialLinesDataFrame(l_seg, data.frame(group = 1:i))
  raster::crs(l_seg) <- raster::crs(l)
  l_seg
}
make_bidirectional <- function(bearing) {
  is_na_bearings <- is.na(bearing)
  non_na_bearings <- bearing[!is_na_bearings]
  non_na_bearings[non_na_bearings > 90] <- non_na_bearings[non_na_bearings > 90] - 180
  non_na_bearings[non_na_bearings < -90] <- non_na_bearings[non_na_bearings < -90] + 180
  bearing[!is_na_bearings] <- non_na_bearings
  bearing
}
