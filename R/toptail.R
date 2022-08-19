#' Clip the first and last n metres of SpatialLines
#'
#' Takes lines and removes the start and end point, to a distance determined
#' by the user.
#'
#' Note: see the function
#' [`toptailgs()`](https://github.com/ropensci/stplanr/blob/v1.0.0/R/toptail.R)
#' in {stplanr} v0.8.5 for an implementation that uses the geosphere
#' package.
#'
#' @param l An `sf` object representing lines
#' @param toptail_dist The distance (in metres) to top and tail the line by.
#' Can either be a single value or a vector of the same length as the
#' SpatialLines object.
#' @param ... Arguments passed to `sf::st_buffer()`
#' @aliases toptail
#' @family lines
#' @export
#' @examples
#' lib_versions <- sf::sf_extSoftVersion()
#' lib_versions
#' # dont test due to issues with sp classes on some set-ups
#' if (lib_versions[3] >= "6.3.1") {
#'   l <- routes_fast_sf[2:4, ]
#'   l_top_tail <- geo_toptail(l, 300)
#'   l_top_tail
#'   plot(sf::st_geometry(l_top_tail))
#'   plot(sf::st_geometry(geo_toptail(l, 600)), lwd = 9, add = TRUE)
#' }
geo_toptail <- function(l, toptail_dist, ...) {
  suppressMessages(suppressWarnings({
    line_list <- lapply(
      seq(nrow(l)),
      function(i) {
        li <- l[i, ]
        sel_points <- sf::st_union(
          lwgeom::st_startpoint(li),
          lwgeom::st_endpoint(li)
        )
        sel <- geo_buffer(shp = sel_points, dist = toptail_dist, nQuadSegs = 5, ...)
        if (any(sf::st_contains_properly(sel, li, sparse = FALSE))) {
          message(
            "Line ", i, " is completely removed by the clip and",
            " is omitted from the results"
          )
          return()
        }
        sf::st_difference(x = li, y = sel)
      }
    )
  }))
  out <- do.call(rbind, line_list)
  # out <- data.table::rbindlist(line_list)
  # sf::st_sf(out)
  out
}

#' Clip the beginning and ends of `sf` LINESTRING objects
#'
#' Takes lines and removes the start and end point, to a distance determined
#' by the nearest `buff` polygon border.
#'
#' @inheritParams geo_toptail
#' @param buff An `sf` object with POLYGON geometry to buffer the linestring.
#' @family lines
#' @export
#' @examples
#' l <- routes_fast_sf
#' buff <- zones_sf
#' r_toptail <- toptail_buff(l, buff)
#' nrow(l)
#' nrow(r_toptail)
#' plot(zones_sf$geometry)
#' plot(l$geometry, add = TRUE)
#' plot(r_toptail$geometry, lwd = 5, add = TRUE)
toptail_buff <- function(l, buff, ...) {
  i_indexed <- out <- NULL
  for (i in 1:length(l)) {
    lpoints <- line2points(l[i, ])
    # Select zones per line
    sel <- sf::st_union(buff[lpoints, ])
    l2 <- sf::st_difference(l$geometry[i], sel)
    # mapview::mapview(sel) +
    #   mapview::mapview(l2[1])
    if (length(l2) == 0) {
      next
    }
    i_indexed <- c(i_indexed, i)
    out <- c(out, l2)
  }
  out <- sf::st_sfc(out)
  l_between_zones <- l[i_indexed, ]
  l_between_zones$geometry <- out
  l_between_zones
}
