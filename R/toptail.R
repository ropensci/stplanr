#' Clip the first and last n metres of SpatialLines
#'
#' Takes lines and removes the start and end point, to a distance determined
#' by the user.
#'
#' Note: [toptailgs()] is around 10 times faster, but only works
#' on data with geographic CRS's due to its reliance on the geosphere
#' package.
#'
#' @param l A SpatialLines object
#' @param toptail_dist The distance (in metres) to top and tail the line by.
#' Can either be a single value or a vector of the same length as the
#' SpatialLines object.
#' @param ... Arguments passed to rgeos::gBuffer()
#' @aliases toptail
#' @family lines
#' @export
#' @examples
#' lib_versions <- sf::sf_extSoftVersion()
#' lib_versions
#' # dont test due to issues with sp classes on some set-ups
#' if (lib_versions[3] >= "6.3.1") {
#'   # l <- routes_fast[2:4, ] # to run with sp classes
#'   l <- routes_fast_sf[2:4, ]
#'   l_top_tail <- geo_toptail(l, 300)
#'   l_top_tail
#'   plot(sf::st_geometry(l_top_tail))
#'   plot(sf::st_geometry(geo_toptail(l, 600)), lwd = 9, add = TRUE)
#' }
geo_toptail <- function(l, toptail_dist, ...) {
  UseMethod("geo_toptail")
}
#' @export
geo_toptail.Spatial <- toptail <- function(l, toptail_dist, ...) {
  if (length(toptail_dist) > 1 & length(toptail_dist) != length(l)) {
    stop("toptail_dist is vector but not of equal length to spatial object")
  }

  lpoints <- line2points(l)

  if (length(toptail_dist) == 1) {
    toptail_dist <- rep(toptail_dist, length(l))
  }

  for (i in 1:length(l)) {
    sel_points <- lpoints[lpoints$id == i, ]

    # Create buffer for geographic or projected crs
    if (!sp::is.projected(l)) {
      sel <- geo_buffer(lpoints, width = toptail_dist[i], ..., silent = TRUE)
    } else {
      sel <- rgeos::gBuffer(lpoints, dist = toptail_dist[i], ...)
    }

    if (rgeos::gContainsProperly(sel, l[i, ])) {
      message(paste0(
        "Line ", i, " is completely removed by the clip and",
        " is omitted from the results"
      ))
      next
    }
    l2 <- rgeos::gDifference(l[i, ], sel)
    if (!exists("out")) {
      out <- l2
    } else {
      out <- raster::bind(out, l2)
    }
  }
  out
}
#' @export
geo_toptail.sf <- function(l, toptail_dist, ...) {
  suppressMessages(suppressWarnings({
    line_list <- lapply(
      seq(nrow(l)),
      function(i) {
        li <- l[i, ]
        sel_points <- sf::st_union(
          lwgeom::st_startpoint(li),
          lwgeom::st_endpoint(li)
        )
        sel <- geo_buffer(shp = sel_points, dist = toptail_dist, nQuadSegs = 5)
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
#' Clip the first and last n metres of SpatialLines
#'
#' Takes lines and removes the start and end point, to a distance determined
#' by the user. Uses the geosphere::distHaversine function and requires
#' coordinates in WGS84 (lng/lat).
#'
#' @param l A SpatialLines object
#' @param toptail_dist The distance (in metres) to top the line by.
#' Can be either a single value or a vector of the same length as the
#' SpatialLines object. If tail_dist is missing, is used as the tail distance.
#' @param tail_dist The distance (in metres) to tail the line by. Can be
#' either a single value or a vector of the same length as the SpatialLines
#' object.
#' @family lines
#' @export
#' @examples
#' data("routes_fast")
#' rf <- routes_fast[2:3, ]
#' r_toptail <- toptailgs(rf, toptail_dist = 300)
#' plot(rf, lwd = 3)
#' plot(r_toptail, col = "red", add = TRUE)
#' plot(cents, add = TRUE)
toptailgs <- function(l, toptail_dist, tail_dist = NULL) {
  if (length(toptail_dist) > 1) {
    if (length(toptail_dist) != length(l)) {
      stop("toptail_dist is vector but not of equal length to SpatialLines object")
    }
  }
  if (!missing(tail_dist)) {
    if (length(tail_dist) > 1) {
      if (length(tail_dist) != length(l)) {
        stop("tail_dist is vector but not of equal length to SpatialLines object")
      }
    }
  }
  else {
    tail_dist <- toptail_dist
  }

  toptail_disto <- toptail_dist
  tail_disto <- tail_dist

  i <- 1
  while (i <= length(l)) {
    toptail_dist <- ifelse(length(toptail_disto) == 1, toptail_disto, toptail_disto[i])
    linecoords <- coordinates(l@lines[[i]])[[1]]
    topdists <- geosphere::distHaversine(linecoords[1, ], linecoords)
    linecoords <- rbind(
      tail(linecoords[which(topdists < toptail_dist), , drop = FALSE], n = 1) + (
        linecoords[which(topdists >= toptail_dist), , drop = FALSE][1, ] -
          tail(linecoords[which(topdists < toptail_dist), , drop = FALSE], n = 1)
      ) * (
        (toptail_dist - tail(topdists[which(topdists < toptail_dist)], n = 1)) / (topdists[which(topdists >= toptail_dist)][1] - tail(topdists[which(topdists < toptail_dist)], n = 1))
      ),
      linecoords[which(topdists >= toptail_dist), , drop = FALSE]
    )
    bottomdists <- geosphere::distHaversine(linecoords[nrow(linecoords), ], linecoords)
    tail_dist <- ifelse(length(tail_disto) == 1, tail_disto, tail_disto[i])

    linecoords <- rbind(
      linecoords[which(bottomdists >= tail_dist), , drop = FALSE],
      tail(linecoords[which(bottomdists >= tail_dist), , drop = FALSE], n = 1) + (
        linecoords[which(bottomdists < tail_dist), , drop = FALSE][1, ] -
          tail(linecoords[which(bottomdists >= tail_dist), , drop = FALSE], n = 1)
      ) *
        ((tail(bottomdists[which(bottomdists >= tail_dist)], n = 1) - tail_dist) / (tail(bottomdists[which(bottomdists >= tail_dist)], n = 1) - bottomdists[which(bottomdists < tail_dist)][1]))
    )
    l@lines[[i]]@Lines[[1]]@coords <- unname(linecoords)
    i <- i + 1
  }
  return(l)
}

#' Clip the beginning and ends SpatialLines to the edge of SpatialPolygon borders
#'
#' Takes lines and removes the start and end point, to a distance determined
#' by the nearest polygon border.
#'
#' @param l An sf LINESTRING object
#' @param buff An sf POLYGON object to act as the buffer
#' @param ... Arguments passed to rgeos::gBuffer()
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
