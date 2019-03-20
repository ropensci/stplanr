#' Extract coordinates from OD data
#'
#' @section Details:
#' Origin-destination ('OD') flow data is often provided
#' in the form of 1 line per flow with zone codes of origin and destination
#' centroids. This can be tricky to plot and link-up with geographical data.
#' This function makes the task easier.
#' @inheritParams od2line
#' @family od
#' @export
#' @examples
#' data(flow)
#' data(zones)
#' od2odf(flow[1:2, ], zones)
od2odf <- function(flow, zones) {
  coords <- dplyr::data_frame(
    code = as.character(zones[[1]]),
    fx = coordinates(zones)[, 1], fy = coordinates(zones)[, 2]
  )
  flowcode <- dplyr::data_frame(code_o = as.character(flow[[1]]), code_d = as.character(flow[[2]]))
  odf <- dplyr::left_join(flowcode, coords, by = c("code_o" = "code"))
  coords <- dplyr::rename_(coords, tx = quote(fx), ty = quote(fy))
  odf <- dplyr::left_join(odf, coords, by = c("code_d" = "code"))

  data.frame(odf) # return data.frame as more compatible with spatial data
}

#' Create matrices representing origin-destination coordinates
#'
#' This function takes a wide range of input data types (spatial lines, points or text strings)
#' and returns a matrix of coordinates representing origin (fx, fy) and destination (tx, ty) points.
#'
#' @param from An object representing origins
#' (if lines are provided as the first argument, from is assigned to `l`)
#' @param to An object representing destinations
#' @param l Only needed if from and to are empty, in which case this
#' should be a spatial object representing desire lines
#' @family od
#' @export
#' @examples
#' od_coords(from = c(0, 52), to = c(1, 53)) # lon/lat coordinates
#' od_coords(from = cents[1, ], to = cents[2, ]) # Spatial points
#' od_coords(cents_sf[1:3, ], cents_sf[2:4, ]) # sf points
#' # od_coords("Hereford", "Leeds") # geocode locations
#' od_coords(flowlines[1:3, ])
#' od_coords(flowlines_sf[1:3, ])
od_coords <- function(from = NULL, to = NULL, l = NULL) {

  if (is(object = from, class2 = "sf")) {
    is_sf_line <- all(sf::st_geometry_type(from) == "LINESTRING")
  } else {
    is_sf_line <- FALSE
  }

  if (is_sf_line | any(grepl(pattern = "Line", x = class(from)))) {
    l <- from
  }

  if (!is.null(l)) {
    coord_matrix <- line2df(l) %>%
      dplyr::select("fx", "fy", "tx", "ty")
  }

  else {
    # Convert sp object to lat/lon vector
    if (is(object = from, "Spatial")) from <- sp::coordinates(from)
    if (is(object = to, "Spatial")) to <- sp::coordinates(to)

    # sf objects
    if (is(object = from, "sf")) from <- sf::st_coordinates(from)
    if (is(object = to, "sf")) to <- sf::st_coordinates(to)

    # Convert character strings to lon/lat if needs be
    if (is.character(from)) from <- matrix(geo_code(from), ncol = 2)
    if (is.character(to)) to <- matrix(geo_code(to), ncol = 2)
    if (is.vector(from) & is.vector(to)) {
      coord_matrix <- matrix(c(from, to), ncol = 4)
    } else {
      coord_matrix <- cbind(from, to)
    }
    colnames(coord_matrix) <- c("fx", "fy", "tx", "ty")
  }

  as.matrix(coord_matrix)

}

#' Convert origin-destination coordinates into desire lines
#'
#' @param odc A data frame or matrix of representing the coordinates
#' of origin-destination data. The first two columns represent the
#' coordinates of the origin (typically longitude and latitude) points;
#' the second two columns represent the coordinates of the destination
#' (in the same CRS). Each row represents travel from origin to destination.
#' @param crs A number representing the coordinate reference system
#' of the result.
#' @family od
#' @export
#' @examples
#' odf <- od_coords(l = flowlines_sf)
#' odlines <- od_coords2line(odf)
#' odlines <- od_coords2line(odf, crs = 4326)
#' plot(odlines)
od_coords2line <- function(odc, crs = sf::st_crs()) {
  odm <- as.matrix(odc)
  linestring_list <- lapply(seq(nrow(odm)), function(i) {
    sf::st_linestring(rbind(odm[i, 1:2], odm[i, 3:4]))
  })
  sf::st_sfc(linestring_list, crs = crs)
}
#' Convert flow data to SpatialLinesDataFrame
#'
#' Origin-destination ('OD') flow data is often provided
#' in the form of 1 line per flow with zone codes of origin and destination
#' centroids. This can be tricky to plot and link-up with geographical data.
#' This function makes the task easier.
#'
#' @details
#' The function expects zone codes to be in the 1st column of the zones/destinations
#' datasets and the 1st and 2nd columns of the flow data, respectively.
#'
#' [od2line2()] is a faster implementation
#' (around 6 times faster on large datasets)
#' that returns a `SpatialLines` object, omitting the data and working
#' only when there is no destinations dataset (i.e. when the geography of
#' origins is the same as that of destinations).
#'
#' @param flow A data frame representing the flow between two points
#' or zones. The first two columns of this data frame should correspond
#' to the first column of the data in the zones. Thus in [cents()],
#' the first column is geo_code. This corresponds to the first two columns
#' of [flow()].
#' @param zones A spatial object representing origins (and destinations
#' if no separate destinations object is provided) of travel.
#' @param destinations A SpatialPolygonsDataFrame or SpatialPointsDataFrame
#' representing destinations of travel flows.
#' @param zone_code Name of the variable in `zones` containing the ids of the zone.
#' By default this is the first column names in the zones.
#' @param origin_code Name of the variable in `flow` containing the ids of the zone of origin.
#' By default this is the first column name in the flow input dataset.
#' @param dest_code Name of the variable in `flow` containing the ids of the zone of destination.
#' By default this is the second column name in the flow input dataset or the first column name in the
#' destinations if that is set.
#' @param zone_code_d Name of the variable in `destinations` containing the ids of the zone.
#' By default this is the first column names in the destinations.
#' @param silent TRUE by default, setting it to TRUE will show you the matching columns
#' @family od
#' @export
#' @examples
#' l <- od2line(flow = flow, zones = cents)
#' plot(cents)
#' plot(l, lwd = l$All / mean(l$All), add = TRUE)
#' # When destinations are different
#' head(flow_dests[1:5]) # check data
#' head(destinations[1:5])
#' flowlines_dests <- od2line(flow_dests, cents, destinations = destinations)
#' plot(flowlines_dests)
#' l <- od2line(flow, zones_sf)
#' plot(l["All"], lwd = l$All/mean(l$All))
#' @name od2line
NULL

#' @rdname od2line
#' @export
od2line <- function(flow, zones, destinations = NULL,
                    zone_code = names(zones)[1],
                    origin_code = names(flow)[1],
                    dest_code = names(flow)[2],
                    zone_code_d = NA, silent = FALSE) {
  UseMethod("od2line", object = zones)
}
#' @export
od2line.sf <- function(flow, zones, destinations = NULL,
                       zone_code = names(zones)[1],
                       origin_code = names(flow)[1],
                       dest_code = names(flow)[2],
                       zone_code_d = NA, silent = TRUE) {
  if (grepl(pattern = "POLYGON", x = unique(sf::st_geometry_type(zones)))) {
    message("Creating centroids representing desire line start and end points.")
    zones <- sf::st_centroid(zones)
  }

  coords_o <- sf::st_coordinates(zones)[, 1:2]
  origin_points <- coords_o[match(flow[[origin_code]], zones[[zone_code]]), ]

  if (is.null(destinations)) {
    if (!silent) {
      message(paste(
        "Matching", zone_code, "in the zones to", origin_code, "and", dest_code,
        "for origins and destinations respectively"
      ))
    }

    dest_points <- coords_o[match(flow[[dest_code]], zones[[zone_code]]), ]
  } else {
    dest_points <- coords_o[match(flow[[dest_code]], destinations[[zone_code_d]]), ]
  }

  odm = cbind(origin_points, dest_points)

  odsfc <- od_coords2line(odm, crs = sf::st_crs(zones))
  sf::st_sf(flow, geometry = odsfc)

}
#' @export
od2line.Spatial <- function(flow, zones, destinations = NULL,
                            zone_code = names(zones)[1],
                            origin_code = names(flow)[1],
                            dest_code = names(flow)[2],
                            zone_code_d = NA, silent = TRUE) {
  l <- vector("list", nrow(flow))

  if (is.null(destinations)) {
    if (!silent) {
      message(paste(
        "Matching", zone_code, "in the zones to", origin_code, "and", dest_code,
        "for origins and destinations respectively"
      ))
    }
    for (i in 1:nrow(flow)) {
      from <- zones@data[[zone_code]] %in% flow[[origin_code]][i]
      if (sum(from) == 0) {
        warning(paste0("No match for line ", i))
      }
      to <- zones@data[[zone_code]] %in% flow[[dest_code]][i]
      if (sum(to) == 0 & sum(from) == 1) {
        warning(paste0("No match for line ", i))
      }
      x <- sp::coordinates(zones[from, ])
      y <- sp::coordinates(zones[to, ])
      l[[i]] <- sp::Lines(list(sp::Line(rbind(x, y))), as.character(i))
    }
  } else {
    if (is.na(zone_code_d)) {
      zone_code_d <- names(destinations)[1]
    }
    if (!silent) {
      message(paste(
        "Matching", zone_code, "in the zones and", zone_code_d, "in the destinations,\nto",
        origin_code, "and", dest_code,
        "for origins and destinations respectively"
      ))
    }
    for (i in 1:nrow(flow)) {
      from <- zones@data[[zone_code]] %in% flow[[origin_code]][i]
      if (sum(from) == 0) {
        warning(paste0("No match for line ", i))
      }
      to <- destinations@data[[zone_code_d]] %in% flow[[dest_code]][i]
      if (sum(to) == 0 & sum(from) == 1) {
        warning(paste0("No match for line ", i))
      }
      x <- sp::coordinates(zones[from, ])
      y <- sp::coordinates(destinations[to, ])
      l[[i]] <- sp::Lines(list(sp::Line(rbind(x, y))), as.character(i))
    }
  }
  l <- sp::SpatialLines(l)
  l <- sp::SpatialLinesDataFrame(l, data = flow, match.ID = FALSE)
  sp::proj4string(l) <- sp::proj4string(zones)
  l
}

#' @rdname od2line
#' @export
od2line2 <- function(flow, zones) {
  odf <- od2odf(flow, zones)
  l <- vector("list", nrow(odf))
  for (i in 1:nrow(odf)) {
    l[[i]] <- sp::Lines(list(sp::Line(rbind(c(odf$fx[i], odf$fy[i]), c(odf$tx[i], odf$ty[i])))), as.character(i))
  }
  l <- sp::SpatialLines(l)
}

#' Convert geographic line objects to a data.frame with from and to coords
#'
#' This function returns a data frame with fx and fy and tx and ty variables
#' representing the beginning and end points of spatial line features respectively.
#'
#' @param l A spatial lines object
#' @family lines
#' @export
#' @examples
#' data(flowlines)
#' line2df(flowlines[5, ]) # beginning and end of a single straight line
#' line2df(flowlines) # on multiple lines
#' line2df(routes_fast[5:6, ]) # beginning and end of routes
#' line2df(routes_fast_sf[5:6, ]) # beginning and end of routes
line2df <- function(l) {
  UseMethod("line2df")
}
#' @export
line2df.sf <- function(l) {
  X <- rlang::quo(X)
  Y <- rlang::quo(Y)
  L1 <- rlang::quo(L1)

  ldf_geom <- sf::st_coordinates(l)
  dplyr::group_by(dplyr::as_data_frame(ldf_geom), !!L1) %>%
    dplyr::summarise(
      fx = dplyr::first(!!X), fy = dplyr::first(!!Y),
      tx = dplyr::last(!!X), ty = dplyr::last(!!Y)
    )
}
#' @export
line2df.Spatial <- function(l) {
  ldf_geom <- raster::geom(l)
  dplyr::group_by_(dplyr::as_data_frame(ldf_geom), "object") %>%
    dplyr::summarise_(fx = quote(dplyr::first(x)), fy = quote(dplyr::first(y)),
	tx = quote(dplyr::last(x)), ty = quote(dplyr::last(y)))
}

#' Convert a SpatialLinesDataFrame to points
#' The number of points will be double the number of lines with `line2points`.
#' A closely related function, `line2pointsn` returns all the points that were line vertices.
#' The points corresponding with a given line, `i`, will be `(2*i):((2*i)+1)`.
#' @param l A SpatialLinesDataFrame
#' @param ids Vector of ids (by default `1:nrow(l)`)
#' @export
#' @examples
#' l <- routes_fast[2:4, ]
#' lpoints <- line_to_points(l)
#' lpoints2 <- line2pointsn(l)
#' plot(lpoints, pch = lpoints$id, cex = lpoints$id)
#' points(lpoints2, add = TRUE)
#' line_to_points(routes_fast_sf[2:4, ])
#' @aliases line2points
#' @export
line_to_points <- function(l, ids = rep(1:nrow(l), each = 2)) {
  UseMethod("line_to_points")
}
#' @export
line_to_points.sf <- function(l, ids = rep(1:nrow(l), each = 2)) {
  y_coords <- x_coords <- double(length = length(ids)) # initiate coords
  d_indices <- 1:nrow(l) * 2
  o_indices <- d_indices - 1
  x_coords[o_indices] <- sapply(l$geometry, `[[`, 1) # first (x) element of each line
  x_coords[d_indices] <- sapply(l$geometry, function(x) x[length(x) / 2]) # last (x) element of each line
  y_coords[o_indices] <- sapply(l$geometry, function(x) x[length(x) / 2 + 1]) # first (y) element of each line
  y_coords[d_indices] <- sapply(l$geometry, tail, n = 1) # last (y) element of each line
  p_multi <- sf::st_multipoint(cbind(x_coords, y_coords))
  p <- sf::st_cast(sf::st_sfc(p_multi), "POINT")
  sf::st_sf(data.frame(id = ids), p)
}
#' @export
line_to_points.Spatial <- function(l, ids = rep(1:nrow(l), each = 2)) {
  for (i in 1:length(l)) {
    lcoords <- sp::coordinates(l[i, ])[[1]][[1]]
    pmat <- matrix(lcoords[c(1, nrow(lcoords)), ], nrow = 2)
    lpoints <- sp::SpatialPoints(pmat)
    if (i == 1) {
      out <- lpoints
    } else {
      out <- raster::bind(out, lpoints)
    }
  }
  sp::proj4string(out) <- sp::proj4string(l)
  out <- sp::SpatialPointsDataFrame(coords = out, data = data.frame(id = ids))
  out
}
#' @export
line2points <- function(l) {
  for (i in 1:length(l)) {
    l1 <- l[i, ]
    lcoords <- sp::coordinates(l1)[[1]][[1]]
    lpoints <- sp::SpatialPoints(matrix(lcoords[c(1, nrow(lcoords)), ], nrow = 2))
    sp::proj4string(lpoints) <- sp::proj4string(l)
    if (i == 1) {
      out <- lpoints
    } else {
      out <- raster::bind(out, lpoints)
    }
  }
  out
}

#' @rdname line_to_points
#' @export
line2pointsn <- function(l) {
  spdf <- raster::geom(l)
  p <- sp::SpatialPoints(coords = spdf[, c("x", "y")])
  raster::crs(p) <- raster::crs(l)
  p
}
#' Convert straight OD data (desire lines) into routes
#'
#' @section Details:
#'
#' See [route_cyclestreet()] and other route functions for details.
#'
#' A parallel implementation of this was available until version 0.1.8.
#' See \href{https://github.com/ropensci/stplanr/blob/18a598674bb378d5577050178da1561489496157/R/od-funs.R}{github.com/ropensci/stplanr} for details.
#'
#'
#' @param l A SpatialLinesDataFrame
#' @param route_fun A routing function to be used for converting the straight lines to routes
#' [od2line()]
#' @param n_print A number specifying how frequently progress updates
#' should be shown
#' @param list_output If FALSE (default) assumes SpatialLinesDataFrame output. Set to TRUE to save output as a list.
#' @param l_id Character string naming the id field from the input lines data,
#' typically the origin and destination ids pasted together. If absent, the row name of the
#' straight lines will be used.
#' @param time_delay Number or seconds to wait between each query
#' @param ... Arguments passed to the routing function, e.g. [route_cyclestreet()]
#' @inheritParams route_cyclestreet
#' @family routes
#' @export
#' @examples
#' \dontrun{
#' l <- flowlines[2:5, ]
#' r <- line2route(l)
#' rf <- line2route(l = l, "route_cyclestreet", plan = "fastest")
#' rq <- line2route(l = l, plan = "quietest", silent = TRUE)
#' plot(r)
#' plot(rf, col = "red", add = TRUE)
#' plot(rq, col = "green", add = TRUE)
#' plot(l, add = T)
#' line2route(flowlines_sf[2:3, ], route_osrm)
#' # Plot for a single line to compare 'fastest' and 'quietest' route
#' n <- 2
#' plot(l[n, ])
#' lines(rf[n, ], col = "red")
#' lines(rq[n, ], col = "green")
#' # Example with list output
#' l <- l[1:3, ]
#' rf_list <- line2route(l = l, list_output = TRUE)
#' line2route(l[1, ], route_graphhopper)
#' }
line2route <- function(l, route_fun = stplanr::route_cyclestreet, n_print = 10, list_output = FALSE, l_id = NA, time_delay = 0, ...) {
  return_sf <- is(l, "sf")
  if (return_sf) {
    l <- as(l, "Spatial")
  }
  FUN <- match.fun(route_fun)
  ldf <- line2df(l)
  n_ldf <- nrow(ldf)

  error_fun <- function(e) {
    warning(paste("Fail for line number", i))
    e
  }

  rc <- as.list(rep(NA, length(l)))
  for (i in 1:n_ldf) {
    rc[[i]] <- tryCatch({
      FUN(from = c(ldf$fx[i], ldf$fy[i]), to = c(ldf$tx[i], ldf$ty[i]), ...)
    }, error = error_fun)
    perc_temp <- i %% round(n_ldf / n_print)
    # print % of distances calculated
    if (!is.na(perc_temp) & perc_temp == 0) {
      message(paste0(round(100 * i / n_ldf), " % out of ", n_ldf, " distances calculated"))
    }
    Sys.sleep(time = time_delay)
  }

  if (list_output) {
    r <- rc
  } else {
    # Set the names based on the first non failing line (then exit loop)
    for (i in 1:n_ldf) {
      if (grepl("Spatial.*DataFrame", class(rc[[i]]))[1]) {
        rdata <- data.frame(matrix(nrow = nrow(l), ncol = ncol(rc[[i]]) + 1))
        names(rdata) <- c(names(rc[[i]]), "error")
        r <- l
        r@data <- rdata
        break
      }
      Sys.sleep(time = time_delay)
    }

    # Copy rc into r including the data or copy the error into r
    for (i in 1:n_ldf) {
      if (grepl("Spatial.*DataFrame", class(rc[[i]]))[1]) {
        r@lines[[i]] <- Lines(rc[[i]]@lines[[1]]@Lines, row.names(l[i, ]))
        r@data[i, ] <- c(rc[[i]]@data, error = NA)
      } else {
        r@data[i, "error"] <- rc[[i]][1]
      }
      Sys.sleep(time = time_delay)
    }

    # Set the id in r
    l_ids <- c(l_id, "id")
    l_id <- l_ids[!is.na(l_ids)][1]
    r$id <- if (l_id %in% names(l)) {
      l@data[[l_id]]
    } else {
      row.names(l)
    }
  }
  if (return_sf) {
    r <- sf::st_as_sf(r)
  }
  r
}

#' Convert straight SpatialLinesDataFrame from flow data into routes retrying
#' on connection (or other) intermittent failures
#'
#' @section Details:
#'
#' See [line2route()] for the version that is not retried on errors.
#' @param lines A SpatialLinesDataFrame
#' @param pattern A regex that the error messages must not match to be retried, default "^Error: " i.e. do not retry errors starting with "Error: "
#' @param n_retry Number of times to retry
#' @inheritParams line2route
#' @family routes
#' @export
#' @examples
#' \dontrun{
#' data(flowlines)
#' rf_list <- line2routeRetry(flowlines[1:2, ], pattern = "nonexistanceerror", silent = F)
#' }
line2routeRetry <- function(lines, pattern = "^Error: ", n_retry = 3, ...) {
  routes <- line2route(lines, reporterrors = T, ...)

  # When the time is NA then the routing failed,
  # if there is no error message or the message matches the pattern select line to be retried
  failed_to_route <- lines[is.na(routes$time) & (is.na(routes$error) | !grepl(pattern, routes$error)), ]
  if (nrow(failed_to_route) > 0 && n_retry > 0) {
    ids <- routes$ids
    routes_retry <- line2routeRetry(failed_to_route, pattern = pattern, n_retry = n_retry - 1, ...)
    for (idx_retry in 1:nrow(routes_retry)) {
      # Merge in retried routes if they are Spatial DataFrames
      if (grepl("Spatial.*DataFrame", class(routes_retry[[idx_retry]]))) {
        idx_to_replace <- which(routes$id == routes_retry$id[idx_retry])

        routes@data[idx_to_replace, ] <- routes_retry@data[idx_retry, ]
        routes@lines[[idx_to_replace]] <- Lines(routes_retry@lines[[idx_retry]]@Lines, row.names(routes_retry[idx_retry, ]))
      }
    }
  }
  routes
}

#' Convert a series of points into a dataframe of origins and destinations
#'
#' Takes a series of geographical points and converts them into a data.frame
#' representing the potential flows, or 'spatial interaction', between every combination
#' of points.
#'
#' @param p A spatial points object
#' @family od
#' @export
#' @examples
#' data(cents)
#' df <- points2odf(cents)
#' cents_centroids <- rgeos::gCentroid(cents, byid = TRUE)
#' df2 <- points2odf(cents_centroids)
#' df3 <- points2odf(cents_sf)
points2odf <- function(p) {
  UseMethod("points2odf")
}
#' @export
points2odf.sf <- function(p) {
  odf <- data.frame(
    expand.grid(p[[1]], p[[1]])[2:1]
  )
  names(odf) <- c("O", "D")
  odf
}
#' @export
points2odf.Spatial <- function(p) {
  if (grepl(pattern = "DataFrame", class(p))) {
    geo_code <- p@data[, 1]
  } else if (is(p, "SpatialPoints")) {
    geo_code <- 1:length(p)
  } else {
    geo_code <- p[, 1]
  }
  odf <- data.frame(
    expand.grid(geo_code, geo_code)[2:1]
  )
  names(odf) <- c("O", "D")
  odf
}
#' Convert a series of points into geographical flows
#'
#' Takes a series of geographical points and converts them into a SpatialLinesDataFrame
#' representing the potential flows, or 'spatial interaction', between every combination
#' of points.
#'
#' @param p SpatialPointsDataFrame
#' @family od
#'
#' @export
#' @examples
#' data(cents)
#' plot(cents)
#' flow <- points2flow(cents)
#' plot(flow, add = TRUE)
#' flow_sf <- points2flow(cents_sf)
#' plot(flow_sf)
points2flow <- function(p) {
  odf <- points2odf(p)
  od2line(flow = odf, zones = p)
}

#' Update line geometry
#'
#' Take two SpatialLines objects and update the geometry of the former with that of the latter,
#' retaining the data of the former.
#'
#' @param l A SpatialLines object, whose geometry is to be modified
#' @param nl A SpatialLines object of the same length as `l` to provide the new geometry
#' @family lines
#'
#' @export
#' @examples
#' data(flowlines)
#' l <- flowlines[2:5, ]
#' nl <- routes_fast
#' nrow(l)
#' nrow(nl)
#' l <- l[!is_linepoint(l), ]
#' names(l)
#' names(routes_fast)
#' l_newgeom <- update_line_geometry(l, nl)
#' plot(l, lwd = l$All / mean(l$All))
#' plot(l_newgeom, lwd = l$All / mean(l$All))
#' names(l_newgeom)
update_line_geometry <- function(l, nl) {
  for (i in 1:nrow(l)) {
    l@lines[[i]] <- Lines(nl@lines[[i]]@Lines, row.names(l[i, ]))
  }
  l
}

#' Quickly calculate Euclidean distances of od pairs
#'
#' It is common to want to know the Euclidean distance between origins and destinations
#' in OD data. You can calculate this by first converting OD data to SpatialLines data,
#' e.g. with [od2line()]. However this can be slow and overkill if you just
#' want to know the distance. This function is a few orders of magnitude faster.
#'
#' Note: this function assumes that the zones or centroids in `cents` have a geographic
#' (lat/lon) CRS.
#'
#' @inheritParams od2line
#' @family od
#' @export
#' @examples
#' data(flow)
#' data(cents)
#' od_dist(flow, cents)
od_dist <- function(flow, zones) {
  omatch <- match(flow[[1]], zones@data[[1]])
  dmatch <- match(flow[[2]], zones@data[[1]])
  cents_o <- zones@coords[omatch, ]
  cents_d <- zones@coords[dmatch, ]
  geosphere::distHaversine(p1 = cents_o, p2 = cents_d)
}

#' Convert a series of points, or a matrix of coordinates, into a line
#'
#' This is a simple wrapper around [spLines()] that makes the creation of
#' `SpatialLines` objects easy and intuitive
#'
#' @param p A SpatialPoints obect or matrix representing the coordinates of points.
#' @family lines
#' @export
#' @examples
#' p <- matrix(1:4, ncol = 2)
#' library(sp)
#' l <- points2line(p)
#' plot(l)
#' l <- points2line(cents)
#' plot(l)
#' p <- line2points(routes_fast)
#' l <- points2line(p)
#' plot(l)
#' l_sf <- points2line(cents_sf)
#' plot(l_sf)
points2line <- function(p) {
  UseMethod("points2line")
}
#' @export
points2line.sf <- function(p) {
  points2flow(p = p)
}
#' @export
points2line.Spatial <- function(p) {
  if (is(p, "SpatialPoints")) {
    p_proj <- sp::proj4string(p)
    p <- sp::coordinates(p)
  } else {
    p_proj <- NA
  }
  l <- points2line(p)
  raster::crs(l) <- p_proj
  l
}
#' @export
points2line.matrix <- function(p) {
  l <- raster::spLines(p)
  l
}
