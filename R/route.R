#' Plan routes on the transport network
#'
#' Takes origins and destinations, finds the optimal routes between them
#' and returns the result as a spatial (sf or sp) object.
#' The definition of optimal depends on the routing function used
#'
#' @inheritParams od_coords
#' @inheritParams line2route
#' @family routes
#' @export
#' @examples
#' \dontrun{
#' from <- "Leek, UK"
#' to <- "Hereford, UK"
#' route_leek_to_hereford <- route(from, to)
#' route(cents_sf[1:3, ], cents_sf[2:4, ]) # sf points
#' route(flowlines_sf[2:4, ]) # lines
#' }
route <- function(from = NULL, to = NULL, l = NULL,
                  route_fun = stplanr::route_cyclestreet,
                  n_print = 10, list_output = FALSE, ...) {

  # generate od coordinates
  FUN <- match.fun(route_fun)
  ldf <- od_coords(from, to, l) %>%
    dplyr::as_data_frame()

  error_fun <- function(e) {
    warning(paste("Fail for line number", i))
    e
  }

  # pre-allocate objects
  rc <- as.list(rep(NA, nrow(ldf)))
  rg <- sf::st_sfc(lapply(1:nrow(ldf), function(x)
    sf::st_linestring(matrix(as.numeric(NA), ncol = 2))))

  rc[[1]] <- FUN(from = c(ldf$fx[1], ldf$fy[1]), to = c(ldf$tx[1], ldf$ty[1]), ...)
  rdf <- dplyr::as_data_frame(matrix(ncol = ncol(rc[[1]]@data), nrow = nrow(ldf)))
  names(rdf) <- names(rc[[1]])

  rdf[1, ] <- rc[[1]]@data[1, ]
  rg[1] <- sf::st_as_sfc(rc[[1]])

  if (nrow(ldf) > 1) {
    for (i in 2:nrow(ldf)) {
      rc[[i]] <- tryCatch({
        FUN(from = c(ldf$fx[i], ldf$fy[i]), to = c(ldf$tx[i], ldf$ty[i]), ...)
      }, error = error_fun)
      perc_temp <- i %% round(nrow(ldf) / n_print)
      # print % of distances calculated
      if (!is.na(perc_temp) & perc_temp == 0) {
        message(paste0(round(100 * i / nrow(ldf)), " % out of ", nrow(ldf), " distances calculated"))
      }

      rdf[i, ] <- rc[[i]]@data[1, ]
      rg[i] <- sf::st_as_sf(rc[[i]])$geometry
    }
  }

  r <- sf::st_sf(geometry = rg, rdf)

  if (list_output) {
    r <- rc
  }

  r
}

#' Route on local data using the dodgr package
#'
#' @inheritParams route
#' @param net sf object representing the route network
#' @family routes
#' @export
#' @examples
#' # from <- geo_code("pedallers arms leeds")
#' from <- c(-1.5327, 53.8006)
#' # to <- geo_code("gzing")
#' to <- c(-1.5279, 53.8044)
#' # next 4 lines recreate `stplanr::osm_net_example`
#' # pts <- rbind(from, to)
#' # colnames(pts) <- c("X", "Y")
#' # net <- dodgr::dodgr_streetnet(pts = rbind(from, to), expand = 0.1)
#' # osm_net_example <- net[c("highway", "name", "lanes", "maxspeed")]
#' r <- route_dodgr(from, to, net = osm_net_example)
#' plot(osm_net_example$geometry)
#' plot(r$geometry, add = TRUE, col = "red", lwd = 5)
route_dodgr <-
  function(from = NULL,
           to = NULL,
           l = NULL,
           net = NULL
           # ,
           # return_net = FALSE
           ) {

  od_coordinate_matrix <- od_coords(from, to)
  fm_coords <- od_coordinate_matrix[, 1:2, drop = FALSE]
  to_coords <- od_coordinate_matrix[, 3:4, drop = FALSE]

  # Try to get route network if net not provided
  if(is.null(net)) {
      pts <- rbind(fm_coords, to_coords)
      net <- dodgr::dodgr_streetnet(pts = pts, expand = 0.2)
  }

  suppressMessages (
    ways_dg <- dodgr::weight_streetnet(net)
  )

  verts <- dodgr::dodgr_vertices(ways_dg) # the vertices or points for routing
  #suppressMessages ({
    from_id <- verts$id[dodgr::match_pts_to_graph(verts, fm_coords)]
    to_id <- verts$id[dodgr::match_pts_to_graph(verts, to_coords)]
  #})
  dp <- dodgr::dodgr_paths(ways_dg, from = from_id, to = to_id)
  paths <- lapply(dp, function (i)
                   lapply(i, function (j) {
                             res <- verts[match (j, verts$id), c("x", "y")]
                             sf::st_linestring(as.matrix(res))
    }))
  nms <- unlist(lapply(paths, function (i) names (i)))
  from_to <- do.call(rbind, strsplit(nms, "-"))
  from_xy <- fm_coords[match(from_to[, 1], unique(from_to[, 1])), , drop = FALSE]
  to_xy <- fm_coords[match(from_to[, 2], unique(from_to[, 2])), , drop = FALSE]

  paths <- sf::st_sfc(unlist(paths, recursive = FALSE), crs = 4326)
  sf::st_sf(from = from_to[, 1],
            from_x = from_xy [, 1],
            from_y = from_xy [, 2],
            to = from_to[, 2],
            to_x = to_xy [, 1],
            to_y = to_xy [, 2],
            geometry = paths)
}
