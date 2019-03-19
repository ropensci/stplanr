#' Plan routes on the transport network
#'
#' Takes origins and destinations, finds the optimal routes between them
#' and returns the result as a spatial (sf or sp) object.
#' The definition of optimal depends on the routing function used
#'
#' @inheritParams od_coords
#' @inheritParams line2route
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
#' @export
#' @examples
#' # from <- matrix(stplanr::geo_code("pedallers arms leeds"), ncol = 2)
#' from <- structure(c(-1.5327711, 53.8006988), .Dim = 1:2)
#' # to <- matrix(stplanr::geo_code("gzing"), ncol = 2)
#' to <- structure(c(-1.527937, 53.8044309), .Dim = 1:2)
#' pts <- rbind(from, to)
#' colnames(pts) = c("X", "Y")
#' # net <- dodgr::dodgr_streetnet(pts = pts, expand = 0.1)
#' # osm_net_example <- net[c("highway", "name", "lanes", "maxspeed")]
#' r <- route_dodgr(from, to, net = osm_net_example)
#' plot(net$geometry)
#' plot(r, add = TRUE, col = "red", lwd = 5)
route_dodgr <-
  function(from = NULL,
           to = NULL,
           l = NULL,
           net = NULL
           # ,
           # return_net = FALSE

           ) {

  if(is.numeric(from) & is.numeric(to)) {
    from_coords <- from
    to_coords <- to
  }

  # Try to get route network if net not provided
  pts <- rbind(from_coords, to_coords)
  if(is.null(net)) {
    net <- dodgr::dodgr_streetnet(pts = pts, expand = 0.2)
  }

  suppressWarnings(
    suppressMessages(
      ways_dg <- dodgr::weight_streetnet(net)
      )
  )

  verts <- dodgr::dodgr_vertices(ways_dg) # the vertices or points for routing
  knf <- nabor::knn(cbind(verts$x, verts$y), matrix(from_coords, ncol = 2), k = 1)
  knt <- nabor::knn(cbind(verts$x, verts$y), matrix(to_coords, ncol = 2), k = 1)
  dp <- dodgr::dodgr_paths(ways_dg, from = verts$id[knf$nn.idx], to = verts$id[knt$nn.idx])
  path <- verts[match(dp[[1]][[1]], verts$id), ]
  path_linestring <- sf::st_linestring(cbind(path$x, path$y))
  path_sf <- sf::st_sf(sf::st_sfc(path_linestring), crs = 4326)
  # plot(net$geometry) # in there for the fun of it (to be removed)
  # plot(path_linestring, col = "red", lwd = 5, add = TRUE)
  path_sf

  # clever routing code

}
