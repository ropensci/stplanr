#' Plan routes on the transport network
#'
#' Takes origins and destinations, finds the optimal routes between them
#' and returns the result as a spatial (sf or sp) object.
#' The definition of optimal depends on the routing function used
#'
#' @inheritParams od_coords
#' @inheritParams line2route
#' @export
#' @examples \dontrun{
#' from = "Leek, UK"
#' to = "Hereford, UK"
#' route_leek_to_hereford = route(from, to)
#' route(cents_sf[1:3, ], cents_sf[2:4, ]) # sf points
#' route(flowlines_sf[2:4, ]) # lines
#' }
route <- function(from = NULL, to = NULL, l = NULL,
                       route_fun = route_cyclestreet,
                       n_print = 10, list_output = FALSE, ...){

  # generate od coordinates
  FUN <- match.fun(route_fun)
  ldf <- od_coords(from, to, l) %>%
    dplyr::as_data_frame()

  error_fun <- function(e){
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

  if(nrow(ldf) > 1) {
    for(i in 2:nrow(ldf)){
      rc[[i]] <- tryCatch({
        FUN(from = c(ldf$fx[i], ldf$fy[i]), to = c(ldf$tx[i], ldf$ty[i]), ...)
      }, error = error_fun)
      perc_temp <- i %% round(nrow(ldf) / n_print)
      # print % of distances calculated
      if(!is.na(perc_temp) & perc_temp == 0) {
        message(paste0(round(100 * i/nrow(ldf)), " % out of ", nrow(ldf), " distances calculated"))
      }

      rdf[i, ] <- rc[[i]]@data[1, ]
      rg[i] <- sf::st_as_sf(rc[[i]])$geometry

    }
  }

  r <- sf::st_sf(geometry = rg, rdf)

  if(list_output) {
    r <- rc
  }

  r
}
