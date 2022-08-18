#' Plan routes on the transport network
#'
#' Takes origins and destinations, finds the optimal routes between them
#' and returns the result as a spatial (sf or sp) object.
#' The definition of optimal depends on the routing function used
#'
#' @inheritParams od_coords
#' @param l A spatial (linestring) object
#' @param route_fun A routing function to be used for converting the lines to routes
#' @param n_print A number specifying how frequently progress updates
#' should be shown
#' @param list_output If FALSE (default) assumes spatial (linestring) object output.
#' Set to TRUE to save output as a list.
#' @param ... Arguments passed to the routing function
#' @family routes
#' @param cl Cluster
#' @param wait How long to wait between routes?
#'   0 seconds by default, can be useful when sending requests to rate limited APIs.
#' @family routes
#' @export
#' @examples
#' # Todo: add examples
route <- function(from = NULL, to = NULL, l = NULL,
                  route_fun = cyclestreets::journey, wait = 0,
                  n_print = 10, list_output = FALSE, cl = NULL, ...) {
  UseMethod(generic = "route")
}
#' @export
route.numeric <- function(from = NULL, to = NULL, l = NULL,
                          route_fun = cyclestreets::journey, wait = 0.1,
                          n_print = 10, list_output = FALSE, cl = NULL, ...) {
  odm <- od_coords(from, to)
  l <- od_coords2line(odm)
  route(l, route_fun = route_fun, ...)
}
#' @export
route.character <- function(from = NULL, to = NULL, l = NULL,
                            route_fun = cyclestreets::journey, wait = 0.1,
                            n_print = 10, list_output = FALSE, cl = NULL, ...) {
  odm <- od_coords(from, to)
  l <- od_coords2line(odm)
  route(l, route_fun = route_fun, ...)
}
#' @export
route.sf <- function(from = NULL, to = NULL, l = NULL,
                     route_fun = cyclestreets::journey, wait = 0.1,
                     n_print = 10, list_output = FALSE, cl = NULL, ...) {
  FUN <- match.fun(route_fun)
  if (requireNamespace("opentripplanner", quietly = TRUE)) {
    if (identical(FUN, opentripplanner::otp_plan) && !is.null(l)) {
      message("Routing in batch mode with OTP")
      l_origins_sf = lwgeom::st_startpoint(l)
      l_destinations_sf = lwgeom::st_endpoint(l)
      l_origins_matrix = sf::st_coordinates(l_origins_sf)
      l_destinations_matrix = sf::st_coordinates(l_destinations_sf)
      routes_out = opentripplanner::otp_plan(
        fromPlace = l_origins_matrix,
        toPlace = l_destinations_matrix,
        ...
        )
      return(routes_out)
    }
  }

  # generate od coordinates
  ldf <- od_coords(from, to, l)
  # calculate line data frame
  if (is.null(l)) {
    l <- od_coords2line(ldf)
  }
  # Check the CRS before trying to do routing:
  # https://github.com/ropensci/stplanr/issues/474
  if(!sf::st_is_longlat(l)) {
    warning("CRS of line object is not geographic (in degrees lon/lat)")
    message("It has the following CRS: ", sf::st_crs(l))
    message("See ?st_transform() to transform its CRS, e.g. to EPSG 4326")
  }
  if (list_output) {
    if (is.null(cl)) {
      list_out <- pbapply::pblapply(1:nrow(l), function(i) route_l(FUN, ldf, i, l, ...))
    } else {
      list_out <- pbapply::pblapply(1:nrow(l), function(i) route_l(FUN, ldf, i, l, ...), cl = cl)
    }
  } else {
    if (is.null(cl)) {
      list_out <- pbapply::pblapply(1:nrow(l), function(i) route_i(FUN, ldf, wait, i, l, ...))
    } else {
      list_out <- pbapply::pblapply(1:nrow(l), function(i) route_i(FUN, ldf, wait, i, l, ...), cl = cl)
    }
  }

  list_elements_sf <- most_common_class_of_list(list_out, "sf")
  if (sum(list_elements_sf) < length(list_out)) {
    failing_routes <- which(!list_elements_sf)
    message("These routes failed: ", paste0(failing_routes, collapse = ", "))
    message("The first of which was:")
    print(list_out[[failing_routes[1]]])
  }
  if (list_output | !any(list_elements_sf)) {
    message("Returning list")
    return(list_out)
  }
  if (requireNamespace("data.table", quietly = TRUE)) {
    # browser()
    # warning("data.table used to create the sf object, bounding box may be incorrect.")
    out_dt <- data.table::rbindlist(list_out[list_elements_sf])
    out_dtsf <- sf::st_sf(out_dt[, !"geometry"], geometry = out_dt$geometry)
    # attributes(out_dtsf$geometry)
    # identical(sf::st_bbox(out_dtsf), sf::st_bbox(out_sf)) # FALSE
    attr(out_dtsf$geometry, "bbox") = sfheaders::sf_bbox(out_dtsf)
    # identical(sf::st_bbox(out_dtsf), sf::st_bbox(out_sf)) # TRUE
    return(out_dtsf)
  } else {
    out_sf <- do.call(rbind, list_out[list_elements_sf])
    out_sf
  }
}

# output sf objects
route_i <- function(FUN, ldf, wait, i, l, ...) {
  Sys.sleep(wait)
  error_fun <- function(e) {
    e
  }
  tryCatch(
    {
      single_route <- FUN(ldf[i, 1:2], ldf[i, 3:4], ...)
      sf::st_sf(cbind(
        sf::st_drop_geometry(l[rep(i, nrow(single_route)), ]),
        route_number = i,
        sf::st_drop_geometry(single_route)
      ),
      geometry = single_route$geometry
      )
    },
    error = error_fun
  )
}

# output whatever the routing function returns
route_l <- function(FUN, ldf, i, l, ...) {
  error_fun <- function(e) {
    e
  }
  tryCatch(
    {
      single_route <- FUN(ldf[i, 1:2], ldf[i, 3:4], ...)
    },
    error = error_fun
  )
}

most_common_class_of_list <- function(l, class_to_find = "sf") {
  class_out <- sapply(l, function(x) class(x)[1])
  most_common_class <- names(sort(table(class_out), decreasing = TRUE)[1])
  message("Most common output is ", most_common_class)
  is_class <- class_out == class_to_find
  is_class
}
