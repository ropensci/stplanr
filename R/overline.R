#' Do the intersections between two geometries create lines?
#'
#' This is a function required in [overline()]. It identifies
#' whether sets of lines overlap (beyond shared points) or
#' not.
#'
#' @param g1 A spatial object
#' @param g2 A spatial object
#' @family rnet
#' @export
#' @examples
#' \dontrun{
#' rnet <- overline(routes_fast[c(2, 3, 22), ], attrib = "length")
#' plot(rnet)
#' lines(routes_fast[22, ], col = "red") # line without overlaps
#' islines(routes_fast[2, ], routes_fast[3, ])
#' islines(routes_fast[2, ], routes_fast[22, ])
#' # sf implementation
#' islines(routes_fast_sf[2, ], routes_fast_sf[3, ])
#' islines(routes_fast_sf[2, ], routes_fast_sf[22, ])
#' }
islines <- function(g1, g2) {
  UseMethod("islines")
}
islines.Spatial <- function(g1, g2) {
  ## return TRUE if geometries intersect as lines, not points
  inherits(rgeos::gIntersection(g1, g2), "SpatialLines")
}
islines.sf <- function(g1, g2) {
  sf::st_geometry_type(sf::st_intersection(sf::st_geometry(g1), sf::st_geometry(g2))) == "MULTILINESTRING"
}

#' Function to split overlapping SpatialLines into segments
#'
#' Divides SpatialLinesDataFrame objects into separate Lines.
#' Each new Lines object is the aggregate of a single number
#' of aggregated lines.
#'
#' @param sl SpatialLinesDataFrame with overlapping Lines to split by
#' number of overlapping features.
#' @param buff_dist A number specifying the distance in meters of the buffer to be used to crop lines before running the operation.
#' If the distance is zero (the default) touching but non-overlapping lines may be aggregated.
#' @family rnet
#' @export
#' @examples
#' sl <- routes_fast[2:4, ]
#' rsec <- gsection(sl)
#' rsec_buff <- gsection(sl, buff_dist = 1)
#' plot(sl[1], lwd = 9, col = 1:nrow(sl))
#' plot(rsec, col = 5 + (1:length(rsec)), add = TRUE, lwd = 3)
#' plot(rsec_buff, col = 5 + (1:length(rsec_buff)), add = TRUE, lwd = 3)
#' \dontrun{
#' sl <- routes_fast_sf[2:4, ]
#' rsec <- gsection(sl)
#' rsec <- gsection(sl, buff_dist = 100) # 4 features: issue
#' }
gsection <- function(sl, buff_dist = 0) {
  UseMethod("gsection")
}
#' @export
gsection.Spatial <- function(sl, buff_dist = 0) {
  if (buff_dist > 0) {
    sl <- geo_toptail(sl, toptail_dist = buff_dist)
  }
  overlapping <- rgeos::gOverlaps(sl, byid = T)
  u <- rgeos::gUnion(sl, sl)
  u_merged <- rgeos::gLineMerge(u)
  sp::disaggregate(u_merged)
}
#' @export
gsection.sf <- function(sl, buff_dist = 0) {
  if (buff_dist > 0) {
    sl <- geo_toptail(sl, toptail_dist = buff_dist)
  }

  u <- sf::st_union(sl)
  u_merged <- sf::st_line_merge(u)
  u_disag <- sf::st_cast(u_merged, "LINESTRING")

  u_disag
}
#' Label SpatialLinesDataFrame objects
#'
#' This function adds labels to lines plotted using base graphics. Largely
#' for illustrative purposes, not designed for publication-quality
#' graphics.
#'
#' @param sl A SpatialLinesDataFrame with overlapping elements
#' @param attrib A text string corresponding to a named variable in `sl`
#'
#' @author Barry Rowlingson
#' @family rnet
#'
#' @export
lineLabels <- function(sl, attrib) {
  text(sp::coordinates(
    rgeos::gCentroid(sl, byid = TRUE)
  ), labels = sl[[attrib]])
}

#' Convert series of overlapping lines into a route network
#'
#' This function takes a series of Lines stored in a
#'  `SpatialLinesDataFrame`
#' and converts these into a single route network.
#'
#' @param sl A SpatialLinesDataFrame with overlapping elements
#' @param attrib A character vector corresponding to the variables in
#' `sl$` on which the function(s) will operate.
#' @param fun The function(s) used to aggregate the grouped values (default: sum).
#' If length of `fun` is smaller than `attrib` then the functions are
#' repeated for subsequent attributes.
#' @param na.zero Sets whether aggregated values with a value of zero are removed.
#' @inheritParams gsection
#' @author Barry Rowlingson
#' @references
#' Rowlingson, B (2015). Overlaying lines and aggregating their values for
#'  overlapping segments. Reproducible question from
#'  <http://gis.stackexchange.com>. See <http://gis.stackexchange.com/questions/139681/overlaying-lines-and-aggregating-their-values-for-overlapping-segments>.
#' @family rnet
#' @export
#' @examples
#' sl <- routes_fast[2:4, ]
#' rnet1 <- overline(sl = sl, attrib = "length")
#' rnet2 <- overline(sl = sl, attrib = "length", buff_dist = 1)
#' plot(rnet1, lwd = rnet1$length / mean(rnet1$length))
#' plot(rnet2, lwd = rnet2$length / mean(rnet2$length))
#' \dontrun{
#' # sf methods
#' sl <- routes_fast_sf[2:4, ]
#' overline(sl = sl, attrib = "length", buff_dist = 10)
#' rnet_sf <- overline(routes_fast_sf, attrib = "length", buff_dist = 10)
#' plot(rnet_sf$geometry, lwd = rnet_sf$length / mean(rnet_sf$length))
#' }
overline <- function(sl, attrib, fun = sum, na.zero = FALSE, buff_dist = 0) {
  UseMethod("overline")
}
#' @export
overline.sf <- function(sl, attrib, fun = sum, na.zero = FALSE, buff_dist = 0) {
  sl_spatial <- sp::SpatialLinesDataFrame(sl = sf::as_Spatial(sl$geometry), data = sf::st_set_geometry(sl, NULL), match.ID = FALSE)
  rnet_sp <- overline(sl_spatial, attrib, fun = fun, na.zero = na.zero, buff_dist = buff_dist)
  sf::st_as_sf(rnet_sp)

  # attempt to run with sf funs
  # slu <- gsection(sl, buff_dist = buff_dist)
  # aggs <- aggregate(sl[attrib], slu, FUN = fun, join = sf::st_overlaps, na.rm = FALSE)
  # aggs
}
#' @export
overline.Spatial <- function(sl, attrib, fun = sum, na.zero = FALSE, buff_dist = 0) {
  fun <- c(fun)
  if (length(fun) < length(attrib)) {
    fun <- rep(c(fun), length.out = length(attrib))
  }

  sl_sp <- as(sl, "SpatialLines")

  ## get the line sections that make the network
  slu <- gsection(sl, buff_dist = buff_dist)
  ## overlay network with routes
  overs <- sp::over(slu, sl_sp, returnList = TRUE)
  ## overlay is true if end points overlay, so filter them out:
  overs <- lapply(1:length(overs), function(islu) {
    Filter(function(isl) {
      islines(sl_sp[isl, ], slu[islu, ])
    }, overs[[islu]])
  })
  ## now aggregate the required attribibute using fun():
  # aggs = sapply(overs, function(os){fun(sl[[attrib]][os])})
  aggs <- setNames(
    as.data.frame(
      lapply(
        1:length(attrib),
        function(y, overs, attribs, aggfuns) {
          sapply(overs, function(os, attrib, fun2) {
            fun2(sl[[attrib]][os])
          },
          attrib = attribs[y],
          fun2 = aggfuns[[y]]
          )
        },
        overs,
        attrib,
        fun
      )
    ),
    attrib
  )

  ## make a sl with the named attribibute:
  sl <- sp::SpatialLinesDataFrame(slu, aggs)
  # names(sl) = attrib

  ## remove lines with attribute values of zero
  if (na.zero) {
    sl <- sl[sl[[attrib]] > 0, ]
  }

  sl
}

#' Aggregate flows so they become non-directional (by geometry - the slow way)
#'
#' Flow data often contains movement in two directions: from point A to point B
#' and then from B to A. This can be problematic for transport planning, because
#' the magnitude of flow along a route can be masked by flows the other direction.
#' If only the largest flow in either direction is captured in an analysis, for
#' example, the true extent of travel will by heavily under-estimated for
#' OD pairs which have similar amounts of travel in both directions.
#' Flows in both direction are often represented by overlapping lines with
#' identical geometries (see [flowlines()]) which can be confusing
#' for users and are difficult to plot.
#'
#' This function aggregates directional flows into non-directional flows,
#' potentially halving the number of lines objects and reducing the number
#' of overlapping lines to zero.
#'
#' @param x A SpatialLinesDataFrame
#' @param attrib A text string containing the name of the line's attribute to
#' aggregate or a numeric vector of the columns to be aggregated
#'
#' @return `onewaygeo` outputs a SpatialLinesDataFrame with single lines
#' and user-selected attribute values that have been aggregated. Only lines
#' with a distance (i.e. not intra-zone flows) are included
#' @family lines
#' @export
#' @examples
#' plot(flowlines[1:30, ], lwd = flowlines$On.foot[1:30])
#' singlines <- onewaygeo(flowlines[1:30, ], attrib = which(names(flowlines) == "On.foot"))
#' plot(singlines, lwd = singlines$On.foot / 2, col = "red", add = TRUE)
#' \dontrun{
#' plot(flowlines, lwd = flowlines$All / 10)
#' singlelines <- onewaygeo(flowlines, attrib = 3:14)
#' plot(singlelines, lwd = singlelines$All / 20, col = "red", add = TRUE)
#' sum(singlelines$All) == sum(flowlines$All)
#' nrow(singlelines)
#' singlelines_sf <- onewaygeo(flowlines_sf, attrib = 3:14)
#' sum(singlelines_sf$All) == sum(flowlines_sf$All)
#' summary(singlelines$All == singlelines_sf$All)
#' }
onewaygeo <- function(x, attrib) {
  UseMethod("onewaygeo")
}
#' @export
onewaygeo.sf <- function(x, attrib) {
  geq <- sf::st_equals(x, x, sparse = FALSE) | sf::st_equals_exact(x, x, sparse = FALSE, par = 0.0)
  sel1 <- !duplicated(geq) # repeated rows
  x$matching_rows <- apply(geq, 1, function(x) paste0(formatC(which(x), width = 4, format = "d", flag = 0), collapse = "-"))

  singlelines <- stats::aggregate(x[attrib], list(x$matching_rows), FUN = sum)

  return(singlelines)
}
#' @export
onewaygeo.Spatial <- function(x, attrib) {
  geq <- rgeos::gEquals(x, x, byid = TRUE) | rgeos::gEqualsExact(x, x, byid = TRUE)
  sel1 <- !duplicated(geq) # repeated rows
  singlelines <- x[sel1, ]
  non_numeric_cols <- which(!sapply(x@data, is.numeric))
  keeper_cols <- sort(unique(c(non_numeric_cols, attrib)))

  singlelines@data[, attrib] <- (matrix(
    unlist(
      lapply(
        apply(geq, 1, function(x) {
          which(x == TRUE)
        }),
        function(y, x) {
          colSums(x[y, attrib]@data)
        }, x
      )
    ),
    nrow = nrow(x),
    byrow = TRUE
  ))[sel1, ]

  singlelines@data <- singlelines@data[keeper_cols]

  return(singlelines)
}

#' Overlay duplicated lines
#' @param x SF data frame of linestrings
#' @param attrib character, column name to be summed
#' @param ncores integer, how many cores to use in parallel processing
#' @param simplify, logical, if TRUE group final segments back into lines
#' @param regionalise, integer, during simplification regonalisation is used if the number of segments exceeds this value
#' @family rnet
#' @export
#' @examples
#' sl = routes_fast_sf[routes_fast_sf$length > 0, ]
#' sl$bicycle = 1
#' system.time({rnet1 = overline2(sl, "bicycle")})
#' system.time({rnet2 = overline2(sl, "bicycle", ncores = 4)})
#' identical(rnet1, rnet2)
#' lwd = rnet1$bicycle / mean(rnet1$bicycle)
#' plot(rnet1, lwd = lwd)
#' \donttest{
#' region = "isle-of-wight"
#'
#' u = paste0(
#'   "https://github.com/npct/pct-outputs-regional-notR/raw/master/commute/msoa/",
#'    region,
#'   "/rf.geojson"
#' )
#'
#' sl = sf::read_sf(u)
#' system.time({rnet1 = overline2(sl, "bicycle")})
#' system.time({rnet2 = overline2(sl, "bicycle", ncores = 4)})
#' identical(rnet1, rnet2)
#' lwd = rnet1$bicycle / mean(rnet1$bicycle)
#' plot(rnet1, lwd = lwd)
#' }
overline2 <- function(x, attrib, ncores = 1, simplify = TRUE, regionalise = 1e4) {

  # Defensive programming checks:
  if (all(sf::st_geometry_type(x) != "LINESTRING")) {
    message("Only LINESTRING is supported")
    stop()
  }
  if ("matchingID" %in% names(x)) {
    message("matchingID is not a permitted column name, please rename that column")
    stop()
  }
  if(!requireNamespace("pbapply")) {
    stop("Install pbapply to use this function")
  }

  x <- x[, attrib]
  x_crs <- sf::st_crs(x)

  message(paste0(Sys.time(), " constructing segments"))
  c1 <- sf::st_coordinates(x) # Convert SF to matrix
  l1 <- c1[, 3] # Get which line each point is part of
  c1 <- c1[, 1:2]
  l1_start <- duplicated(l1) # find the break points between lines
  l1_start <- c(l1_start[2:length(l1)], FALSE)
  c2 <- c1[2:nrow(c1), 1:2] # Create new coords offset by one row
  c2 <- rbind(c2, c(NA, NA))
  c2[nrow(c1), ] <- c(NA, NA)
  c2[!l1_start, 1] <- NA
  c2[!l1_start, 2] <- NA
  c3 <- cbind(c1, c2) # make new matrix of start and end coords
  rm(c1, c2)
  c3 <- c3[!is.na(c3[, 3]), ]
  message(paste0(Sys.time(), " transposing 'B to A' to 'A to B'"))
  c3 <- split(c3, 1:nrow(c3))
  c3 <- pbapply::pblapply(c3, function(y) {
    if (y[1] != y[3]) {
      if (y[1] > y[3]) {
        c(y[3], y[4], y[1], y[2])
      } else {
        y
      }
    } else {
      if (y[2] > y[4]) {
        c(y[3], y[4], y[1], y[2])
      } else {
        y
      }
    }
  })
  c3 <- matrix(unlist(c3), byrow = TRUE, nrow = length(c3))
  message(paste0(Sys.time(), " removing duplicates"))
  c3_dup <- duplicated(c3) # de-duplicate
  c3_nodup <- c3[!c3_dup, ]
  c3_df <- as.data.frame(c3)
  names(c3_df) <- c("X1", "Y1", "X2", "Y2")
  c3_nodup_df <- as.data.frame(c3_nodup)
  names(c3_nodup_df) <- c("X1", "Y1", "X2", "Y2")
  c3_nodup_df$matchID <- 1:nrow(c3_nodup_df)
  matchID <- dplyr::left_join(c3_df,
    c3_nodup_df,
    by = c(
      "X1" = "X1",
      "Y1" = "Y1",
      "X2" = "X2",
      "Y2" = "Y2"
    )
  )
  matchID <- matchID$matchID
  rm(c3_df, c3_nodup_df, c3, c3_dup)

  # Calcualte the attributes
  message(paste0(Sys.time(), " restructuring attributes"))
  x_split <- x # extract attributes
  sf::st_geometry(x_split) <- NULL
  x_split <- as.data.frame(x_split)
  x_split <- x_split[l1[l1_start], , drop = FALSE] # repeate attriibutes
  x_split$matchingID <- matchID
  x_split <- x_split %>%
    dplyr::group_by(matchingID) %>%
    dplyr::summarise_all(dplyr::funs(sum), na.rm = TRUE) %>%
    dplyr::arrange(matchingID)
  x_split$matchingID <- NULL

  # Make Geometry
  message(paste0(Sys.time(), " building geometry"))
  geoms <- pbapply::pblapply(1:nrow(c3_nodup), function(y) {
    sf::st_linestring(matrix(c3_nodup[y, ], ncol = 2, byrow = T))
  })
  rm(c3_nodup, l1, l1_start, matchID)
  geoms <- sf::st_as_sfc(geoms, crs = sf::st_crs(x))
  sf::st_geometry(x_split) <- geoms # put together
  rm(geoms, x)

  # Recombine into fewer lines
  if (simplify) {
    message(paste0(Sys.time(), " simplifying geometry"))
    if (nrow(x_split) > regionalise) {
      message(paste0("large data detected, using regionalisation"))
      suppressWarnings(cents <- sf::st_centroid(x_split))
      grid <- sf::st_make_grid(cents, what = "polygons")
      inter <- unlist(sf::st_intersects(cents, grid))
      x_split$grid <- inter
      rm(cents, grid, inter)
      # split into a list of df by grid
      x_split <- split(x_split, f = x_split$grid)
      message(paste0(Sys.time(), " regionalisation complete"))
      if (ncores > 1) {
        cl <- parallel::makeCluster(ncores)
        parallel::clusterExport(
          cl = cl,
          varlist = c("attrib"),
          envir = environment()
        )
        parallel::clusterEvalQ(cl, {
          library(sf)
          library(dplyr)
        })
        overlined_simple <- pbapply::pblapply(x_split, function(y) {
          y %>% dplyr::group_by_at(attrib) %>% dplyr::summarise()
        }, cl = cl)
        parallel::stopCluster(cl)
        rm(cl)
      } else {
        overlined_simple <- pbapply::pblapply(x_split, function(y) {
          y %>% dplyr::group_by_at(attrib) %>% dplyr::summarise()
        })
      }
      rm(x_split)
      suppressWarnings(overlined_simple <-
        dplyr::bind_rows(overlined_simple))
      overlined_simple <- as.data.frame(overlined_simple)
      overlined_simple <- sf::st_sf(overlined_simple)
      sf::st_crs(overlined_simple) <- x_crs
      overlined_simple$grid <- NULL
    } else {
      overlined_simple <- x_split %>%
        dplyr::group_by_at(attrib) %>%
        dplyr::summarise()
      rm(x_split)
    }

    # Separate our the linestrings and the mulilinestrings
    message(paste0(Sys.time(), " rejoining segments into linestrings"))
    geom_types <- sf::st_geometry_type(overlined_simple)
    overlined_simple_l <- overlined_simple[geom_types == "LINESTRING", ]
    overlined_simple_ml <- overlined_simple[geom_types == "MULTILINESTRING", ]
    rm(overlined_simple, geom_types)
    overlined_simple_ml <- sf::st_line_merge(overlined_simple_ml)
    suppressWarnings(overlined_simple_ml <-
      sf::st_cast(
        sf::st_cast(overlined_simple_ml, "MULTILINESTRING"),
        "LINESTRING"
      ))

    overlined_fin <- rbind(overlined_simple_l, overlined_simple_ml)

    return(overlined_fin)
  } else {
    return(x_split)
  }
}

# devtools::install_github("ropensci/stplanr", "refactor-overlineII")
# library(stplanr)
# sl = routes_fast_sf[!is.na(routes_fast_sf$length), ]
# sl$bicycle = 1
# system.time({rnet1 = overline2(sl, "bicycle")})
# system.time({rnet2 = overline2(sl, "bicycle", ncores = 4)})
# plot(rnet1, lwd = rnet1$bicycle)
# nrow(rnet1)
# rnet1_2 = rnet1[rnet1$bicycle == 2, ]
# plot(rnet1_2)
# plot(rnet1_2[7, ])
