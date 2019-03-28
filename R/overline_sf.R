#' Convert series of overlapping lines into a route network
#'
#' This function takes overlapping `LINESTRING`s stored in an
#' `sf` object and returns a route network composed of non-overlapping
#' geometries and aggregated values.
#'
#' @param sl An `sf` `LINESTRING` object with overlapping elements
#' @inheritParams overline
#' @export
#' @examples \dontrun{
#' routes_fast_sf$value = 1
#' sl <- routes_fast_sf[4:6, ]
#' rnet = overline_sf(sl = sl, attrib = c("value", "length"))
#' plot(rnet, lwd = rnet$value)
#' # A larger example
#' sl <- routes_fast_sf[4:7, ]
#' rnet = overline_sf(sl = sl, attrib = c("value", "length"))
#' plot(rnet, lwd = rnet$value)
#' rnet_sf <- overline(routes_fast_sf[4:7, ], attrib = c("value", "length"), buff_dist = 10)
#' plot(rnet_sf, lwd = rnet$value)
#' }
overline_sf <- function(sl, attrib, fun = sum) {
  # initiate line to join-on
  rnet = sl[1, attrib]
  for(i in 2:nrow(sl)) {
    # relate_rnet = sf::st_relate(rnet, sl[i, ], pattern = "1F1F00102")
    # sel_overlaps = lengths(relate_rnet) > 0
    sel_overlaps = overlaps(rnet, sl[i, ])
    if(!any(sel_overlaps)) {
      rnet = rbind(rnet, sl[i, attrib])
    } else {
      rnet_no_overlap = rnet[!sel_overlaps, ]
      rnet_overlap = rnet[sel_overlaps, ]
      if(sum(sel_overlaps) == 1) {
        rnet_new = overline_sf2(rnet_overlap, sl[i, ], attrib = attrib, fun = fun)
      } else {
        message(sum(sel_overlaps), " intersections for route ", i)
        rnet_multi = overline_sf2(rnet_overlap[1, ], sl[i, ], attrib = attrib, fun = fun)
        for(j in 2:sum(sel_overlaps)) {
          rnet_multi_new = overline_sf2(rnet_multi[j, ], sl[i, ], attrib = attrib, fun = fun)
          rnet_multi = rbind(rnet_multi, rnet_multi_new)
        }
        rnet_new = overline_sf2(rnet_overlap[1,], sl[i,], attrib = attrib, fun = fun)
        # sel_overlaps2 = overlaps(rnet_new, sl[i, ])
        # rnet_new = rnet_new_over[sel_overlaps2, ]
        # plot(rnet_new$geometry, lwd = rnet_new$value * 5)
        # plot(rnet_new$geometry, lwd = rnet_new$value * 5)
        # sl$geometry[i] = rnet_new$geometry[!is_linestring(rnet_new)]
        # for(j in 2:nrow(rnet_overlap)) {
        #   rnet_new_over = overline_sf2(rnet_overlap[j, ], sl[i, ], attrib = attrib, fun = fun)
        #   sel_overlaps2 = overlaps(rnet_new_over, sl[i, ])
        #   rnet_new2 = rnet_new_over[sel_overlaps2, ]
        #   rnet_new = rbind(rnet_new, rnet_new2)
        #   sl$geometry[i] = sf::st_difference(sl$geometry[i], rnet_new$geometry)
        # }
      }
      rnet = rbind(rnet, rnet_new)
    }
    # plot(rnet, lwd = rnet$value, main = "yes")
    # Sys.sleep(1)
  }
  # test for overlaps (to comment-out)
  rnet_overlaps = sf::st_relate(rnet)
  if(length(unique(as.vector(rnet_overlaps))) > 3) message("Unwanted relation")
  # rnet_overlaps = matrix(as.numeric(as.factor(rnet_overlaps)), nrow = 10)
  return(rnet)
}
#' Combine 2 `sf` `LINESTRING` objects into non-overlapping segments and
#' find the aggregate value of selected columns defined by `attrib` along shared segments.
#' @inheritParams overline
#' @param sl2 A second `LINESTRING` object to aggregate
#' @param return_linestring Can the result include `MULTILINESTRING` geometries?
#' Default: `FALSE`
#' @export
#' @examples
#' routes_fast_sf$value = 1
#' sl = routes_fast_sf[2, ]
#' sl2 = routes_fast_sf[3, ]
#' rnet = overline_sf2(sl, sl2, attrib = "value")
#' unique(sf::st_geometry_type(rnet))
#' plot(rnet, lwd = rnet$value)
#' rnet2 = overline_sf2(sl, sl2, attrib = c("value", "length"))
#' plot(rnet2)
#' rnet = overline_sf(routes_fast_sf[2:6, "value"], "value")
#' plot(rnet$geometry, lwd = rnet$value)
#' plot(routes_fast_sf[7, ], add = TRUE, col = "red")
#' sel_o = overlaps(rnet, routes_fast_sf[7, ])
#' rnet_sub = rnet[sel_o, ]
#' plot(rnet_sub$geometry, lwd = rnet_sub$value)
#' plot(routes_fast_sf[7, ], add = TRUE, col = "red")
#' rnet_new = overline_sf2(rnet_sub[1, ], routes_fast_sf[7, ])
#' plot(rnet_new$geometry, lwd = 8)
#' plot(rnet_new[1, ], add = TRUE, col = "red")
#' plot(rnet_new[2, ], add = TRUE, col = "red")
#' plot(rnet_new[3, ], add = TRUE, col = "red")
#' plot(rnet_new[4, ], add = TRUE, col = "red")
overline_sf2 = function(sl, sl2, attrib = "value", fun = sum, return_linestring = FALSE) {
  attrib1 = paste0(attrib, ".1")
  suppressMessages({
    suppressWarnings({
      sl_intersection = sf::st_intersection(sl[attrib], sl2[attrib])
    })
  })
  for(i in seq_len(length(attrib))) {
    sl_intersection[[attrib[i]]] = fun(sl_intersection[[attrib[i]]], sl_intersection[[attrib1[i]]])
    sl_intersection[[attrib1[i]]] = NULL
  }
  suppressMessages({
    suppressWarnings({
      sl_intersection = sf::st_line_merge(sl_intersection)
      sl_seg1 = sf::st_difference(sl[attrib], sf::st_geometry(sl_intersection))
      sl_seg2 = sf::st_difference(sl2[attrib], sf::st_geometry(sl_intersection))
    })
  })
  rnet = rbind(sl_seg1, sl_seg2, sl_intersection)
  if(!return_linestring && any(!is_linestring(rnet))) {
    rnet = to_linestring(rnet)
  }
  return(rnet)
}
#' Identify overlapping lines
#'
#' A small wrapper around `sf::st_relate()` that does not create
#' messages when done on non projected data, and which has
#' default [DE-9IM](https://en.wikipedia.org/wiki/DE-9IM#Spatial_predicates)
#' values.
#' @inheritParams overline_sf2
#' @param pattern DE-9IM patterns to match. Default: `1F1F00102|1F10F0102|1FF0FF102|1FF00F102`
#' @examples
#' routes_fast_sf$value = 1
#' plot(routes_fast_sf[2:3, 4])
#' overlaps(routes_fast_sf[2, ], routes_fast_sf[3, ])
#' @export
overlaps = function(sl, sl2, pattern = "1F1F00102|1F10F0102|1FF0FF102|1FF00F102") {
  suppressMessages({
    relate_rnet = sf::st_relate(sl, sl2)
  })
  grepl(pattern = pattern, relate_rnet)
}
#' Identify overlapping lines
#'
#' A small wrapper around `sf::st_geometry_type()` that
#' identifies lines that are `LINESTRING`s
#' @inheritParams overline_sf2
#' @param geom_type The type of geometry to check for, `LINESTRING` by default
#' @export
is_linestring = function(sl, geom_type = "LINESTRING") {
  sf::st_geometry_type(sl) == "LINESTRING"
}
#' Identify overlapping lines
#'
#' Convert geometries that may be a mixture of `MULTILINESTRING`s
#' and `LINESTRING`s into non-overlapping `LINESTRING`s.
#'
#' @inheritParams overline_sf2
#' @export
#' @examples
#' l1 = sf::st_linestring(matrix(c(1, 2, 4, 1, 1, 2), ncol = 2))
#' l2 = sf::st_linestring(matrix(c(1, 2, 4, 1, 1, 0), ncol = 2))
#' ml = sf::st_multilinestring(list(l1, l2))
#' sl = sf::st_sf(sf::st_sfc(l1, l2, ml))
#' to_linestring(sl)
to_linestring = function(sl) {
  sel_linestring = is_linestring(sl)
  sl_linestring = sl[sel_linestring, ]
  sl_multilinestring = sl[!sel_linestring, ]
  sl_mlines = sf::st_cast(sl_multilinestring, "LINESTRING")
  rbind(sl_linestring, sl_mlines)
}
