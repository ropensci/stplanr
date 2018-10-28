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
#' sl <- routes_fast_sf[2:6, ]
#' rnet = overline_sf(sl = sl, attrib = c("value", "length"))
#' plot(rnet, lwd = rnet$value)
#' # A larger example
#' sl <- routes_fast_sf[2:7, ]
#' rnet = overline_sf(sl = sl, attrib = c("value", "length"))
#' plot(rnet, lwd = rnet$value)
#' # rnet_sf <- overline(routes_fast_sf, attrib = "length", buff_dist = 10)
#' # plot(rnet_sf$geometry, lwd = rnet_sf$length / mean(rnet_sf$length))
#' }
overline_sf <- function(sl, attrib, fun = sum) {
  # initiate line to join-on
  rnet = sl[1, attrib]
  for(i in 2:nrow(sl)) {
    # for(i in 2:5) {
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
        message("Multiple intersections for route ", i)
        rnet_new = overline_sf2(rnet_overlap[1,], sl[i,], attrib = attrib, fun = fun)
        # sel_overlaps2 = overlaps(rnet_new, sl[i, ])
        # rnet_new = rnet_new_over[sel_overlaps2, ]
        rnet_new_ls = rnet_new[is_linestring(rnet_new), ] # save 'easy' part
        sl$geometry[i] = rnet_new$geometry[!is_linestring(rnet_new)]
        for(j in 2:nrow(rnet_overlap)) {
          rnet_new_over = overline_sf2(rnet_overlap[j, ], sl[i, ], attrib = attrib, fun = fun)
          sel_overlaps2 = overlaps(rnet_new_over, sl[i, ])
          rnet_new2 = rnet_new_over[sel_overlaps2, ]
          rnet_new = rbind(rnet_new, rnet_new2)
          sl$geometry[i] = sf::st_difference(sl$geometry[i], rnet_new$geometry)
        }
      }
      rnet = rbind(rnet_no_overlap, rnet_new)
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
#' sf::st_geometry_type(rnet)
#' plot(rnet, lwd = rnet$value)
#' rnet2 = overline_sf2(sl, sl2, attrib = c("value", "length"))
#' plot(rnet2)
#' sl <- routes_fast_sf[2:6, ]
#' rnet3 = overline_sf(sl = sl, attrib = c("value", "length"))
#' sel_over = overlaps(rnet3, routes_fast_sf[7, ])
#' sl1 = rnet3[sel_over, ][1, ]
#' rnet4 = overline_sf2(sl1, routes_fast_sf[7, ], "value")
#' plot(rnet3$geometry, lwd = rnet3$value)
#' plot(sl1$geometry, lwd = 9, add = TRUE)
#' plot(rnet4, add = TRUE, col = "red", lwd = rnet4$value)
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
  if(return_linestring && any(!is_linestring(rnet))) {
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
