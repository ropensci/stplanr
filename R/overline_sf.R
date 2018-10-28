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
#' sl <- routes_fast_sf[2:5, ]
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
    relate_rnet = sf::st_relate(rnet, sl[i, ])
    sel_overlaps = grepl(pattern = "1F1F00102|1F10F0102|1FF0FF102|1FF00F102", relate_rnet)
    if(any(sel_overlaps)) {
      rnet_no_overlap = rnet[!sel_overlaps, ]
      rnet_overlap = rnet[sel_overlaps, ]
      if(sum(sel_overlaps) > 1) {
        message("Multiple intersections for route ", i)
        for(j in 1:nrow(rnet_overlap)) {
          rnet_new = overline_sf2(rnet_overlap[j, ], sl[i, ])
        }
      }
      rnet_new = overline_sf2(rnet_overlap, sl[i, ], attrib = attrib, fun = fun)
      rnet = rbind(rnet_no_overlap, rnet_new)
    } else {
      rnet = rbind(rnet, sl[i, attrib])
    }
    # plot(rnet, lwd = rnet$value, main = "yes")
    # Sys.sleep(1)
  }
  # test for overlaps
  rnet_overlaps = sf::st_relate(rnet)
  if(length(unique(as.vector(rnet_overlaps))) > 3) message("Unwanted relation")
  # rnet_overlaps = matrix(as.numeric(as.factor(rnet_overlaps)), nrow = 10)
  return(rnet)
}
#' Combine 2 `sf` `LINESTRING` objects into non-overlapping segments and
#' find the aggregate value of selected columns defined by `attrib` along shared segments.
#' @inheritParams overline
#' @param sl2 A second `LINESTRING` object to aggregate
#' @export
#' @examples
#' routes_fast_sf$value = 1
#' sl = routes_fast_sf[2, ]
#' sl2 = routes_fast_sf[3, ]
#' rnet = overline_sf2(sl, sl2, attrib = "value")
#' plot(rnet, lwd = rnet$value)
#' rnet2 = overline_sf2(sl, sl2, attrib = c("value", "length"))
#' plot(rnet2)
#' sl <- routes_fast_sf[2:5, ]
#' rnet3 = overline_sf(sl = sl, attrib = c("value", "length"))
#' rnet4 = overline_sf2(rnet3[1, ], routes_fast_sf[6, ], "value")
#' plot(rnet3$geometry, lwd = rnet3$value)
#' plot(rnet4, add = T, col = "red")
overline_sf2 = function(sl, sl2, attrib = "value", fun = sum) {
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
      sl_seg1 = sf::st_difference(sl[attrib], sf::st_geometry(sl_intersection))
      sl_seg2 = sf::st_difference(sl2[attrib], sf::st_geometry(sl_intersection))
    })
  })
  rnet = rbind(sl_intersection, sl_seg1, sl_seg2)
  return(rnet)
}
#' Identify overlapping lines
#'
#' @inheritParams overline_sf2
#' @export
overlaps = function(sl, sl2, pattern = "1F1F00102") {
  suppressMessages({
    sf::st_relate(sl, sl2, pattern = pattern)
  })
}
