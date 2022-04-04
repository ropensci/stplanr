#' Divide sf LINESTRING objects into regular segments
#' @inheritParams line2df
#' @param n_segments The number of segments to divide the line into
#' @param segment_length The approximate length of segments in the output (overides n_segments if set)
#' @family lines
#' @export
#' @examples
#' l <- routes_fast_sf[2, ]
#' l_seg2 <- line_segment_sf(l = l, n_segments = 2)
#' plot(sf::st_geometry(l_seg2), col = 1:2, lwd = 5)
line_segment_sf <- function(l, n_segments, segment_length = NA) {
  if (!is.na(segment_length)) {
    l_length <- line_length(l)
    n_segments <- round(l_length / segment_length)
  }
  # browser() # tests
  # first_linestring = lwgeom::st_linesubstring(x = l, from = 0, to = 0.2)
  from_to_sequence = seq(from = 0, to = 1, length.out = n_segments + 1)
  line_segment_list = lapply(seq(n_segments), function(i)
    lwgeom::st_linesubstring(
      x = l,
      from = from_to_sequence[i],
      to = from_to_sequence[i + 1])
  )
  do.call(rbind, line_segment_list)
}