#' Cleanr a sf object with LINESTRING geometry such that
#'
#' @param rnet a sf LINESTRING object that ...
#'
#' @return sf object
#' @export
#'
#' @examples
#' par(mar = rep(0, 4))
#' plot(rnet_roundabout$geometry, col = c(1, rainbow(nrow(rnet_roundabout) - 1)), lwd = 2)
#'
#' rnet_roundabout_clean <- rnet_clean_vertices(rnet_roundabout)
#' plot(rnet_roundabout_clean$geometry, col = c(1, rainbow(nrow(rnet_roundabout_clean) - 1)), lwd = 2)


rnet_clean_vertices <- function(rnet) {
  rnet_nodes <- line2points(rnet)
  rnet_vertexes <- line2pointsn(rnet)

  duplicated_vertexes <- rnet_vertexes[duplicated(rnet_vertexes$geometry), ]
  duplicated_vertexes_not_nodes <- duplicated_vertexes[rnet_vertexes, ]

  rnet_clean <- lwgeom::st_split(rnet, duplicated_vertexes_not_nodes) %>%
    sf::st_collection_extract("LINESTRING")

  rnet_clean
}
