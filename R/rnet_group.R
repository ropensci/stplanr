#' Assign segments in a route network to groups
#'
#' @param cluster_fun The clustering function to use. Various clustering functions
#' are available in the `igraph` package. Default: [igraph::clusters()].
#' @param d Optional distance variable used to classify segments that are
#' close (within a certain distance specified by `d`) to each other but not
#' necessarily touching
#' @param as.undirected Coerce the graph created internally into an undirected
#' graph with [igraph::as.undirected()]? TRUE by default, which enables use
#' of a wider range of clutering functions.
#' @inheritParams rnet_breakup_vertices
#'
#' @return The function returns an integer vector reporting the group of each network element
#'
#' @family rnet
#' @examples
#' rnet <- stplanr::osm_net_example
#' rnet$group <- rnet_group(rnet)
#' plot(rnet["group"])
#' # mapview::mapview(rnet["group"])
#' rnet$group_25m <- rnet_group(rnet, d = 25)
#' plot(rnet["group_25m"])
#' rnet$group_walktrap <- rnet_group(rnet, igraph::cluster_walktrap)
#' plot(rnet["group_walktrap"])
#' rnet$group_louvain <- rnet_group(rnet, igraph::cluster_louvain)
#' plot(rnet["group_louvain"])
#' rnet$group_fast_greedy <- rnet_group(rnet, igraph::cluster_fast_greedy)
#' plot(rnet["group_fast_greedy"])
#' @export
rnet_group <- function(rnet, cluster_fun = igraph::clusters, d = NULL, as.undirected = TRUE) {
  if (!is.null(d)) {
    touching_list <- sf::st_is_within_distance(rnet, dist = d)
  } else {
    touching_list <- sf::st_intersects(rnet)
  }
  g <- igraph::graph.adjlist(touching_list)
  if (as.undirected) {
    g <- igraph::as.undirected(g)
  }
  wc <- cluster_fun(g)
  m <- igraph::membership(wc)
  class(m)
  m <- as.integer(m)
  m
}
