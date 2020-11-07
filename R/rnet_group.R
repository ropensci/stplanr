#' Assign segments in a route network to groups
#'
#' @param cluster_fun The clustering function to use. Various clustering functions
#' are available in the `igraph` package. Default: [igraph::clusters()].
#' @param d Optional distance variable used to classify segments that are
#' close (within a certain distance specified by `d`) to each other but not
#' necessarily touching
#' @param char Should the function return a character representation of the groups
#' (`"1"` instead of the numeric value `1`)? `TRUE` by default
#' @inheritParams rnet_breakup_vertices
#'
#' @family
#' @examples
#' rnet <- stplanr::osm_net_example
#' rnet$group <- rnet_group(rnet)
#' plot(rnet["group"])
#' # mapview::mapview(rnet["group"])
#' rnet$group_25m = rnet_group(rnet, d = 25)
#' plot(rnet["group_25m"])
#' rnet$group_walktrap = rnet_group(rnet, igraph::cluster_walktrap)
#' plot(rnet["group_walktrap"])
#' rnet$group_louvain = rnet_group(rnet, igraph::cluster_louvain)
#' plot(rnet["group_louvain"])
#' rnet$group_fast_greedy = rnet_group(rnet, igraph::cluster_fast_greedy)
#' plot(rnet["group_fast_greedy"])
#' @export
rnet_group <- function(rnet, cluster_fun = igraph::clusters, d = NULL, char = TRUE) {
  if(!is.null(d)) {
    touching_list = sf::st_is_within_distance(rnet, dist = d)
  } else {
    touching_list = sf::st_intersects(rnet)
  }
  g = igraph::graph.adjlist(touching_list)
  # igraph::is.directed(g)
  #> [1] TRUE
  g = igraph::as.undirected(g)
  wc = cluster_fun(g)
  m = igraph::membership(wc)
  if(char) m = as.character(m)
  m
}
