#' Assign segments in a route network to groups
#'
#' This function assigns linestring features, many of which in an
#' `sf` object can form route networks, into groups.
#' By default, the function `igraph::clusters()` is used to determine
#' group membership, but any `igraph::cluster*()` function can be used.
#' See examples and the web page
#' [igraph.org/r/doc/communities.html](https://igraph.org/r/doc/communities.html)
#' for more information. From that web page, the following clustering
#' functions are available:
#'
#' `cluster_edge_betweenness, cluster_fast_greedy, cluster_label_prop,`
#' `cluster_leading_eigen, cluster_louvain, cluster_optimal, cluster_spinglass, cluster_walktrap`
#'
#' @param rnet An sf, sfc, or sfNetwork object representing a route network.
#' @param ... Arguments passed to other methods.
#'
#' @return If the input rnet is an sf/sfc object, it returns an integer vector
#'   reporting the groups of each network element. If the input is an sfNetwork
#'   object, it returns an sfNetwork object with an extra column called
#'   rnet_group representing the groups of each network element. In the latter
#'   case, the connectivity of the spatial object is derived from the sfNetwork
#'   object.
#'
#' @family rnet
#' @examples
#' rnet <- rnet_breakup_vertices(stplanr::osm_net_example)
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
#'
#' # show sfNetwork implementation
#' sfn <- SpatialLinesNetwork(rnet)
#' sfn <- rnet_group(sfn)
#' plot(sfn@sl["rnet_group"])
#' @export
#'
rnet_group <- function(rnet, ...) {
  UseMethod("rnet_group")
}

#' @name rnet_group
#' @export
rnet_group.default = function(rnet, ...) {
  stop(
    "At the moment there is no support for matching objects of class ",
    paste0(class(rnet), collapse = " "), ".",
    call. = FALSE
  )
}

#' @param cluster_fun The clustering function to use. Various clustering functions
#' are available in the `igraph` package. Default: [igraph::clusters()].
#' @param d Optional distance variable used to classify segments that are
#' close (within a certain distance specified by `d`) to each other but not
#' necessarily touching
#' @param as.undirected Coerce the graph created internally into an undirected
#' graph with [igraph::as.undirected()]? TRUE by default, which enables use
#' of a wider range of clutering functions.
#' @name rnet_group
#' @export
rnet_group.sfc <- function(
  rnet,
  cluster_fun = igraph::clusters,
  d = NULL,
  as.undirected = TRUE,
  ...
) {
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
  m <- as.integer(m)
  m
}

#' @name rnet_group
#' @export
rnet_group.sf <- function(
  rnet,
  cluster_fun = igraph::clusters,
  d = NULL,
  as.undirected = TRUE,
  ...
) {
  rnet_group(
    sf::st_geometry(rnet),
    cluster_fun = cluster_fun,
    d = d,
    as.undirected = as.undirected,
    ...
  )
}

#' @name rnet_group
#' @export
rnet_group.sfNetwork <- function(
  rnet,
  cluster_fun = igraph::clusters,
  ...
) {
  # 1. Derive the dual graph of the input rnet object
  rnet_graph_dual <- igraph::make_line_graph(methods::slot(rnet, "g"))

  # 2. Apply the cluster_fun
  wc <- cluster_fun(rnet_graph_dual)

  # 3. Derive the membership
  m <- igraph::membership(wc)

  # 4. Add the new column
  if ("rnet_group" %in% colnames(methods::slot(rnet, "sl"))) {
    warning(
      "The rnet_group column will be overwritten.",
      call. = FALSE,
      immediate. = TRUE
    )
  }
  methods::slot(rnet, "sl")[["rnet_group"]] <- as.integer(m)

  # Return
  rnet

}