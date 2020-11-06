u = "https://github.com/ropensci/stplanr/releases/download/0.6.0/r_key_roads_test.Rds"
rnet_disconnected = readRDS(url(u))
sf:::plot.sfc_LINESTRING(rnet_disconnected$geometry)
touching_list = sf::st_intersects(rnet_disconnected)
g = igraph::graph.adjlist(touching_list)
igraph::is.directed(g)
g = igraph::as.undirected(g)
# g
# wc = igraph::cluster_walktrap(g)
# wc = igraph::cluster_louvain(g)
# wc = igraph::cluster_fast_greedy(g)
wc = igraph::clusters(g)
m = igraph::membership(wc)
length(unique(m))
summary(m)
nrow(rnet_disconnected)
length(m)
rnet_disconnected$membership = m
rnet_disconnected$membership = as.character(rnet_disconnected$membership)
mapview::mapview(rnet_disconnected["membership"], lwd = 3)


mapview::mapview(rnet_disconnected)
# rnet_membership = stplanr::sln_clean_graph()