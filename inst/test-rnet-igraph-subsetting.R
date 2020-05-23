# Aim: test functions for subsetting and doing other things with route networks based on igraph functions

library(stplanr)

# piggyback::pb_download_url("r_key_roads_test.Rds")
u = "https://github.com/ropensci/stplanr/releases/download/0.6.0/r_key_roads_test.Rds"
rnet_disconnected = readRDS(url(u))
# test speed of sf solution compared with stplanr graph creation:
system.time({
  touching_list = sf::st_intersects(rnet_disconnected)
  g = igraph::graph.adjlist(touching_list)
})
# user  system elapsed
# 0.078   0.000   0.078
system.time({
  sln = SpatialLinesNetwork(rnet_disconnected)
})
# user  system elapsed
# 0.305   0.000   0.308
g
sln@g
summary(sln@g)
summary(g)
igraph::is_connected(g)
sf:::plot.sfc_LINESTRING(rnet_disconnected$geometry)


