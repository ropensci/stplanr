remotes::install_dev("stplanr")
library(stplanr)

rnet_x = rnet_subset(osm_net_example[1], route_network_small)
# The source object:
rnet_y = route_network_small["flow"]
rnet_y$quietness = rnorm(nrow(rnet_y))
funs = list(flow = sum, quietness = mean)
rnet_merged = rnet_merge(rnet_x[1], rnet_y[c("flow", "quietness")],
                         dist = 9, segment_length = 20, funs = funs)
plot(rnet_y["flow"])
plot(rnet_merged["flow"])