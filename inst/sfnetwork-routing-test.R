remotes::install_github("itsleeds/slopes")
remotes::install_github("itsleeds/od")
remotes::install_github("ropensci/stplanr")
library(sfnetworks)
library(sf)

r = slopes::lisbon_road_segments
r = stplanr::overline(r, "Avg_Slope")
sln = stplanr::SpatialLinesNetwork(r)
sln = stplanr::sln_clean_graph(sln)
nrow(r)
nrow(sln@sl) # simple graph
v = sf::st_coordinates(sln@sl)
nrow(v)
set.seed(8)
p = v[sample(nrow(v), size = ), ]
p = st_sample(st_convex_hull(st_union(sln@sl)), size = 3)
l = od::points_to_odl(st_coordinates(p), crs = st_crs(r), interzone_only = TRUE)
l$v = 1
l = od::od_oneway(l)
plot(sln@sl$geometry)
plot(p, add = TRUE)

net = as_sfnetwork(sln@sl)
net_t = net %>%
  activate("edges") %>%
  dplyr::mutate(length = sf::st_length(.))
igraph::shortest_paths(graph = net_t, 1, 9)$vpath

# rnet
r_test = stplanr::sum_network_routes(sln = sln, start = 1, end = 9)
plot(r_test)

# calculate shortest paths
# test with route_local
l_start_points = lwgeom::st_startpoint(l)
l_end_points = lwgeom::st_endpoint(l)
r1 = stplanr::route_local(sln = sln, from = l_start_points[1], to = l_end_points[2])
plot(r1$geometry, col = "red", lwd = 5)

# calculate shortest paths
sp = stplanr::route(
  l = l,
  route_fun = stplanr::route_local,
  sln = sln
)
plot(st_geometry(sln@sl))
plot(st_geometry(l), add = TRUE, lwd = 5)
plot(sp["route_number"], add = TRUE, lwd = rev(sp$route_number) * 3)
