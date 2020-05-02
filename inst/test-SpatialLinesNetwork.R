library(stplanr)
library(sf)
library(igraph)

# Create the network structure
routes <- st_sf(
  ID_from = c(101, 102, 103, 104, 101),
  ID_to = c(102, 103, 104, 101, 103),
  ID_edge = 1:5,
  distance = c(2.1, 1, 1.5, 2.5, 10),
  geometry = st_sfc(
    st_linestring(rbind(c(1, 3), c(3, 3))),
    st_linestring(rbind(c(3, 3), c(3, 2))),
    st_linestring(rbind(c(3, 2), c(2, 1))),
    st_linestring(rbind(c(2, 1), c(1, 3))),
    st_linestring(rbind(c(1, 3), c(3, 2)))
  )
)

sln <- SpatialLinesNetwork(routes)
plot(sln)
V(sln@g)$name <- as.character(101:104)

# shortest path
(shortest <- sum_network_routes(sln = sln,
                                start = "101", end = "103",
                                sumvars = c("length")))
plot(st_geometry(sln@sl))
plot(st_geometry(shortest), add = TRUE, col = "red", lwd = 2)
# E(sln@g)$weight <- routes$distance
# sln@sl$length <- routes$distance
weightfield(sln) <- "distance"

(most_efficient <- sum_network_routes(sln = sln,
                                      start = 1, end = 3,
                                      sumvars = c("length")))
plot(st_geometry(shortest), add = TRUE, col = "red", lwd = 2)
plot(st_geometry(most_efficient), add = TRUE, col = "blue", lwd = 2)
