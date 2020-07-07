download.file("https://github.com/ropensci/stplanr/releases/download/0.6.1/Example.zip", "Example.zip")
unzip("Example.zip")


library(geosphere)
library(sf)
library(stplanr)

roads = st_read("Example/Roads/Roads.shp")
points = st_read("Example/Points/Points.shp")

# Convert roads to coordinate system of points
roads_trf = st_transform(roads, st_crs(points))
# Convert to points to SpatialPointsDataframe
points_sp =  as(points, "Spatial")

from = c(-49.95058, -24.77502)
to = c(-49.91084, -24.75200)
p = SpatialLinesNetwork(roads_trf, uselonglat = FALSE, tolerance = 0)
r = route_local(p, from, to)
plot(p)
plot(r$geometry, add = TRUE, col = "red", lwd = 5)
plot(points_sp[c(3, 4), ], add = TRUE)
r2 = route_local(sln = p, points[3, ], points[4, ])
plot(r2$geometry, add = TRUE, col = "blue", lwd = 3)

# with complete roads...
roads = st_read("Example/Complete Roads/complete_roads.shp")
points = st_read("Example/Points/Points.shp")
roads = sf::st_cast(roads, "LINESTRING")


# Convert roads to coordinate system of points
roads_trf = st_transform(roads, st_crs(points))
# Convert to points to SpatialPointsDataframe
points_sp =  as(points, "Spatial")

from = c(-49.95058, -24.77502)
to = c(-49.91084, -24.75200)
system.time({

})
r = route_local(p, from, to)
plot(p)
plot(r$geometry, add = TRUE, col = "red", lwd = 5)
plot(points_sp[c(3, 4), ], add = TRUE)
r2 = route_local(sln = p, points[3, ], points[4, ])
plot(r2$geometry, add = TRUE, col = "blue", lwd = 3)

# with sfnetworks

download.file("https://github.com/ropensci/stplanr/releases/download/0.6.1/Example.zip", "Example.zip")
unzip("Example.zip")

library(sf)
library(sfnetworks)
library(tidygraph)

roads = st_read("Example/Roads/Roads.shp")
points = st_read("Example/Points/Points.shp")
summary(sf::st_geometry_type(roads))


# Convert roads to coordinate system of points
roads_trf = st_transform(roads, st_crs(points))
# Convert to points to SpatialPointsDataframe
points_sp = as(points, "Spatial")

from = c(-49.95058, -24.77502)
to = c(-49.91084, -24.75200)
net = as_sfnetwork(roads_trf, directed = FALSE) %>%
  activate("edges") %>%
  mutate(weight = edge_length())

p1 = sf::st_as_sf(data.frame(x = from[1], y = from[2]), coords = c("x", "y"), crs = sf::st_crs(net))
p2 = sf::st_as_sf(data.frame(x = to[1], y = to[2]), coords = c("x", "y"), crs = sf::st_crs(net))

r = net %>%
  convert(to_spatial_shortest_paths, p1, p2)

r2 = net %>%
  convert(to_spatial_shortest_paths, points[4, ], points[4, ])
plot(net)
plot(r, col = "red", lwd = 5, add = TRUE)
plot(points_sp[c(3, 4), ], add = TRUE)
plot(r2, add = TRUE, col = "blue", lwd = 3)

# with complete roads...
roads = st_read("Example/Complete Roads/complete_roads.shp")
points = st_read("Example/Points/Points.shp")

# Convert roads to coordinate system of points
net = as_sfnetwork(roads_trf, directed = FALSE) %>%
  activate("edges") %>%
  mutate(weight = edge_length())

summary(sf::st_geometry_type(roads))
roads = sf::st_cast(roads, "LINESTRING")
roads_trf = st_transform(roads, st_crs(points))


system.time({
  net = as_sfnetwork(roads_trf, directed = FALSE) %>%
    activate("edges") %>%
    mutate(weight = edge_length())
})

r = net %>%
  convert(to_spatial_shortest_paths, p1, p2)

r2 = net %>%
  convert(to_spatial_shortest_paths, points[4, ], points[4, ])
plot(net)
plot(r, col = "red", lwd = 5, add = TRUE)
plot(r2, add = TRUE, col = "blue", lwd = 3)


