# Aim: try to speed-up rnet_breakup_vertices

library(sf)
library(stplanr)
rnet = stplanr::rnet_roundabout
rnet = stplanr::osm_net_example
rnet = trafficalmr::tc_data_osm

rnet_boundary_rnet_breakup <- function(rnet) {
  rnet_nodes <- sf::st_geometry(line2points(rnet))
  rnet_internal_vertexes <- sf::st_geometry(line2vertices(rnet))
  unique_rnet_nodes <- do.call("c", unique(rnet_nodes))
  unique_rnet_internal_vertexes <- do.call("c", unique(rnet_internal_vertexes))
  rbind_nodes_internal_vertexes <- rbind(unique_rnet_nodes, unique_rnet_internal_vertexes)
  index_intersection_points <- duplicated(rbind_nodes_internal_vertexes)
    intersection_points <- sf::st_as_sf(
      data.frame(rbind_nodes_internal_vertexes[index_intersection_points, , drop = FALSE]),
      coords = c("x_coords", "y_coords"),
      crs = sf::st_crs(rnet)
    )
}

rnet_boundary_rnet_breakup2 <- function(rnet) {
  rnet_nodes <- sfheaders::sf_to_df(rnet)
  rnet_internal_vertexes <- sf::st_geometry(line2vertices(rnet))
  unique_rnet_nodes <- do.call("c", unique(rnet_nodes))
  unique_rnet_internal_vertexes <- do.call("c", unique(rnet_internal_vertexes))
  rbind_nodes_internal_vertexes <- rbind(unique_rnet_nodes, unique_rnet_internal_vertexes)
  index_intersection_points <- duplicated(rbind_nodes_internal_vertexes)
  intersection_points <- sf::st_as_sf(
    data.frame(rbind_nodes_internal_vertexes[index_intersection_points, , drop = FALSE]),
    coords = c("x_coords", "y_coords"),
    crs = sf::st_crs(rnet)
  )
}

bpline2points <- line2points(rnet)
boundary_points <- rnet_boundary_points(rnet)
boundary_points2 <- rnet_duplicated_vertices(rnet)
boundary_points_rnb <- rnet_boundary_rnet_breakup(rnet)
plot(rnet$geometry)
plot(boundary_points, add = TRUE, cex = 0.2) # too many
plot(boundary_points2, add = TRUE)
plot(boundary_points_rnb, col = "red", add = TRUE)

nrow(bpline2points)
nrow(boundary_points)
nrow(boundary_points2)
nrow(boundary_points_rnb)

identical(boundary_points2$geometry, boundary_points_rnb$geometry)
summary(boundary_points2$geometry %in% boundary_points_rnb$geometry)

bench::mark(check = FALSE,
  l2p = {line2points(rnet)},
  rbp = {rnet_boundary_points(rnet)},
  rdv = {rnet_duplicated_vertices(rnet)},
  rnb = {rnet_boundary_rnet_breakup(rnet)},
  rnbv = {rnet_breakup_vertices(rnet)}
)



rnet_breakup_vertices2 <- function(rnet, breakup_internal_vertex_matches = TRUE) {
  rnet_nodes <- sf::st_geometry(line2points(rnet))
  rnet_internal_vertexes <- sf::st_geometry(line2vertices(rnet))

  # For the first part of the procedure I don't need duplicated nodes or
  # duplicated vertexes so I can extract their unique values
  unique_rnet_nodes <- do.call("c", unique(rnet_nodes))
  unique_rnet_internal_vertexes <- do.call("c", unique(rnet_internal_vertexes))

  # Intersection between nodes and internal vertexes
  # The following code is the same as
  # intersection_point <- sf::st_intersection(unique_rnet_nodes, unique_rnet_internal_vertexes)
  # but faster since we are dealing only with points

  rbind_nodes_internal_vertexes <- rbind(unique_rnet_nodes, unique_rnet_internal_vertexes)
  index_intersection_points <- duplicated(rbind_nodes_internal_vertexes)

  if (any(index_intersection_points)) {
    intersection_points <- sf::st_as_sf(
      data.frame(rbind_nodes_internal_vertexes[index_intersection_points, , drop = FALSE]),
      coords = c("x_coords", "y_coords"),
      crs = sf::st_crs(rnet)
    )

    message("Splitting rnet object at the shared boundary points.")
    rnet_breakup_collection <- lwgeom::st_split(rnet, intersection_points$geometry)
    rnet_clean <- sf::st_collection_extract(rnet_breakup_collection, "LINESTRING")
  } else {
    rnet_clean <- rnet
  }

  # Split again at the duplicated internal vertexes
  rnet_internal_vertexes_duplicated <- rnet_internal_vertexes[duplicated(rnet_internal_vertexes)]

  if (length(rnet_internal_vertexes_duplicated) > 0 & breakup_internal_vertex_matches) {
    message("Splitting rnet object at the shared internal points.")
    rnet_breakup_collection <- lwgeom::st_split(rnet_clean, rnet_internal_vertexes_duplicated)
    rnet_clean <- sf::st_collection_extract(rnet_breakup_collection, "LINESTRING")
  }

  rnet_clean
}

plot(st_geometry(rnet_roundabout), lwd = 2, col = rainbow(nrow(rnet_roundabout)))
bench::mark(f1 = {rnet_roundabout_clean <- rnet_breakup_vertices(rnet_roundabout)})


boundary_points <- st_geometry(line2points(rnet_roundabout))
points_cols <- rep(rainbow(nrow(rnet_roundabout)), each = 2)
plot(boundary_points, pch = 16, add = TRUE, col = points_cols)

# Clean the roundabout example.