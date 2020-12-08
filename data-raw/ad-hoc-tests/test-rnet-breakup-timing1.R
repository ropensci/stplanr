# Aim: try to speed-up rnet_breakup_vertices

remotes::install_github("ropensci/stplanr")

library(sf)
library(stplanr)
# rnet = stplanr::rnet_roundabout
# rnet = stplanr::osm_net_example
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

`%dtIN%` <- function(y, x) {
  tmp = data.table::rbindlist(list(x,y))
  len_ = nrow(x)
  tmp[, idx := any(.I <= len_) & .N > 1L, by=names(tmp)]
  tail(tmp$idx, nrow(y))
}

rnet_boundary_rnet_breakup2 <- function(rnet) {
  coordinates <- sfheaders::sf_to_df(rnet)
  first_pair <- !duplicated(coordinates[["sfg_id"]])
  last_pair <- !duplicated(coordinates[["sfg_id"]], fromLast = TRUE)
  idxs <- first_pair | last_pair
  pairs <- unique(coordinates[idxs, c("x", "y")])
  i <- coordinates[!idxs, c("x", "y")]
  # find 'internal intersections'
  i_dup <- duplicated(i)
  i_u <- unique(i)
  # this stage can be made a bit faster with data.table:
  # https://stackoverflow.com/questions/23971160
  # i_in_pairs = interaction(i_u) %in% interaction(pairs) # very slow!
  if(requireNamespace("data.table", quietly = TRUE))
  i_in_pairs <- i_u %dtIN% pairs
  p_sf <- rbind(i_u[i_in_pairs, ], i[i_dup, ])
  p <- sfheaders::sf_point(unique(p_sf))
  sf::st_crs(p) <- sf::st_crs(rnet)
  p
}

p1 = rnet_boundary_rnet_breakup(rnet)
p2 = rnet_boundary_rnet_breakup2(rnet)

nrow(p1)
nrow(p2)
plot(p1)
plot(p2, cex = 2, add = TRUE)

bpline2points <- line2points(rnet)
boundary_points <- rnet_boundary_points(rnet)
boundary_points2 <- rnet_duplicated_vertices(rnet)
boundary_points_rnb <- rnet_boundary_rnet_breakup(rnet)
boundary_points_rnb2 <- rnet_boundary_rnet_breakup2(rnet)
plot(rnet$geometry)
plot(boundary_points, add = TRUE, cex = 0.5) # too many
plot(boundary_points2, add = TRUE)
plot(boundary_points_rnb, col = "red", add = TRUE, cex = 3)

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
  rnb2 = {rnet_boundary_rnet_breakup2(rnet)},
  rnbv = {rnet_breakup_vertices(rnet)}
)

library(profvis)
profvis({
  coordinates <- sfheaders::sf_to_df(trafficalmr::tc_data_osm)
  first_pair <- !duplicated(coordinates[["sfg_id"]])
  last_pair <- !duplicated(coordinates[["sfg_id"]], fromLast = TRUE)
  idxs <- first_pair | last_pair
  pairs <- unique(coordinates[idxs, c("x", "y")])
  i <- coordinates[!idxs, c("x", "y")]
  # find 'internal intersections'
  i_dup <- duplicated(i)
  i_u <- unique(i)
  # this stage can be made a bit faster with data.table:
  # https://stackoverflow.com/questions/23971160
  # i_in_pairs = interaction(i_u) %in% interaction(pairs) # very slow!
  tmp <- data.table::rbindlist(list(i_u, pairs))
  len_ <- nrow(i_u)
  tmp[, idx := any(.I <= len_) & .N > 1L, by=names(tmp)]
  i_in_pairs <- tail(tmp$idx, nrow(pairs))
  p_sf <- rbind(i_u[i_in_pairs, ], i[i_dup, ])
  p <- sfheaders::sf_point(unique(p_sf))
  sf::st_crs(p) <- sf::st_crs(rnet)
  p
})


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

library(stplanr)
library(sf)
rnet = stplanr::osm_net_example

# define:
# rnet_boundary_rnet_breakup
# `%dtIN%`
# rnet_boundary_rnet_breakup2


boundary_points_by_id_r <- function (rnet) {
  ids <- lapply (rnet$geometry, function (i) rownames (as.matrix (i)))
  ends <- vapply (ids, function (i) i [c (1, length (i))],
                  character (2), USE.NAMES = FALSE)
  ends <- unique (as.vector (ends))

  mids <- lapply (ids, function (i) i [-c (1, length (i))])
  mids <- unname (do.call (c, mids))
  mid_is_end <- mids [mids %in% ends]
  mids <- table (mids)
  mids_dup <- names (mids) [which (mids > 1)]

  ids <- unique (c (mid_is_end, mids_dup))

  xy <- do.call (rbind, lapply (rnet$geometry, function (i) as.matrix (i)))
  xy <- xy [match (ids, rownames (xy)), ]
  p <- sfheaders::sf_point (xy)
  sf::st_crs(p) <- sf::st_crs(rnet)
  return (p)
}

library(Rcpp)
cppFunction('
Rcpp::CharacterVector getids (Rcpp::List g) {
    const int n = g.size ();

    std::unordered_set <std::string> endset, midset, middupset;
    Rcpp::List mids (n);
    for (Rcpp::NumericMatrix i: g) {
        Rcpp::List dimnames = i.attr ("dimnames");
        Rcpp::CharacterVector rownames = dimnames (0);

        endset.emplace (rownames (0));
        endset.emplace (rownames (rownames.size () - 1));

        rownames.erase (0);
        rownames.erase (rownames.size () - 1);
        for (auto j: rownames)
        {
            std::string js = Rcpp::as <std::string> (j);
            if (midset.find (js) != midset.end ())
                middupset.emplace (j);
            midset.emplace (js);
        }
    }

    std::unordered_set <std::string> mid_is_end;
    for (auto i: midset) {
        if (endset.find (i) != endset.end ())
            mid_is_end.emplace (i);
    }

    std::unordered_set <std::string> res;
    for (auto i: mid_is_end)
        res.emplace (i);
    for (auto i: middupset)
        res.emplace (i);

    return Rcpp::wrap (res);
    }
')

boundary_points_by_id_cpp <- function (rnet) {
  ids <- getids (rnet$geometry)
  xy <- do.call (rbind, lapply (rnet$geometry, function (i) as.matrix (i)))
  xy <- xy [match (ids, rownames (xy)), ]
  p <- sfheaders::sf_point (xy)
  sf::st_crs(p) <- sf::st_crs(rnet)
  return (p)
}

pref <- rnet_boundary_rnet_breakup2 (rnet)
pr <- boundary_points_by_id_r (rnet)
pc <- boundary_points_by_id_cpp (rnet)
nrow (pref); nrow (pr); nrow (pc)
#> [1] 55
#> [1] 55
#> [1] 55

bench::mark(check = FALSE,
            l2p = {line2points(rnet)},
            rbp = {rnet_boundary_points(rnet)},
            rdv = {rnet_duplicated_vertices(rnet)},
            rnb = {rnet_boundary_rnet_breakup(rnet)},
            rnb2 = {rnet_boundary_rnet_breakup2(rnet)},
            rnbv = {rnet_breakup_vertices(rnet)},
            rrid = {boundary_points_by_id_r(rnet)},
            rcid = {boundary_points_by_id_cpp(rnet)},
            time_unit = "ms")