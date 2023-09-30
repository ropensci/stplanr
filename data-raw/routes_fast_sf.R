## code to prepare `routes_fast_sf` dataset goes here
library(stplanr)
routes <- routes_fast_sf
routes <- routes[as.numeric(sf::st_length(routes)) > 0, ]
waldo::compare(routes_fast_sf, routes)
routes_fast_sf = routes
usethis::use_data(routes_fast_sf, overwrite = TRUE)

# And for `routes_slow_sf`:
routes <- routes_slow_sf
routes <- routes[as.numeric(sf::st_length(routes)) > 0, ]
waldo::compare(routes_slow_sf, routes)
routes_slow_sf = routes
usethis::use_data(routes_slow_sf, overwrite = TRUE)

# And for `flowlines_sf`:
routes <- flowlines_sf
routes <- routes[as.numeric(sf::st_length(routes)) > 0, ]
waldo::compare(flowlines_sf, routes)
flowlines_sf = routes
usethis::use_data(flowlines_sf, overwrite = TRUE)

# New function to convert routes to origins and destinations
routes_to_od <- function(routes, remove_o_equals_d = TRUE) {
  route_coordinates <- sf::st_coordinates(routes)
  route_ids = route_coordinates[, 3]
  # The ids of the first coordinate of each route
  first_coordinate <- which(!duplicated(route_ids))
  # The ids of the last coordinate of each route
  last_coordinate <- which(!duplicated(route_ids, fromLast = TRUE))
  origins <- route_coordinates[first_coordinate, 1:2]
  destinations <- route_coordinates[last_coordinate, 1:2]
  if (remove_o_equals_d) {
    o_equals_d <- which(origins[, 1] == destinations[, 1] & origins[, 2] == destinations[, 2])
    if (length(o_equals_d) > 0) {
      warning(paste0(
        "There are ", length(o_equals_d), " routes where the origin and destination are the same\n",
        "These routes will be removed: ", paste0(o_equals_d, collapse = ", ")
        ))
      origins <- origins[-o_equals_d, ]
      destinations <- destinations[-o_equals_d, ]
    }
  }
  od_coordinates <- cbind(origins, destinations)
}
routes_fast_sf_coordinates = routes_to_od(routes_fast_sf)
routes_fast_desire_lines = od::odc_to_sf(routes_fast_sf_coordinates)
routes_fast_sf_new = route(l = routes_fast_desire_lines, route_fun = cyclestreets::journey, plan = "fastest")
