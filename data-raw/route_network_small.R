## code to prepare `route_network_small` dataset goes here

# Create route network focussed on OSM data
library(tidyverse)
osm_vertices = sf::st_cast(osm_net_example, to = "POINT")
plot(osm_vertices$geometry)
set.seed(2022)
osm_vertices_random = osm_vertices %>%
  sample_n(size = 9)

desire_lines = od::points_to_odl(p = osm_vertices_random)
plot(desire_lines)
desire_lines$length = sf::st_length(desire_lines) %>% as.numeric()
summary(desire_lines$length)
desire_lines_long = desire_lines %>%
  filter(length > 100)

routes = route(l = desire_lines_long, route_fun = stplanr::route_osrm)
routes$flow = seq(nrow(routes))
route_network_small = overline(routes, attrib = "flow")
plot(osm_net_example$geometry, lwd = 5, col = "grey")
route_network_small %>%
  select(flow) %>%
  plot(add = TRUE)
usethis::use_data(route_network_small, overwrite = TRUE)
