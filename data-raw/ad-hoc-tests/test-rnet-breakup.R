library(sf)
library(stplanr)
plot(st_geometry(rnet_roundabout), lwd = 2, col = rainbow(nrow(rnet_roundabout)))
boundary_points <- st_geometry(line2points(rnet_roundabout))
points_cols <- rep(rainbow(nrow(rnet_roundabout)), each = 2)
plot(boundary_points, pch = 16, add = TRUE, col = points_cols)

# Clean the roundabout example.
rnet_roundabout_clean <- rnet_breakup_vertices(rnet_roundabout)
plot(st_geometry(rnet_roundabout_clean), lwd = 2, col = rainbow(nrow(rnet_roundabout_clean)))
rnet_roundabout$variable = 1:nrow(rnet_roundabout)
system.time(rnet_roundabout_clean <- rnet_breakup_vertices(rnet_roundabout))
system.time(rnet_overline <- overline(rnet_roundabout, attrib = "variable"))
plot(st_geometry(rnet_overline), lwd = 2, col = rainbow(nrow(rnet_overline)))

library(dplyr)
rnet_agg = rnet_roundabout_clean %>%
  group_by()

sl <- routes_fast_sf
sl$All <- flowlines$All
rnet <- overline(sl = sl, attrib = "All")
plot(rnet, lwd = 3)

sl_broken = rnet_breakup_vertices(sl)
sl_agg = sl_broken %>%
  group_by(sf::st_as_text(geometry)) %>%
  summarise(All = sum(All))
plot(sl_agg["All"], lwd = 3)
mapview::mapview(sl_agg["All"], lwd = 3)

sl_broken = rnet_breakup_vertices(sl, breakup_internal_vertex_matches = FALSE)

sl_agg = sl_broken %>%
  group_by(sf::st_as_text(geometry)) %>%
  summarise(All = sum(All))
plot(sl_agg["All"], lwd = 3)
mapview::mapview(sl_agg["All"], lwd = 3)

system.time({
  sl_broken = rnet_breakup_vertices(sl, breakup_internal_vertex_matches = FALSE)

  sl_agg = sl_broken %>%
    group_by(sf::st_as_text(geometry)) %>%
    summarise(All = sum(All))
})

system.time(rnet <- overline(sl = sl, attrib = "All"))

