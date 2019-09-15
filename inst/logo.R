# Aim: make logo


# create points defining a hexagon ----------------------------------------

library(sf)
library(stplanr)

zones_osgb = zones_sf %>%
  st_transform(27700)

hexagonal_grid = st_make_grid(zones_osgb, cellsize = 12000, square = FALSE)
plot(hexagonal_grid)
hex = st_cast(hexagonal_grid, "POLYGON")
hex_sf = st_sf(data.frame(name = "hex"), geometry = hex)
mapview::mapview(hex)

# shrink points
z_centroids = st_centroid(hex)
hex_smaller = (hex - z_centroids) * 0.2 + z_centroids
st_crs(hex_smaller) = 27700
hex_sf = st_sf(data.frame(name = "hex"), geometry = hex_smaller)
mapview::mapview(hex_smaller)
hex_moved = mapedit::editFeatures(hex_sf)
saveRDS(hex_moved, "inst/hex.Rds")
hex_points = st_cast(hex_moved, "POINT")
hex_points = st_sf(data.frame(n = 1:nrow(hex_points)), geometry = st_geometry(hex_points))

mapview::mapview(hex_points)
l = points2line(hex_points)
l$id = paste(l$O, l$D)

outer_lines = dplyr::filter(l, id %in% paste(1:6, c(2:6, 1)))
additional_lines = dplyr::filter(l, id %in% paste(c("1 3", "1 5", "3 6", "1 4")))
all_lines = rbind(outer_lines, additional_lines)
all_lines$flow = 1
all_lines$flow[7:nrow(all_lines)] = 1:(nrow(all_lines) - 6)

# l$flow = runif(nrow(l))
# plot(l)

hex_points = hex_points[1:6, ]
hex_points_moved = mapedit::editFeatures(hex_points)

tm_shape(all_lines) +
  tm_lines("flow", palette = "-viridis", lwd = "flow", scale = 9) +
  tm_shape(hex_points) +
  tm_markers() +
  tm_scale_bar()


# colourful logo ----------------------------------------------------------



z = zones_sf
mapview::mapview(z) +
  mapview::mapview(flowlines) +
  mapview::mapview(routes_fast_sf)

mapview::mapview(route_network_sf)

library(tmap)
ttm()

tm_shape(flowlines_sf) +
  tm_lines(col = "Bicycle", lwd = "All", palette = "viridis", scale = 9)


tm_shape(flowlines_sf) +
  tm_lines(col = "Bicycle", lwd = "All", palette = "viridis", scale = 9)

library(mapedit)
points = mapedit::drawFeatures()
mapview::mapview(points)
l = points2line(points)
mapview::mapview(l)
l = l[l$D == 475, ]
plot(l)
l$v = 1
r = line2route(l)
r$v = 1
rnet = overline2(r, "v")
mapview::mapview(r)
mapview::mapview(rnet, lwd = "v")
mapview::mapview(l)

tm_shape(rnet) +
  tm_lines(lwd = "v", scale = 9)

# # animate
#
# # Aim: make logo
#
# install.packages("transformr")
# library(stplanr)
# library(tmap)
#
# plot(flowlines_sf["All"])
#
# f = flowlines_sf[flowlines_sf$Area.of.residence != flowlines_sf$Area.of.workplace,]
# tm_shape(f) +
#   tm_lines(col = "All", pal = "viridis", lwd = "All", scale = 9) +
#   tm_layout(legend.show = FALSE, frame = FALSE)
#
# tmap_save(.Last.value, "logo-test.pdf")
#
#
# # Animation
# circles <- poly_circles()
# star_hole <- poly_star_hole()
#
# morph <- tween_polygon(circles, star_hole,
#                        ease = 'linear',
#                        id = id,
#                        nframes = 12,
#                        match = FALSE)
#
# ggplot(morph) +
#   geom_polygon(aes(x = x, y = y, group = id), fill = NA, colour = 'black') +
#   facet_wrap(~.frame, labeller = label_both, ncol = 3) +
#   theme_void()
#
# library(gganimate)
# ggplot(mtcars, aes(factor(cyl), mpg)) +
#   geom_boxplot() +
#   # Here comes the gganimate code
#   transition_states(
#     gear,
#     transition_length = 2,
#     state_length = 1
#   ) +
#   enter_fade() +
#   exit_shrink() +
#   ease_aes('sine-in-out')
#
# gganimate::anim_save(animation = animation, "animation.gif")
