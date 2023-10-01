# Test locally:
# setwd("~/github/ropensci/stplanr")
# devtools::load_all()

# Test on lastest version
remotes::install_dev("stplanr")
library(stplanr)

rnet_y = sf::read_sf("https://github.com/ropensci/stplanr/releases/download/v1.0.2/rnet_y_ed.geojson")
rnet_y_projected = sf::st_transform(rnet_y, "EPSG:27700")
summary(sf::st_length(rnet_y_projected))
# rnet_y_projected_seg = line_segment(rnet_y_projected, segment_length = 10, debug_mode = TRUE)

rnet_y_projected_seg = line_segment(rnet_y_projected, segment_length = 20, debug_mode = TRUE)
# 108 is to blame
failing_line = rnet_y_projected[108,]
plot(failing_line)
n_vertices(failing_line)
sf::st_length(failing_line)
line_segment1(failing_line, segment_length = 20, use_rsgeo = TRUE)
line_segment(failing_line, segment_length = 20)

# Try again after removing failing line:
rnet_y_projected_seg = line_segment(rnet_y_projected[-108,], segment_length = 20, debug_mode = TRUE)
# Still failed at line 108
rnet_y_projected_seg = line_segment(rnet_y_projected[1:109,], segment_length = 20, debug_mode = TRUE)
rnet_y_projected_seg = line_segment(rnet_y_projected[-109,], segment_length = 20, debug_mode = TRUE)

# Was actually 109 that was failing...
failing_line = rnet_y_projected[109,]
plot(failing_line)
n_vertices(failing_line)
sf::st_length(failing_line)
line_segment1(failing_line, segment_length = 20, use_rsgeo = TRUE)
line_segment(failing_line, segment_length = 20)
line_segment1(failing_line$geometry, segment_length = 20, use_rsgeo = TRUE)
dput(failing_line$geometry)

library(stplanr)

failing_line = structure(list(structure(c(324957.69921197, 324957.873557727, 
324959.863123514, 324961.852683597, 324963.822867622, 324969.636546456, 
324976.718443977, 324996.443964294, 673670.123131518, 673680.139281405, 
673686.784106964, 673693.428933452, 673698.960855279, 673709.992098018, 
673722.114520549, 673742.922904206), dim = c(8L, 2L), class = c("XY", 
"LINESTRING", "sfg"))), class = c("sfc_LINESTRING", "sfc"), precision = 0, bbox = structure(c(xmin = 324957.69921197, 
ymin = 673670.123131518, xmax = 324996.443964294, ymax = 673742.922904206
), class = "bbox"), n_empty = 0L)
sf::st_crs(failing_line) = "EPSG:27700"
line_segment(failing_line, segment_length = 20)

# Try with rsgeo: 
geo <- rsgeo::as_rsgeo(sf::st_geometry(failing_line))


  # segmentize the line strings
res <- rsgeo::line_segmentize(geo, n = 4)
res <- sf::st_cast(sf::st_as_sfc(res), "LINESTRING")
res
length(res) # should be 4
sf::st_length(res)
