# Test locally:
# setwd("~/github/ropensci/stplanr")
# devtools::load_all()

# Test on lastest version
remotes::install_dev("stplanr")
library(stplanr)

rnet_y = sf::read_sf("https://github.com/ropensci/stplanr/releases/download/v1.0.2/rnet_y_ed.geojson")
rnet_y_projected = sf::st_transform(rnet_y, "EPSG:27700")
summary(sf::st_length(rnet_y_projected))
rnet_y_projected_seg = line_segment(rnet_y_projected, segment_length = 10)
rnet_y_projected_seg = line_segment(rnet_y_projected, segment_length = 20, debug_mode = TRUE)
