remotes::install_dev("stplanr")
library(stplanr)
library(sf)
rnet_x = sf::read_sf("https://github.com/nptscot/npt/releases/download/v2/rnet_x_thurso.geojson")
rnet_y = sf::read_sf("https://github.com/nptscot/npt/releases/download/v2/rnet_y_thurso.geojson")

rnet_xp = st_transform(rnet_x, "EPSG:27700")
rnet_yp = st_transform(rnet_y, "EPSG:27700")

res = bench::mark(
  rsgeo = rnet_join(rnet_x, rnet_y, use_rsgeo = TRUE),
  rsgeo_projected = rnet_join(rnet_xp, rnet_yp, use_rsgeo = TRUE),
  sf = rnet_join(rnet_x, rnet_y, use_rsgeo = FALSE),
  check = FALSE
)
res
