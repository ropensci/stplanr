library(stplanr)
library(tmap)
remotes::install_github("saferactive/trafficalmr")
tmap_mode("view")

rnet = overline(routes_fast_sf, "length")
rnet = readRDS("geocompr/ltn_all.Rds")

rnet_projected = sf::st_transform(rnet, 27700)
res = qgis_run_algorithm(
  algorithm = "grass7:v.split",
  input = rnet_projected,
  length = 500
)




zones = stplanr::zones_sf
plot(rnet)
summary(sf::st_length(rnet))
qtm(zones) +
  qtm(rnet)

library(sf)
rnet_broken1 = line_segment(sf::as_Spatial(rnet), segment_length = 500)
rnet_broken1 = trafficalmr::line_segment_sf(rnet[2, ], 10)
rnet_split = rnet_boundary_rnet_breakup()

remotes::install_github("paleolimbot/qgisprocess")
library(qgisprocess)
rnet_projected = sf::st_transform(rnet, 27700)
res = qgis_run_algorithm(
  algorithm = "grass7:v.split",
  input = rnet_projected,
  length = 500
)

rnet_split_qgis = sf::read_sf(res$output)
nrow(rnet_split_qgis) / nrow(rnet)
summary(sf::st_length(rnet_split_qgis))
# in system CLI:
# docker run -d -p 8785:8787 -e DISABLE_AUTH=TRUE -v $(pwd):/home/rstudio/geocompr  geocompr/geocompr:qgis

rnet_split =
