remotes::install_github("ropensci/stplanr")
remotes::install_github("itsleeds/osmextract")

library(sf)
library(stplanr)
library(dplyr)

# how many groups?
n = 30

osm_data = osmextract::oe_get("west yorkshire", layer = "lines")
u = "https://github.com/ITSLeeds/leedsboundaries/raw/main/sub_area_simple.geojson"
area_of_interest = sf::st_read(u)

osm_area = osm_data[area_of_interest, , op = sf::st_within]

to_exclude = c("footway","steps","unclassified","path","construction","track","cycleway","proposed","pedestrian")
osm_filtered = osm_area %>%
  filter(! is.na(highway)) %>% 
  filter(! highway %in% to_exclude)

# osm_filtered$groups = stplanr::rnet_group(osm_filtered, igraph::cluster_fast_greedy) %>% 
osm_filtered$groups = stplanr::rnet_group(osm_filtered, igraph::cluster_walktrap) %>%
  formatC(width = 2, flag = "0")
groups_n = table(osm_filtered$groups)
groups_in_order = sort(groups_n)
top_n_groups = tail(groups_in_order, n = n)
osm_filtered_top_groups = osm_filtered %>% 
  filter(groups %in% names(top_n_groups))

plot(osm_filtered_top_groups["groups"])

library(tmap)
tmap_mode("view")
tm_shape(osm_filtered_top_groups) +
  tm_lines(col = "groups", palette = "Set1", popup.vars = "highway", lwd = 2) +
  tm_basemap(server = leaflet::providers$CartoDB.Positron)
# mapview::mapviewOptions(vector.palette = randomcoloR::randomColor(n))
# 
# mapview::mapview(osm_filtered_top_groups["groups"])
