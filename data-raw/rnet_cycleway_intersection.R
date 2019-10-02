## code to prepare `rnet_cycleway_intersection` dataset goes here
remotes::install_github("ITSLeeds/geofabric")
library(geofabric)
# download pbf data of west yorkshire
roads_westyorkshire <- geofabric::get_geofabric("west yorkshire")

# subset them
place_name <- "chapeltown leeds"
place_point <- tmaptools::geocode_OSM(place_name)
place_df <- data.frame(name = place_name, lon = place_point$coords[1], lat = place_point$coords[2])
place_sf <- sf::st_as_sf(place_df, coords = c("lon", "lat"), crs = 4326)
place_buffer <- stplanr::geo_projected(place_sf, sf::st_buffer, dist = 5000)

roads_chapeltown_leeds = roads_westyorkshire[place_buffer, ]
key_roads_text = "primary|secondary|tertiary|cycleway|trunk|motorway"
roads_chapeltown_leeds = roads_chapeltown_leeds[grepl(pattern = key_roads_text, x = roads_chapeltown_leeds$highway), ]

cycleway_sf <- sf::st_as_sf(
  data.frame(lon = -1.545201, lat = 53.814019),
  coords = c("lon", "lat"),
  crs = 4326
)
cycleway_buffer <- stplanr::geo_projected(cycleway_sf, sf::st_buffer, dist = 15)
rnet_cycleway_intersection <- roads_chapeltown_leeds[cycleway_buffer, ]

# par(mar = rep(0, 4))
# plot(rnet_cycleway_intersection$geometry)

usethis::use_data(rnet_cycleway_intersection)
