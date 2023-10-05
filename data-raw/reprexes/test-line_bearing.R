remotes::install_dev("stplanr")
library(stplanr)
l = routes_fast_sf[1:3, ]
line_bearing(l)
line_bearing(sf::st_transform(l, "EPSG:27700"))
