# Testing routing services

library(stplanr)

r1 <- route_graphhopper("Yeadon", to = "Leeds")
r2 <- route_graphhopper("Leeds", "Yeadon")


library(leaflet)
leaflet() %>% addTiles() %>% addPolylines(data = r1)

r1@data
r2@data


r1 <- route_cyclestreet("Yeadon", to = "Leeds")
r2 <- route_cyclestreet("Leeds", "Yeadon")


library(leaflet)
leaflet() %>% addTiles() %>% addPolylines(data = r1)

r1@data
r2@data
