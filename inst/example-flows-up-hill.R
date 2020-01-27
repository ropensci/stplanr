# Testing routing services

library(stplanr)
r1 <- route_graphhopper("Yeadon, UK", to = "Leeds", silent = FALSE)
r2 <- route_graphhopper("Leeds", "Yeadon, UK")

# plot the data
if(require(leaflet)) {
  leaflet() %>% addTiles() %>% addPolylines(data = r1)
}

# look at the data
r1@data
r2@data

# Now with CycleStreets.net
r1 <- route_cyclestreets("Yeadon", to = "Leeds")
r2 <- route_cyclestreets("Leeds", "Yeadon")

# plot the data
if(require(leaflet)) {
  leaflet() %>% addTiles() %>% addPolylines(data = r1)
}

r1@data
r2@data
