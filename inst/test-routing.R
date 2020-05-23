# # These lines require API keys/osrm instances so are commented out
from <- c(-1.5484, 53.7941) # from <- geo_code("leeds rail station")
to <-   c(-1.5524, 53.8038) # to <- geo_code("university of leeds")
r1 <- route(from, to, route_fun = cyclestreets::journey)
r2 <- route(from, to, route_fun = cyclestreets::journey, plan = "quietest")
plot(r1)
plot(r2)
r = route(cents_sf[1:3, ], cents_sf[2:4, ], route_fun = cyclestreets::journey) # sf points
summary(r$route_number)
dl <- od2line(od_data_sample[1:3, ], cents_sf)
route(dl, route_fun = cyclestreets::journey)
route(dl, route_fun = cyclestreets::journey, plan = "quietest")
route(dl, route_fun = cyclestreets::journey, plan = "balanced")
route(dl, route_fun = cyclestreets::journey, list_output = TRUE)
route(dl, route_fun = cyclestreets::journey, save_raw = TRUE, list_output = TRUE)
# with osrm backend - need to set-up osrm first - see routing vignette
if(require(osrm)) {
  message("You have osrm installed")
  osrm::osrmRoute(c(-1.5, 53.8), c(-1.51, 53.81))
  osrm::osrmRoute(c(-1.5, 53.8), c(-1.51, 53.81), returnclass = "sf")
  # mapview::mapview(.Last.value) # check it's on the route network
  route(l = pct::wight_lines_30[1:2, ], route_fun = osrm::osrmRoute, returnclass = "sf")
}
if(require(cyclestreets)) { # with cyclestreets backend
  l <- pct::wight_lines_30
  system.time(r <- route(l, route_fun = cyclestreets::journey))
  plot(r)
  library(parallel)
  library(cyclestreets)
  cl <- makeCluster(detectCores())
  clusterExport(cl, c("journey"))
  system.time(r2 <- route(l, route_fun = cyclestreets::journey, cl = cl))
  plot(r2)
  identical(r, r2)
  stopCluster(cl)
}