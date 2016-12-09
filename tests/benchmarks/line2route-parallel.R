# Aim: test the performance of parallel code

# Relies on having large lines dataset
n = 1000 # number of lines to route
ii = round(n / nrow(flowlines))
for(i in 1:ii) {
  if(i == 1)
    l = flowlines else
      l = tmap::sbind(l, flowlines)
}

devtools::install_github(repo = "ropensci/stplanr", ref = "9837766")
system.time({r1 = line2route(l)})
# result1 - rl
# user  system elapsed
# 55.864   1.384 198.586
# result2 - ...
detach("package:stplanr", unload=TRUE)
devtools::install_github(repo = "nikolai-b/stplanr", ref = "2ecf449")
library(stplanr)
system.time({r2 = line2route(l = l, n_processes = 4)})
# result1 - rl
# user  system elapsed
# 0.620   0.148  30.679

# tests
identical(r1, r2) # not identical
nrow(r1) == nrow(r2) # identical
identical(raster::geom(r1), raster::geom(r2)) # not identical geometries
plot(r1)
plot(r2) # very different appearance...
# try nikolai's non-parallel version:
system.time({r3 = line2route(l = l)})

