# Aim: test the performance of parallel code
library(stplanr)
# Relies on having large lines dataset
n <- 1000 # number of lines to route
ii <- round(n / nrow(flowlines))
for (i in 1:ii) {
  if (i == 1) {
    l <- flowlines
  } else {
    l <- rbind(l, flowlines)
  }
}

system.time({
  r1 <- line2route(l)
})
# result1 - rl
# user  system elapsed
# 55.864   1.384 198.586
# result2 - rl
# user  system elapsed
# 44.336   0.392 125.790
# user  system elapsed
# 36.476   2.500 186.043
detach("package:stplanr", unload = TRUE)
devtools::install_github(repo = "ropensci/stplanr", ref = "0.1.8")
library(stplanr)
system.time({
  r2 <- line2route(l = l, n_processes = 8)
})
# result1 - rl
# user  system elapsed
# 0.620   0.148  30.679
# result2 - rl n_process = 10
# user  system elapsed
# 1.588   0.212  22.789
# rl n_processes = 30
# user  system elapsed
# 32.264   0.904  43.245
# tests
# rl n_processes = 20
# user  system elapsed
# 1.564   0.332  31.438
# rl n_processes = 4
# user  system elapsed
# 1.384   0.624  30.513
identical(r1, r2) # not identical
nrow(r1) == nrow(r2) # identical
identical(raster::geom(r1), raster::geom(r2)) # not identical geometries
plot(r1)
plot(r2) # very different appearance...
