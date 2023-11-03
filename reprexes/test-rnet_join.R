remotes::install_dev("stplanr")
library(stplanr)
library(sf)
rnet_x = sf::read_sf("https://github.com/nptscot/npt/releases/download/v2/rnet_x_thurso.geojson")
rnet_y = sf::read_sf("https://github.com/nptscot/npt/releases/download/v2/rnet_y_thurso.geojson")

name_list = names(rnet_y)
funs = list()

# Loop through each name and assign it a function based on specific conditions
for (name in name_list) {
  if (name == "geometry") {
    next  # Skip the current iteration
  } else if (name %in% c("Gradient", "Quietness")) {
    funs[[name]] = mean
  } else {
    funs[[name]] = sum
  }
}

nrow(rnet_x)
#> [1] 913
nrow(rnet_y)
#> [1] 639

runtime = system.time({
  rnet_merged = rnet_merge(rnet_x, rnet_y, dist = 20, segment_length = 10, funs = funs, max_angle_diff = 20)
})

nrow(rnet_x) / runtime[3]


rnet_xp = st_transform(rnet_x, "EPSG:27700")
rnet_yp = st_transform(rnet_y, "EPSG:27700")

funs = list(commute_fastest_bicycle = sum)

res = bench::mark(
  rsgeo = rnet_merge(rnet_x, rnet_y, use_rsgeo = TRUE, segment_length = 5, funs = funs),
  rsgeo_projected = rnet_merge(rnet_xp, rnet_yp, use_rsgeo = TRUE, segment_length = 5, funs = funs),
  sf = rnet_merge(rnet_x, rnet_y, use_rsgeo = FALSE, segment_length = 5, funs = funs),
  check = FALSE
)
res

# Test with new dataset
rnet_x = sf::read_sf("https://github.com/nptscot/npt/releases/download/test_rnet_merge/rnet_xp_test.geojson")
rnet_y = sf::read_sf("https://github.com/nptscot/npt/releases/download/test_rnet_merge/rnet_yp_test.geojson")

names(rnet_y)
funs = list(
  commute_fastest_bicycle = sum,
  commute_fastest_bicycle_go_dutch = sum
)

    name_list = names(rnet_y)

    funs = list()

    # Loop through each name and assign it a function based on specific conditions
    for (name in name_list) {
      if (name == "geometry") {
        next  # Skip the current iteration
      } else if (name %in% c("Gradient", "Quietness")) {
        funs[[name]] = mean
      } else {
        funs[[name]] = sum
      }
    }

rnet_merge(rnet_x, rnet_y, funs = funs, dist = 20)
