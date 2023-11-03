# Aim: test read/write speed with geopandas

library(reticulate)

# Import the geopandas package:
geopandas = import("geopandas")

# Save a geojson and then a gpkg file:
test_object = stplanr::routes_fast_sf

# Save as geojson:
sf::write_sf(test_object, "test.geojson", delete_dsn = TRUE)
sf::write_sf(test_object, "test.gpkg", delete_dsn = TRUE)

# Test reading with geopandas:
runtime = system.time({
  output = geopandas$read_file("test.geojson")
})

benchmark = bench::mark(
  check = FALSE,
  geopandas_geojson = geopandas$read_file("test.geojson"),
  geopandas_gpkg = geopandas$read_file("test.gpkg"),
  sf_geojson = sf::read_sf("test.geojson"),
  sf_gpkg = sf::read_sf("test.gpkg")
  )

ggplot2::autoplot(benchmark)
