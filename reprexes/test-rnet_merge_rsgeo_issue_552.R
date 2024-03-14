# From Debug mode in line_segment_rsgeo:

# sf::write_sf(l, "test_line.geojson")
# jsonlite::write_json(n_segments, "n_segments.json")
# system("gh release upload v1.1.2 test_line.geojson")
# system("gh release upload v1.1.2 n_segments.json")

# l <- sf::read_sf("test_line.geojson")
# n_segments <- jsonlite::read_json("n_segments.json", simplifyVector = TRUE)


l <- sf::read_sf("https://github.com/ropensci/stplanr/releases/download/v1.1.2/test_line.geojson")
n_segments <- jsonlite::read_json("https://github.com/ropensci/stplanr/releases/download/v1.1.2/n_segments.json", simplifyVector = TRUE)


# extract geometry and convert to rsgeo
geo <- rsgeo::as_rsgeo(sf::st_geometry(l))

# segmentize the line strings
res_rsgeo <- rsgeo::line_segmentize(geo, n_segments)

# make them into sfc_LINESTRING
res <- sf::st_cast(sf::st_as_sfc(res_rsgeo), "LINESTRING")
length(res)
sum(n_segments)
