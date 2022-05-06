# this is the test code that led to the development of this function:
# u = paste0("https://map.bikecitizens.net/api/v1/locations/route.json?",
#   "cccode=gb-leeds&routing_profile=balanced&bike_profile=citybike&",
#   "from_lat=53.8265&from_lon=-1.576195&to_lat=53.80025&to_lon=-1.51577")
# r = jsonlite::read_json(u)
# d = do.call(rbind, r$route)
# class(d) # matrix
# storage.mode(d) = "numeric"
# dsf = sf::st_sfc(sf::st_linestring(d[, c(2, 1, 3)]), crs = 4326)
# dsf
# mapview::mapview(dsf)

# Examples showing how it works:
#' @export
#' @examples
#' \donttest{
#' if(curl::has_internet()) {
#' route_bikecitizens()
#' ldf <- od_coords(stplanr::od_data_lines[2, ])
#' r <- route_bikecitizens(ldf)
#' plot(r)
#' }
#' }
