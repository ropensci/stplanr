# OSM tools - for downloading osm data

# data("flowlines")
# bb <- sp::bbox(flowlines)
# bbox_2_overpass <- function(bb, type = "("){
#   if(type == "["){
#     paste0("[bbox=", paste(bb, collapse = ","), "]")
#   } else{
#     paste0("(", paste(bb, collapse = ","), ")")
#   }
# }
#
# get_over(bb)
# cb <- rgeos::gCentroid(flowlines)
# cb2 <- center_bbox(coordinates(cb)[1], coordinates(cb)[2], 200, 200)
# lways <- get_osm(cb2, src)
#
# get_over <- function(bb){
#   uri = "http://overpass-api.de/api/interpreter?data="
#   if(length(dim(bb)) != 1)
#     bb <- bbox_2_overpass(bb)
#   dreq <- '[timeout:25];'
#   nreq <- '(node["highway"]'
#   wreq <- ';way["highway"]'
#   rreq <- ';relation["highway"]'
#   freq <- ';);out body;>;out skel qt;'
#   req <- paste0(dreq, nreq, bb, wreq, bb, rreq, bb, freq)
#   req
#   req <- RCurl::curlEscape(req)
#   req <- paste0(uri, req)
#   downloader::download(req, destfile = "/tmp/output.osm")
# }
#
# req <- '[out:json][timeout:25];(node["highway"](41.88539386041403,12.485854625701903,41.89461068146964,12.498128414154053);way["highway"](41.88539386041403,12.485854625701903,41.89461068146964,12.498128414154053);relation["highway"](41.88539386041403,12.485854625701903,41.89461068146964,12.498128414154053););out body;>;out skel qt;'
#
# library(downloader)
# downloader::download(get_over(bb), "here.osm")
#
#
# osm_to_geojson("/tmp/output.osm")
# library(sp)
# osm_dat <- rgdal::readOGR("/tmp/output.geojson", "OGRGeoJSON") # fail
# osm_dat <- geojsonio::geojson_read("/tmp/output.geojson") # fail
# plot(osm_dat)
# geojsonio::topojson_read()
# osm_dat[[3]][4]
#
#
# class(osm_dat)
# plot(osm_dat)
#
# osm_to_geojson <- function(file){
#   newname <- gsub(pattern = ".osm", replacement = ".geojson", file)
#   mess <- paste0("osmtogeojson ", file, " > ", newname)
#   system(mess)
# }
#
# downloader::download(url = URLencode('http://overpass-api.de/api/interpreter?data=[out:json][timeout:25];(node["highway"](41.88539386041403,12.485854625701903,41.89461068146964,12.498128414154053);way["highway"](41.88539386041403,12.485854625701903,41.89461068146964,12.498128414154053);relation["highway"](41.88539386041403,12.485854625701903,41.89461068146964,12.498128414154053););out body;>;out skel qt;)'), destfile = "/tmp/test.json")
#
# http://overpass-api.de/api/interpreter?data=%5Bout%3Ajson%5D%5Btimeout%3A25%5D%3B%28node%5B%22highway%22%5D%2841%2E88539386041403%2C12%2E485854625701903%2C41%2E89461068146964%2C12%2E498128414154053%29%3Bway%5B%22highway%22%5D%2841%2E88539386041403%2C12%2E485854625701903%2C41%2E89461068146964%2C12%2E498128414154053%29%3Brelation%5B%22highway%22%5D%2841%2E88539386041403%2C12%2E485854625701903%2C41%2E89461068146964%2C12%2E498128414154053%29%3B%29%3Bout%20body%3B%3E%3Bout%20skel%20qt%3B%0A
#
# http://overpass-api.de/api/interpreter?
# URLencode('data=[out:json][timeout:25];(node["highway"](41.88539386041403,12.485854625701903,41.89461068146964,12.498128414154053);way["highway"](41.88539386041403,12.485854625701903,41.89461068146964,12.498128414154053);relation["highway"](41.88539386041403,12.485854625701903,41.89461068146964,12.498128414154053););out body;>;out skel qt;)',  reserved = T)
#
# res <- GET('http://overpass-api.de/api/convert?data=%5Bout%3Ajson%5D%5Btimeout%3A25%5D%3B(node%5B%22highway%22%5D(41.88539386041403%2C12.485854625701903%2C41.89461068146964%2C12.498128414154053)%3Bway%5B%22highway%22%5D(41.88539386041403%2C12.485854625701903%2C41.89461068146964%2C12.498128414154053)%3Brelation%5B%22highway%22%5D(41.88539386041403%2C12.485854625701903%2C41.89461068146964%2C12.498128414154053)%3B)%3Bout%20body%3B%3E%3Bout%20skel%20qt%3B&target=compact')
#
# url <- curlEscape('[out:json][timeout:25];(node["highway"](41.88539386041403,12.485854625701903,41.89461068146964,12.498128414154053);way["highway"](41.88539386041403,12.485854625701903,41.89461068146964,12.498128414154053);relation["highway"](41.88539386041403,12.485854625701903,41.89461068146964,12.498128414154053););out body;>;out skel qt;')
#
# url <- curlEscape('[timeout:25];(node["highway"](41.88539386041403,12.485854625701903,41.89461068146964,12.498128414154053);way["highway"](41.88539386041403,12.485854625701903,41.89461068146964,12.498128414154053);relation["highway"](41.88539386041403,12.485854625701903,41.89461068146964,12.498128414154053););out body;>;out skel qt;')
#
# url <- paste0("http://overpass-api.de/api/interpreter?data=", url)
#
# # I've got
# bb
#
# # I want
# bb[1]
# bb[2]
# bb[3]
# bb[4]
#
# cb <- osmar::corner_bbox(bb)
# cb <- osmar::corner_bbox(bb[1],
#   bb[2],
#   bb[3],
#   bb[4])
#
# downloader::download(url, "/tmp/out.osm")
#
# src <- osmsource_file("/tmp/out.osm")
# getroads <- get_osm(x = cb, src = src)
#
# test <- rgdal::readOGR("/tmp/out.geojson", "OGRGeoJSON")
# test <- geojsonio::geojson_read("/tmp/out.geojson")
# str(test)
# plot(test)
#
# ways1 <- jsonlite::fromJSON(url) # works
# ways1 <- geojsonio::geojson_read(url)
# plot(ways1)
#
#
# content(res)
#
# http://overpass-api.de/api/interpreter?data=%5Bout%3Ajson%5D%5Btimeout%3A25%5D%3B%28node%5B%22highway%22%5D%2841%2E88539386041403%2C12%2E485854625701903%2C41%2E89461068146964%2C12%2E498128414154053%29%3Bway%5B%22highway%22%5D%2841%2E88539386041403%2C12%2E485854625701903%2C41%2E89461068146964%2C12%2E498128414154053%29%3Brelation%5B%22highway%22%5D%2841%2E88539386041403%2C12%2E485854625701903%2C41%2E89461068146964%2C12%2E498128414154053%29%3B%29%3Bout%20body%3B%3E%3Bout%20skel%20qt%3B%0A
#
#
# ?POST
# doc <- POST(url = "http://www.overpass-api.de/api/interpreter",
#     "/tmp/query.xml")
# content(doc)
# postForm(uri = "www.overpass-api.de/api/interpreter", )
#
# cb <- osmar::corner_bbox(bb[1],
#   bb[2],
#   bb[3],
#   bb[4])
#
# src <- osmar::osmsource_osmosis(file = "/tmp/output.osm")
# plot(src)
# osmnet <- osmar::get_osm(x = cb, src = src)