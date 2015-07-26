# OSM tools - for downloading osm data - this is work in progress

# data("flowlines")
# bb <- sp::bbox(flowlines)
# bbox_2_overpass <- function(bb, type = "("){
#   if(type == "["){
#     paste0("[bbox=", paste(bb[c(2,1,4,3)], collapse = ","), "]")
#   } else{
#     paste0("(", paste(bb[c(2,1,4,3)], collapse = ","), ")")
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
# library(downloader)
# downloader::download(get_over(bb), "here.osm")
#
#
# osm_to_geojson("/tmp/output.osm")
# library(sp)
# osm_dat <- rgdal::readOGR("/tmp/output.geojson", "OGRGeoJSON") # fail
# osm_dat <- geojsonio::geojson_read("/tmp/output.geojson")
# osm_dat$features
#
# osm_paths <- osm_dat$features
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