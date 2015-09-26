# rotateProj = function(spobj, angle) {
#   # get bounding box as spatial points object
#   boxpts = SpatialPoints(t(bbox(spobj)), proj4string = CRS(proj4string(spobj)))
#   # convert to lat-long
#   boxLL = bbox(spTransform(boxpts, CRS("+init=epsg:4326")))
#   # find the centre
#   llc = apply(boxLL, 1, mean)
#   # construct the proj4 string
#   prj = paste0("+proj=omerc +lat_0=", llc[2], " +lonc=", llc[1], " +alpha=",
#     angle, " +gamma=0.0 +k=1.000000 +x_0=0.000 +y_0=0.000 +ellps=WGS84 +units=m ")
#   # return as a CRS:
#   CRS(prj)
# }
#
# library(sp)
# library(rgdal)
# library(tmap)
#
# downloader::download("https://www.openstreetmap.org/trace/1619756/data", "test.gpx")
#
# r <-readOGR(dsn = "test.gpx", layer = "tracks")
# r <- spTransform(r, CRS("+init=epsg:27700"))
# rproj <- rotateProj(rs, 90) # rotate projection for plotting
# r <- spTransform(r, rproj)
# rs <- rgeos::gSimplify(r, 1000) # snap to nearest km
# qtm(r) + qtm(rs, line.col = "red") + tm_layout(draw.frame = F) + tm_scale_bar()
#
#
#
# plot(r)
