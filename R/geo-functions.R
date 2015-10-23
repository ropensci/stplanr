#' Write to geojson easily
#'
#' Provides a user-friendly wrapper for rgdal::writeOGR(). Note,
#' \code{geojson_write} from the geojsonio package
#' provides the same functionality \url{https://github.com/ropensci/geojsonio}.
#'
#' @inheritParams gclip
#' @param filename File name of the output geojson
writeGeoJSON <- function(shp, filename){
  name <- nm <-deparse(substitute(shp))
  rgdal::writeOGR(obj = shp, layer = name, dsn = filename, driver = "GeoJSON")
  newname <- paste0(filename, ".geojson")
  file.rename(filename, newname)
}

#' Simplify geometry file of a shapfile.
#'
#' @section Details:
#' This is a wrapper funtion for the open source JavaScript command-line GIS application mapshaper: \url{https://github.com/mbloch/mapshaper} . mapshaper which must be installed locally for mapshaper to work. Writes \code{mapshape} writes new file to disk. Thanks to Richard and Adrian Ellison for demonstrating this in R.
#'
#' @param dsn A character string providing the absolute path to the shapefile to simplify.
#' @param percent A number between 0 and 100 stating how aggressively to simplify
#'  the object
#' Percentage of removable points to retain.
#' So \code{percent = 1} is a very aggressive simplication, saving a huge amount of
#' hard-disk space.
#' @export
#' @examples
#' \dontrun{
#' mapshape("~/geodata/myShapefile.shp", 5)
#' }
mapshape <- function(dsn, percent){
  from_layer <- gsub(".shp", replacement = "", dsn)
  to_layer <- paste0(from_layer, "mapshaped_", percent, "%.shp")
  cmd <- paste0("mapshaper ", dsn, " auto-snap -simplify keep-shapes ", percent, "% -o force ", to_layer)
  print(paste0("Attempting to run the following command from the system (requires mapshaper JavaScript library): ", cmd))
  system(cmd, wait = TRUE)
}

#' Crops spatial object x to the bounding box of spatial object (or matrix) b
#'
#' This function is a cross between the spatial subsetting funtions such as
#' sp::over(), rgeos::gIntersects() etc, and the cropping functions of
#' raster::crop() and rgeos::gIntersection(). The output is the subset of
#' spatial object a with an outline described by a square bounding box.
#' The utility of such a function is illustrated in the following question:
#' \url{http://gis.stackexchange.com/questions/46954/clip-spatial-object-to-bounding-box-in-r/}.
#' @param shp The spatial object a to be cropped
#' @param bb the bounding box or spatial object that will be used to crop \code{shp}
#'
#' @export
#' @examples
#' library(sp)
#' data(cents)
#' bb <- bbox(cents)
#' cb <- rgeos::gBuffer(cents[8, ], width = 0.012, byid = TRUE)
#' plot(cents)
#' plot(cb, add = TRUE)
#' clipped <- gclip(cents, cb)
#' row.names(clipped)
#' clipped$avslope # gclip also returns the data attribute
#' points(clipped)
#' points(cents[cb,], col = "red") # note difference
gclip <- function(shp, bb){
  if(class(bb) == "matrix"){
    b_poly <- as(raster::extent(as.vector(t(bb))), "SpatialPolygons")
  }
  else{
    b_poly <- as(raster::extent(bb), "SpatialPolygons")
  }
  clipped <- rgeos::gIntersection(shp, b_poly, byid = TRUE, id = row.names(shp))
  if(grepl("DataFrame", class(shp))){
    if(grepl("SpatialLines", class(shp)) & grepl("SpatialCollections",class(clipped))) {
      geodata <- data.frame(gclip_id = row.names(clipped@lineobj))
    }
    else {
      geodata <- data.frame(gclip_id = row.names(clipped))
    }
    joindata <- cbind(gclip_id = row.names(shp), shp@data)
    geodata <- dplyr::left_join(geodata, joindata)
    row.names(geodata) <- geodata$gclip_id
    #if the data are SpatialPolygonsDataFrame (based on https://stat.ethz.ch/pipermail/r-sig-geo/2008-January/003052.html)
    if(grepl("SpatialPolygons", class(shp))){
      #then rebuild SpatialPolygonsDataFrame selecting relevant rows by row.names (row ID values)
      clipped <- sp::SpatialPolygonsDataFrame(clipped, as(shp[row.names(clipped),], "data.frame"))
    } else if(grepl("SpatialLines", class(shp)) & grepl("SpatialCollections",class(clipped))) {
      clipped <- sp::SpatialLinesDataFrame(clipped@lineobj, geodata)
    } else if(grepl("SpatialLines", class(shp))) {
      clipped <- sp::SpatialLinesDataFrame(clipped, geodata)
    } else { #assumes the data is a SpatialPointsDataFrame
      clipped <- sp::SpatialPointsDataFrame(clipped, geodata)
    }
  }
  clipped@data$gclip_id <- NULL
  clipped
}

#' Scale a bounding box
#'
#' Takes a bounding box as an input and outputs a bounding box of a different size, centred at the same point.
#'
#' @inheritParams gclip
#' @param scale_factor Number determining how much the bounding box will grow or shrink. If the value is 1, the output size will be the same as the input.
#' @export
#' @examples
#' # dput(bbox(cents))
#' bb <- structure(c(-1.55080650299106, 53.8040984493515, -1.51186138683098,
#' 53.828874094091), .Dim = c(2L, 2L), .Dimnames = list(c("coords.x1",
#'   "coords.x2"), c("min", "max")))
#' bb1 <- bbox_scale(bb, 1.05)
#' bb1
#' bb2 <- bbox_scale(bb, 0.75)
#' bb2
#' bb3 <- bbox_scale(bb, 0.1)
#' plot(x = bb1[1,], y = bb1[2,])
#' points(bb2[1,], bb2[2,])
#' points(bb3[1,], bb3[2,])
#' points(bb[1,], bb[2,], col = "red")
#' bbox_scale(bb, 0.75)
bbox_scale <- function(bb, scale_factor){
  b <- (bb - rowMeans(bb)) * scale_factor + rowMeans(bb)
  b
}
