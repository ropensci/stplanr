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

#' Simplify geometry of spatial objects with the mapshaper library
#'
#' @section Details:
#'
#' Note: more advance R/mapshaper tools are provided by the rmapshaper
#' package: \url{https://github.com/ateucher/rmapshaper}.
#'
#' Calls the JavaScript command-line GIS application mapshaper
#' (\url{https://github.com/mbloch/mapshaper}) from the system
#' to simplify geographic features, and then tidies up.
#' mapshaper must be installed and available to \code{\link{system}}.
#' \code{mapshape} writes new a file to disk.
#' Thanks to Richard and Adrian Ellison for demonstrating this in R.
#'
#' @param shp A spatial object to be simplified.
#' @param percent A number between 1 and 100 stating how aggressively to simplify
#'  the object (1 is a very aggressive simplification)
#' @param ms_options Text string of options passed to mapshaper such as
#' @param dsn The name of the temporary file to write to (deleted after use)
#' @param silent Logical determining whether the function call is printed to screen
#' \code{no-topology} (a flag) and \code{snap-interval=1} (a key value pair).
#' See the mapshaper documentation for details:
#' \url{https://github.com/mbloch/mapshaper/wiki/Command-Reference}.
#'
#' The percent argument refers to the percentage of removable points to retain.
#' So \code{percent = 1} is a very aggressive simplication, saving a huge amount of
#' hard-disk space.
#' @seealso
#' \code{\link[rgeos]{gSimplify}}
#' @export
#' @examples
#' \dontrun{
#' data(routes_fast)
#' shp <- routes_fast[1,]
#' rfs10 <- mapshape(shp)
#' rfs5 <- mapshape(shp, percent = 5)
#' rfs1 <- mapshape(shp, percent = 1)
#' plot(shp)
#' plot(rfs10, add = TRUE, col ="red")
#' plot(rfs5, add = TRUE, col ="blue")
#' plot(rfs1, add = TRUE, col = "grey")
#' # snap the lines to the nearest interval
#' rfs_int <- mapshape(shp, ms_options = "snap-interval=0.001")
#' plot(shp)
#' plot(rfs_int, add = TRUE)
#' }
mapshape <- function(shp, percent = 10, ms_options = "",  dsn = "mapshape", silent = FALSE){
  if(!mapshape_available()) stop("mapshaper not available on this system")
  raster::shapefile(shp, dsn, overwrite = TRUE)
  cmd <- paste0("mapshaper ", ms_options, " ", dsn, ".shp -simplify ", percent, "% -o")
  if(!silent)  print(paste0("Running the following command from the system: ", cmd))
  system(cmd, ignore.stderr = TRUE)
  suppressWarnings(new_shp <- raster::shapefile(paste0(dsn, "-ms.shp")))
  new_shp@data <- shp@data
  proj4string(new_shp) <- proj4string(shp)
  to_remove <- list.files(pattern = dsn)
  file.remove(to_remove)
  new_shp
}

#' Does the computer have mapshaper available?
#'
#' This helper function for \code{\link{mapshape}}
#' determines whether or not the JavaScript library
#' mapshaper is available.
#'
#' @export
#' @examples
#' mapshape_available()
mapshape_available <- function() {
  suppressWarnings(system("mapshaper --version")) != 127
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

#' Convert a bounding box to a SpatialPolygonsDataFrame
#'
#' Takes a bounding box as an input and outputs a box in the form of a polygon
#'
#' @inheritParams gclip
#' @export
#' @examples
#' bb <- structure(c(-1.55080650299106, 53.8040984493515, -1.51186138683098,
#' 53.828874094091), .Dim = c(2L, 2L), .Dimnames = list(c("coords.x1",
#'   "coords.x2"), c("min", "max")))
#' bb1 <- bb2poly(bb)
#' plot(bb1)
bb2poly <- function(bb){
  if(class(bb) == "matrix")
    b_poly <- as(raster::extent(as.vector(t(bb))), "SpatialPolygons") else
      b_poly <- as(raster::extent(bb), "SpatialPolygons")
}

