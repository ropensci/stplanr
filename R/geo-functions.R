#' Write to geojson easily
#'
#' Provides a user-friendly wrapper for rgdal::writeOGR().
#'
#' @param x The object to output
#' @param filename File name of the output geojson
#'
#'
writeGeoJSON <- function(x, filename){
  name <- nm <-deparse(substitute(x))
  rgdal::writeOGR(obj = x, layer = name, dsn = filename, driver = "GeoJSON")
  newname <- paste0(filename, ".geojson")
  file.rename(filename, newname)
}

#' Simplify geometry file of a shapfile.
#'
#' @section Details:
#' This is a wrapper funtion for the open source JavaScript command-line GIS application mapshaper: \url{https://github.com/mbloch/mapshaper} . mapshaper which must be installed locally for gMapshaper to work. Writes \code{gMapshape} writes new file to disk. Thanks to Richard and Adrian Ellison for demonstrating this in R.
#'
#' @param dsn A character string providing the absolute path to the shapefile to simplify.
#' @param percent A number between 0 and 100 stating how aggressively to simplify
#'  the object
#' Percentage of removable points to retain.
#' So \code{percent = 1} is a very aggressive simplication, saving a huge amount of
#' hard-disk space.
#'
#' @examples
#' \dontrun{
#' gMapshape("~/geodata/myShapefile.shp", 5)
#' }
gMapshape <- function(dsn, percent){
  from_layer <- gsub(".shp", replacement = "", dsn)
  to_layer <- paste0(from_layer, "mapshaped_", percent, "%.shp")
  cmd <- paste0("mapshaper ", dsn, " auto-snap -simplify keep-shapes ", percent, "% -o force ", to_layer)
  print(paste0("Attempting to run the following command from the system (requires mapshaper JavaScript library): ", cmd))
  system(cmd, wait = TRUE)
}

