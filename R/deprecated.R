#' Deprecated functions in stplanr
#'
#' These functions are depreciated and will be removed:
#'
#' @name stplanr-deprecated
NULL

#' Simplify geometry of spatial objects with the mapshaper library
#'
#' @section Details:
#'
#' Note: more advance R/mapshaper tools are provided by the rmapshaper
#' package: <https://github.com/ateucher/rmapshaper>.
#'
#' Calls the JavaScript command-line GIS application mapshaper
#' (<https://github.com/mbloch/mapshaper>) from the system
#' to simplify geographic features, and then tidies up.
#' mapshaper must be installed and available to [system()].
#' `mapshape` writes new a file to disk.
#' Thanks to Richard and Adrian Ellison for demonstrating this in R.
#'
#' @param shp A spatial object to be simplified.
#' @param percent A number between 1 and 100 stating how aggressively to simplify
#'  the object (1 is a very aggressive simplification)
#' @param ms_options Text string of options passed to mapshaper such as
#' @param dsn The name of the temporary file to write to (deleted after use)
#' @param silent Logical determining whether the function call is printed to screen
#' `no-topology` (a flag) and `snap-interval=1` (a key value pair).
#' See the mapshaper documentation for details:
#' <https://github.com/mbloch/mapshaper/wiki/Command-Reference>.
#'
#' The percent argument refers to the percentage of removable points to retain.
#' So `percent = 1` is a very aggressive simplication, saving a huge amount of
#' hard-disk space.
#' @family geo
#' @export
#' @examples
#' \dontrun{
#' shp <- routes_fast[2, ]
#' plot(shp)
#' rfs10 <- mapshape(shp)
#' rfs5 <- mapshape(shp, percent = 5)
#' rfs1 <- mapshape(shp, percent = 1)
#' plot(rfs10, add = TRUE, col = "red")
#' plot(rfs5, add = TRUE, col = "blue")
#' plot(rfs1, add = TRUE, col = "grey")
#' # snap the lines to the nearest interval
#' rfs_int <- mapshape(shp, ms_options = "snap-interval=0.001")
#' plot(shp)
#' plot(rfs_int, add = TRUE)
#' mapshape(routes_fast_sf[2, ])
#' }
mapshape <- function(shp, percent = 10, ms_options = "", dsn = "mapshape", silent = FALSE) {
  .Deprecated(new = "simplify", package = "rmapshaper")
  shp_filename <- paste0(dsn, ".shp")
  new_filename <- paste0(dsn, "-ms.shp")
  if (!mapshape_available()) stop("mapshaper not available on this system")
  is_sp <- is(shp, "Spatial")
  if (is_sp) {
    shp <- sf::st_as_sf(shp)
  }
  sf::write_sf(shp, shp_filename, delete_layer = TRUE)
  cmd <- paste0("mapshaper ", ms_options, " ", shp_filename, " -simplify ", percent, "% -o ", new_filename)
  if (!silent) print(paste0("Running the following command from the system: ", cmd))
  system(cmd, ignore.stderr = TRUE)
  new_shp <- sf::st_read(paste0(dsn, "-ms.shp"))
  sf::st_crs(new_shp) <- sf::st_crs(shp)
  to_remove <- list.files(pattern = dsn)
  file.remove(to_remove)
  if (is_sp) {
    new_shp <- as(new_shp, "Spatial")
  }
  new_shp
}

#' Does the computer have mapshaper available?
#'
#' This helper function for [mapshape()]
#' determines whether or not the JavaScript library
#' mapshaper is available.
#'
#' @family geo
#' @export
#' @examples
#' \dontrun{
#' mapshape_available()
#' }
mapshape_available <- function() {
  .Deprecated(new = "simplify", package = "rmapshaper")
  suppressWarnings(system("mapshaper --version")) != 127
}
#' Crops spatial object x to the bounding box of spatial object (or matrix) b
#'
#' This function is a cross between the spatial subsetting funtions such as
#' sp::over(), rgeos::gIntersects() etc, and the cropping functions of
#' raster::crop() and rgeos::gIntersection(). The output is the subset of
#' spatial object a with an outline described by a square bounding box.
#' The utility of such a function is illustrated in the following question:
#' <https://gis.stackexchange.com/questions/46954/clip-spatial-object-to-bounding-box-in-r/>.
#' @param shp The spatial object a to be cropped
#' @param bb the bounding box or spatial object that will be used to crop `shp`
#' @family geo
#'
#' @export
#' @examples
#' \dontrun{
#' cb <- sf::st_buffer(cents_sf[8, ], dist = 0.012)
#' plot(cents_sf$geometry)
#' plot(cb, add = TRUE)
#' clipped <- gclip(cents_sf, cb)
#' plot(clipped, add = TRUE)
#' clipped$avslope # gclip also returns the data attribute
#' points(clipped)
#' points(cents[cb, ], col = "red") # note difference
#' gclip(cents_sf, cb)
#' }
gclip <- function(shp, bb) {
  .Deprecated(new = "st_buffer", package = "sf")
  UseMethod("gclip")
}
#' @export
gclip.Spatial <- function(shp, bb) {
  if (class(bb) == "matrix") {
    b_poly <- as(raster::extent(as.vector(t(bb))), "SpatialPolygons")
  }
  else {
    b_poly <- as(raster::extent(bb), "SpatialPolygons")
  }
  clipped <- rgeos::gIntersection(shp, b_poly, byid = TRUE, id = row.names(shp))
  if (grepl("DataFrame", class(shp))) {
    if (grepl("SpatialLines", class(shp)) & grepl("SpatialCollections", class(clipped))) {
      geodata <- data.frame(gclip_id = row.names(clipped@lineobj))
    }
    else {
      geodata <- data.frame(gclip_id = row.names(clipped))
    }
    joindata <- cbind(gclip_id = row.names(shp), shp@data)
    geodata <- dplyr::left_join(geodata, joindata)
    row.names(geodata) <- geodata$gclip_id
    # if the data are SpatialPolygonsDataFrame (based on https://stat.ethz.ch/pipermail/r-sig-geo/2008-January/003052.html)
    if (grepl("SpatialPolygons", class(shp))) {
      # then rebuild SpatialPolygonsDataFrame selecting relevant rows by row.names (row ID values)
      clipped <- sp::SpatialPolygonsDataFrame(clipped, as(shp[row.names(clipped), ], "data.frame"))
    } else if (grepl("SpatialLines", class(shp)) & grepl("SpatialCollections", class(clipped))) {
      clipped <- sp::SpatialLinesDataFrame(clipped@lineobj, geodata)
    } else if (grepl("SpatialLines", class(shp))) {
      clipped <- sp::SpatialLinesDataFrame(clipped, geodata)
    } else { # assumes the data is a SpatialPointsDataFrame
      clipped <- sp::SpatialPointsDataFrame(clipped, geodata)
    }
  }
  clipped@data$gclip_id <- NULL
  clipped
}
#' @export
gclip.sf <- function(shp, bb) {
  shp <- as(shp, "Spatial")
  shp <- gclip.Spatial(shp, as(bb, "Spatial"))
  sf::st_as_sf(shp)
}

#' Match two sets of lines based on similarity
#'
#' This function is a wrapper around gDistance that matches lines based on the Hausdorff distance
#'
#' @param l1 A spatial object
#' @param l2 A spatial object
#' @param threshold The threshold for a match - distances greater than this will not count as matches
#' @param return_sp Should the function return a spatial result (FALSE by default)
#' @family lines
#' @export
#' @examples
#' \dontrun{
#' x1 <- 2:4
#' x2 <- 3:5
#' match(x1, x2) # how the base function works
#' l1 <- flowlines[2:4, ]
#' l2 <- routes_fast[3:5, ]
#' (lmatches <- line_match(l1, l2)) # how the stplanr version works
#' l2matched <- l2[lmatches[!is.na(lmatches)], ]
#' plot(l1)
#' plot(l2, add = TRUE)
#' plot(l2matched, add = TRUE, col = "red") # showing matched routes
#' l2matched2 <- line_match(l1, l2, return_sp = TRUE)
#' identical(l2matched, l2matched2)
#' # decreasing the match likelihood via the threshold
#' line_match(l1, l2, threshold = 0.003)
#' }
line_match <- function(l1, l2, threshold = 0.01, return_sp = FALSE) {
  .Deprecated(new = "st_distance", package = "sf")
  dist_mat <- rgeos::gDistance(l1, l2, byid = TRUE, hausdorff = TRUE)
  closest <- apply(dist_mat, 2, which.min)
  closest_values <- apply(dist_mat, 2, min)
  closest[closest_values > threshold] <- NA
  sel_na <- is.na(closest)
  if (return_sp) {
    l2matched <- l2[closest[!sel_na], ]
    match_num <- names(closest)[which(!sel_na)]
    if (is(l2, "SpatialLinesDataFrame")) {
      l2matched$match <- match_num
    } else {
      l2matched <- sp::SpatialLinesDataFrame(l2matched, data = data.frame(match_num), match.ID = FALSE)
    }
    return(l2matched)
  } else {
    return(unname(closest))
  }
}
