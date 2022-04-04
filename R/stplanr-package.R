#' @title \strong{stplanr: Sustainable Transport Planning with R}
#'
#' @name stplanr-package
#' @aliases stplanr stplanr-package
#' @docType package
#' @author Robin Lovelace \email{rob00x@@gmail.com}
#' @keywords package
#' @seealso <https://github.com/ropensci/stplanr>
#' @description The stplanr package provides functions to access
#' and analyse data for transportation research, including origin-destination analysis,
#' route allocation and modelling travel patterns.
#'
#'
#' @import curl
#' @importFrom sp bbox plot spTransform Lines SpatialLines spChFIDs proj4string proj4string<- CRS coordinates
#' @importFrom graphics text
#' @importFrom methods as slot
#' @importFrom stats setNames
#' @importFrom utils read.csv
#' @importFrom raster extent crop
#' @importFrom geosphere distHaversine
#' @importFrom Rcpp evalCpp
#' @importFrom methods is new
#' @importFrom utils download.file tail unzip
#' @importFrom maptools SpatialLinesMidPoints
#' @importFrom rlang .data
#' @importFrom dplyr first last n
NULL
utils::globalVariables(c(".", "n", ".inc", "object", "x", "y", "stplanr_start", "stplanr_end", "stplanr_linkid"))
