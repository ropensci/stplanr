#' @title \strong{stplanr: Sustainable Transport Planning with R}
#'
#' @name stplanr-package
#' @aliases stplanr stplanr-package
#' @docType package
#' @author Robin Lovelace \email{rob00x@@gmail.com}
#' @keywords package
#' @seealso \url{https://github.com/Robinlovelace/stplanr}
#' @description The stplanr package provides functions to access
#' and analyse data for transportation research, including origin-destination analysis,
#' route allocation and modelling travel patterns.
#'
#' @section Interesting functions:
#' \itemize{
#'  \item \code{\link{overline}} - Aggregate overlaying route lines and data intelligently
#'  \item \code{\link{calc_catchment}} - Create a 'catchment area' to show the areas serving a destination
#'  \item \code{\link{route_cyclestreet}} - Finds the fastest routes for cyclists between two places.
#' }
#'
#' @import sp
#' @import rgdal
#' @import rgeos
#' @import leaflet
#' @import httr
#' @importFrom graphics text
#' @importFrom methods as slot
#' @importFrom stats setNames
#' @importFrom utils read.csv
#' @importFrom openxlsx readWorkbook
#' @importFrom RgoogleMaps getGeoCode
#' @importFrom jsonlite fromJSON
#' @importFrom raster extent crop
NULL