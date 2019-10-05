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
#' @section Interesting functions:
#' \itemize{
#'  \item [overline()] - Aggregate overlaying route lines and data intelligently
#'  \item [calc_catchment()] - Create a 'catchment area' to show the areas serving a destination
#'  \item [route_cyclestreet()] - Finds the fastest routes for cyclists between two places.
#' }
#'
#' @import curl
#' @importFrom sp bbox plot spTransform Lines SpatialLines spChFIDs proj4string proj4string<- CRS coordinates
#' @importFrom rgeos gBuffer gLength gIntersects gIntersection gArea gSimplify
#' @importFrom graphics text
#' @importFrom methods as slot
#' @importFrom stats setNames
#' @importFrom utils read.csv
#' @importFrom openxlsx readWorkbook
#' @importFrom raster extent crop
#' @importFrom geosphere distHaversine
#' @importFrom Rcpp evalCpp
#' @importFrom igraph graph E
#' @importFrom methods is new
#' @importFrom utils download.file tail unzip
#' @importFrom maptools SpatialLinesMidPoints
#' @importFrom rlang .data
#' @importFrom dplyr first last n
#' @useDynLib stplanr
NULL
utils::globalVariables(c(".", "n", ".inc"))

