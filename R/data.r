#' SpatialPointsDataFrame of home locations for flow analysis.
#'
#'  These points represent population-weighted centroids of Medium Super Output Area (MSOA) zones within a 1 mile radius of of my home when I was writing this package.
#'
#' A dataset containing the prices and other attributes of almost 54,000
#'  diamonds. The variables are as follows:
#'
#' \itemize{
#'   \item geo_code. the official code of the zone
#'   \item MSOA11NM. name zone name
#'   \item percent_fem. the percent female
#'   \item avslope. average gradient of the zone
#' }
#'
#' Cents was generated from the data repository pct-data: https://github.com/npct/pct-data. This data was accessed from within the pct repo: https://github.com/npct/pct, using the following code:
#'
#' \dontrun{
#' cents <- rgdal::readOGR(dsn = "pct-data/national/cents.geojson", layer = "OGRGeoJSON")
#' crs <- CRS("+init=epsg:4326")
#' crsuk <- CRS("+init=epsg:27700")
#' cents <- sp::spTransform(x = cents, CRSobj = crsuk)
#' home <- ggmap::geocode("LS7 3HB")
#' home <- sp::SpatialPoints(coords = home, proj4string = crs)
#' home <- sp::spTransform(x = home, CRSobj = crsuk)
#' buf <- rgeos::gBuffer(home, width = 2000)
#' # Check it saved the points OK
#' cents <- cents[buf,]
#' plot(buf)
#' points(cents)
#' load("~/repos/pct/cents.RData")
#' library(devtools)
#' use_data(cents)
#' }
#'
#' @docType data
#' @keywords datasets
#' @name cents
#' @usage data(cents)
#' @format A SpatialPoints with 8 rows and 5 variables
NULL


