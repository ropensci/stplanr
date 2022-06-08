#' Spatial points representing home locations
#'
#' These points represent population-weighted centroids of Medium Super Output Area (MSOA) zones within a 1 mile radius of of my home when I was writing this package.
#'
#' \itemize{
#'   \item geo_code the official code of the zone
#'   \item MSOA11NM name zone name
#'   \item percent_fem the percent female
#'   \item avslope average gradient of the zone
#' }
#'
#' Cents was generated from the data repository pct-data: https://github.com/npct/pct-data. This data was accessed from within the pct repo: https://github.com/npct/pct, using the following code:
#' @examples
#' cents_sf
#' @docType data
#' @keywords datasets
#' @name cents_sf
#' @format A spatial dataset with 8 rows and 5 columns
NULL

#' Data frame of commuter flows
#'
#'
#' This dataset represents commuter flows (work travel) between origin
#' and destination zones.
#' The data is from the UK and is available as open data:
#' <https://wicid.ukdataservice.ac.uk/>.
#'
#' The variables are as follows:
#'
#' \itemize{
#'   \item Area.of.residence. id of origin zone
#'   \item Area.of.workplace id of destination zone
#'   \item All. Travel to work flows by all modes
#'   \item `[,4:15]`. Flows for different modes
#'   \item id. unique id of flow
#' }
#' Although these variable names are unique to UK data, the data
#' structure is generalisable and typical of flow data from any source.
#' The key variables are the origin and destination ids, which link to
#' the georeferenced spatial objects.
#' @family example data
#' @docType data
#' @keywords datasets
#' @name flow
#' @format A data frame with 49 rows and 15 columns
NULL

#' Data frame of invented
#' commuter flows with destinations in a different layer than the origins
#'
#' @family example data
#' @examples
#' \dontrun{
#' # This is how the dataset was constructed
#' flow_dests <- flow
#' flow_dests$Area.of.workplace <- sample(x = destinations$WZ11CD, size = nrow(flow))
#' flow_dests <- dplyr::rename(flow_dests, WZ11CD = Area.of.workplace)
#' devtools::use_data(flow_dests)
#' }
#'
#' @docType data
#' @keywords datasets
#' @name flow_dests
#' @usage data(flow_dests)
#' @format A data frame with 49 rows and 15 columns
NULL

#' Spatial lines dataset of commuter flows on the travel network
#'
#'
#' Simulated travel route allocated to the transport network
#' representing the 'fastest' between `cents_sf`
#' objects.
#'
#' @family example data
#' @docType data
#' @keywords datasets
#' @name routes_fast_sf
#' @usage routes_fast_sf
#' @format A spatial lines dataset with 49 rows and 15 columns
NULL

#' Spatial lines dataset of commuter flows on the travel network
#'
#'
#' Simulated travel route allocated to the transport network
#' representing the 'quietest' between `cents_sf`.
#'
#' @family example data
#' @docType data
#' @keywords datasets
#' @name routes_slow_sf
#' @format A spatial lines dataset 49 rows and 15 columns
NULL

#' Spatial polygons of home locations for flow analysis.
#'
#' These correspond to the `cents_sf` data.
#'
#' \itemize{
#'   \item geo_code. the official code of the zone
#' }
#'
#' @examples
#' library(sf)
#' zones_sf
#' plot(zones_sf)
#' @docType data
#' @keywords datasets
#' @name zones_sf
NULL

#' Spatial lines dataset of commuter flows
#'
#'
#' Flow data after conversion to a spatial format..
#'
#' @family example data
#' @docType data
#' @keywords datasets
#' @name flowlines_sf
#' @format A spatial lines dataset with 49 rows and 15 columns
NULL

#' spatial lines dataset representing a route network
#'
#'
#' The flow of commuters using different segments of the road network represented in the
#' [flowlines_sf()] and [routes_fast_sf()] datasets
#'
#' @family example data
#' @docType data
#' @keywords datasets
#' @name route_network_sf
#' @format A spatial lines dataset 80 rows and 1 column
NULL

#' Example of OpenStreetMap road network
#' @docType data
#' @keywords datasets
#' @name osm_net_example
#' @format An sf object
#' @examples
#' osm_net_example
NULL

#' Example of origin-destination data from UK Census
#'
#' See `data-raw/generate-data.Rmd` for details on how this was created.
#'
#' @docType data
#' @keywords datasets
#' @name od_data_sample
#' @format A data frame (tibble) object
#' @examples
#' od_data_sample
NULL

#' Example of desire line representations of origin-destination data from UK Census
#'
#' Derived from `od_data_sample` showing movement between points represented in `cents_sf`
#'
#' @docType data
#' @keywords datasets
#' @name od_data_lines
#' @format A data frame (tibble) object
#' @examples
#' od_data_lines
NULL


#' Example segment-level route data
#'
#' See `data-raw/generate-data.Rmd` for details on how this was created.
#' The dataset shows routes between origins and destinations represented in
#' `od_data_lines`
#'
#' @docType data
#' @keywords datasets
#' @name od_data_routes
#' @format A data frame (tibble) object
#' @examples
#' od_data_routes
NULL


#' Example of roundabout data showing problems for SpatialLinesNetwork objects
#'
#' See `data-raw/rnet_roundabout.R` for details on how this was created.
#'
#' @docType data
#' @keywords datasets
#' @name rnet_roundabout
#' @format A sf object
#' @examples
#' rnet_roundabout
NULL

#' Example of overpass data showing problems for SpatialLinesNetwork objects
#'
#' See `data-raw/rnet_overpass.R` for details on how this was created.
#'
#' @docType data
#' @keywords datasets
#' @name rnet_overpass
#' @format A sf object
#' @examples
#' rnet_overpass
NULL

#' Example of cycleway intersection data showing problems for SpatialLinesNetwork objects
#'
#' See `data-raw/rnet_cycleway_intersection` for details on how this was created.
#'
#' @docType data
#' @keywords datasets
#' @name rnet_cycleway_intersection
#' @format A sf object
#' @examples
#' rnet_cycleway_intersection
NULL

#' Example destinations data
#'
#' This dataset represents trip destinations on a different geographic
#' level than the origins stored in the object `cents_sf`.
#' @family example data
#' @examples
#' destinations_sf
#' @docType data
#' @keywords datasets
#' @name destinations_sf
#' @format A spatial dataset with 87 features
NULL
