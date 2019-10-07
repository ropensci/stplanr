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
#' @aliases cents_sf
#' @examples
#' \dontrun{
#' cents
#' plot(cents)
#' }
#'
#' @docType data
#' @keywords datasets
#' @name cents
#' @usage data(cents)
#' @format A spatial dataset with 8 rows and 5 variables
NULL

#' data frame of commuter flows
#'
#'
#' This dataset represents commuter flows (work travel) between origin
#' and destination zones (see [cents()]).
#' The data is from the UK and is available as open data:
#' <http://wicid.ukdataservice.ac.uk/>.
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
#' the `cents` georeferenced spatial objects.
#' @family example data
#' @examples
#' \dontrun{
#' # This is how the dataset was constructed - see
#' # https://github.com/npct/pct - if download to ~/repos
#' flow <- readRDS("~/repos/pct/pct-data/national/flow.Rds")
#' data(cents)
#' o <- flow$Area.of.residence %in% cents$geo_code[-1]
#' d <- flow$Area.of.workplace %in% cents$geo_code[-1]
#' flow <- flow[o & d, ] # subset flows with o and d in study area
#' library(devtools)
#' flow$id <- paste(flow$Area.of.residence, flow$Area.of.workplace)
#' use_data(flow, overwrite = TRUE)
#'
#' # Convert flows to spatial lines dataset
#' flowlines <- od2line(flow = flow, zones = cents)
#' # use_data(flowlines, overwrite = TRUE)
#'
#' # Convert flows to routes
#' routes_fast <- line2route(l = flowlines, plan = "fastest")
#' routes_slow <- line2route(l = flowlines, plan = "quietest")
#'
#' use_data(routes_fast)
#' use_data(routes_slow)
#' routes_fast_sf <- sf::st_as_sf(routes_fast)
#' routes_slow_sf <- sf::st_as_sf(routes_slow)
#' }
#'
#' @docType data
#' @keywords datasets
#' @name flow
#' @usage data(flow)
#' @format A data frame with 49 rows and 15 columns
NULL
#' data frame of invented
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
#' Example destinations data
#'
#' This dataset represents trip destinations on a different geographic
#' level than the origins stored in the object `cents`.
#' @family example data
#' @examples
#' \dontrun{
#' # This is how the dataset was constructed - see
#' # http://cowz.geodata.soton.ac.uk/download/
#' download.file(
#'   "http://cowz.geodata.soton.ac.uk/download/files/COWZ_EW_2011_BFC.zip",
#'   "COWZ_EW_2011_BFC.zip"
#' )
#' unzip("COWZ_EW_2011_BFC.zip")
#' wz <- raster::shapefile("COWZ_EW_2011_BFC.shp")
#' to_remove <- list.files(pattern = "COWZ", full.names = TRUE, recursive = TRUE)
#' file.remove(to_remove)
#' proj4string(wz)
#' wz <- sp::spTransform(wz, proj4string(zones))
#' destination_zones <- wz[zones, ]
#' plot(destination_zones)
#' devtools::use_data(destination_zones)
#' head(destination_zones@data)
#' destinations <- rgeos::gCentroid(destinations, byid = TRUE)
#' destinations <- sp::SpatialPointsDataFrame(destinations, destination_zones@data)
#' devtools::use_data(destinations, overwrite = TRUE)
#' destinations_sf <- sf::st_as_sf(destinations)
#' devtools::use_data(destinations_sf)
#' }
#' @docType data
#' @keywords datasets
#' @name destination_zones
#' @aliases destinations destinations_sf
#' @usage data(destination_zones)
#' @format A spatial dataset with 87 features
NULL
#' spatial lines dataset of commuter flows
#'
#'
#' Flow data after conversion to a spatial format
#' with [od2line()] (see [flow()]).
#'
#' @family example data
#' @docType data
#' @keywords datasets
#' @name flowlines
#' @aliases flowlines_sf
#' @format A spatial lines dataset with 49 rows and 15 columns
NULL

#' spatial lines dataset of commuter flows on the travel network
#'
#'
#' Simulated travel route allocated to the transport network
#' representing the 'fastest' between [cents()]
#' objects
#' with [od2line()] (see [flow()]).
#'
#' @family example data
#' @docType data
#' @keywords datasets
#' @name routes_fast
#' @usage data(routes_fast)
#' @format A spatial lines dataset with 49 rows and 15 columns
#' @aliases routes_fast_sf
NULL

#' spatial lines dataset of commuter flows on the travel network
#'
#'
#' Simulated travel route allocated to the transport network
#' representing the 'quietest' between [cents()]
#' objects
#' with [od2line()] (see [flow()]).
#'
#' @family example data
#' @docType data
#' @keywords datasets
#' @name routes_slow
#' @usage data(routes_slow)
#' @format A spatial lines dataset 49 rows and 15 columns
#' @aliases routes_slow_sf
NULL

#' Spatial polygons of home locations for flow analysis.
#'
#'  These correspond to the [cents()] data.
#'
#' \itemize{
#'   \item geo_code. the official code of the zone
#' }
#'
#' @examples
#' zones
#' zones_sf
#' plot(zones_sf)
#' @docType data
#' @keywords datasets
#' @name zones
#' @aliases zones_sf
NULL

#' spatial lines dataset representing a route network
#'
#'
#' The flow of commuters using different segments of the road network represented in the
#' [flowlines()] and [routes_fast()] datasets
#'
#' @family example data
#' @docType data
#' @keywords datasets
#' @name route_network
#' @aliases route_network_sf
#' @usage data(route_network)
#' @format A spatial lines dataset 80 rows and 1 column
#' @examples
#' \dontrun{
#' # Generate route network
#' route_network <- overline(routes_fast, "All", fun = sum)
#' route_network_sf <- sf::st_as_sf(route_network)
#' }
NULL

#' SpatialPointsDataFrame representing road traffic deaths
#'
#' This dataset represents the type of data downloaded and cleaned
#' using stplanr functions. It represents a very small sample (with most variables stripped)
#' of open data from the UK's Stats19 dataset.
#'
#' @docType data
#' @keywords datasets
#' @name ca_local
#' @usage data(ca_local)
#' @format A SpatialPointsDataFrame with 11 rows and 2 columns
#' @examples
#' \dontrun{
#' # Generate data
#' ac <- read_stats19_ac()
#' ca <- read_stats19_ca()
#' ve <- read_stats19_ve()
#' library(dplyr)
#' ca_ac <- inner_join(ca, ac)
#' ca_cycle <- ca_ac %>%
#'   filter(Casualty_Severity == "Fatal" & !is.na(Latitude)) %>%
#'   select(Age = Age_of_Casualty, Mode = Casualty_Type, Longitude, Latitude)
#' ca_sp <- sp::SpatialPointsDataFrame(coords = ca_cycle[3:4], data = ca_cycle[1:2])
#' data("route_network")
#' proj4string(ca_sp) <- proj4string(route_network)
#' bb <- bb2poly(route_network)
#' ca_local <- ca_sp[bb, ]
#' }
NULL

#' Line polygon
#'
#' This dataset represents road width for testing.
#' @docType data
#' @keywords datasets
#' @name l_poly
#' @usage data(l_poly)
#' @format A SpatialPolygon
#'
#' @examples
#' \dontrun{
#' l <- routes_fast[13, ]
#' l_poly <- geo_projected(l, rgeos::gBuffer, 8)
#' plot(l_poly)
#' plot(routes_fast, add = TRUE)
#' # allocate road width to relevant line
#' devtools::use_data(l_poly)
#' }
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
