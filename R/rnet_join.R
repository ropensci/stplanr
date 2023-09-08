#' Join route networks
#'
#' This is a spatial join function that is enables adding columns to a
#' 'target' route network from a 'source' route
#' network that contains the base geometry, e.g. from OSM
#'
#' The output is an sf object containing polygons representing
#' buffers around the route network in `rnet_x`.
#' The examples below demonstrate how to join attributes from
#' a route network object created with the function [overline()] onto
#' OSM geometries.
#'
#' Note: The main purpose of this function is to join an ID from `rnet_x`
#' onto `rnet_y`. Subsequent steps, e.g. with [dplyr::inner_join()]
#' are needed to join the attributes back onto `rnet_x`.
#' There are rarely 1-to-1 relationships between spatial network geometries
#' so we take care when using this function.
#'
#' See [#505](https://github.com/ropensci/stplanr/issues/505) for details
#' and a link to an interactive example of inputs and outputs shown below.
#'
#' @param rnet_x Target route network, the output will have the same geometries
#'   as features in this object.
#' @param rnet_y Source route network. Columns from this route network object will
#'   be copied across to the new network.
#' @param dist The buffer width around rnet_y in meters. 1 m by default.
#' @param length_y Add a new column called `length_y`? Useful when joining based on
#'   length of segments (e.g. weighted mean). `TRUE` by default.
#' @param key_column The index of the key (unique identifier) column in `rnet_x`.
#' @param subset_x Subset the source route network by the target network before
#'   creating buffers? This can lead to faster and better results. Default:
#'   `TRUE`.
#' @param dist_subset The buffer distance in m to apply when breaking up the
#'   source object `rnet_y`. Default: 5.
#' @param segment_length Should the source route network be split?
#'   `0` by default, meaning no splitting. Values above 0 split the source
#'   into linestrings with a max distance. Around 5 (m) may be a sensible
#'   default for many use cases, the smaller the value the slower the process.
#' @param endCapStyle Type of buffer. See `?sf::st_buffer` for details
#' @param contains Should the join be based on `sf::st_contains` or `sf::st_intersects`?
#'   `TRUE` by default. If `FALSE` the centroid of each segment of `rnet_y` is
#'   used for the join. Note: this can result in incorrectly assigning values
#'   on sideroads, as documented in [#520](https://github.com/ropensci/stplanr/issues/520).
#' @param ... Additional arguments passed to `rnet_subset`.
#' @examples
#' library(sf)
#' library(dplyr)
#' # Uncomment for interactive examples:
#' plot(st_geometry(route_network_small))
#' plot(osm_net_example$geometry, lwd = 5, col = "grey", add = TRUE)
#' plot(route_network_small["flow"], add = TRUE)
#' rnetj = rnet_join(osm_net_example, route_network_small, dist = 9)
#' rnetj2 = rnet_join(osm_net_example, route_network_small, dist = 9, segment_length = 10)
#' # library(mapview)
#' # mapview(rnetj, zcol = "flow") +
#' #   mapview(rnetj2, zcol = "flow") +
#' #   mapview(route_network_small, zcol = "flow")
#' plot(sf::st_geometry(rnetj))
#' plot(rnetj["flow"], add = TRUE)
#' plot(rnetj2["flow"], add = TRUE)
#' plot(route_network_small["flow"], add = TRUE)
#' summary(rnetj2$length_y)
#' rnetj_summary = rnetj2 %>%
#'   filter(!is.na(length_y)) %>%
#'   sf::st_drop_geometry() %>%
#'   group_by(osm_id) %>%
#'     summarise(
#'       flow = weighted.mean(flow, length_y, na.rm = TRUE),
#'       )
#' osm_joined_rnet = dplyr::left_join(osm_net_example, rnetj_summary)
#' plot(sf::st_geometry(route_network_small))
#' plot(route_network_small["flow"], lwd = 3, add = TRUE)
#' plot(sf::st_geometry(osm_joined_rnet), add = TRUE)
#' # plot(osm_joined_rnet[c("flow")], lwd = 9, add = TRUE)
#' # Improve fit between geometries and performance by subsetting rnet_x
#' osm_subset = rnet_subset(osm_net_example, route_network_small, dist = 5)
#' osm_joined_rnet = dplyr::left_join(osm_subset, rnetj_summary)
#' plot(route_network_small["flow"])
#' # plot(osm_joined_rnet[c("flow")])
#' # mapview(joined_network) +
#' #   mapview(route_network_small)
#' @export
rnet_join = function(rnet_x, rnet_y, dist = 5, length_y = TRUE, key_column = 1,
                     subset_x = TRUE, dist_subset = NULL, segment_length = 0,
                     endCapStyle = "FLAT", contains = FALSE, ...) {
  if (is.null(dist_subset)) {
    dist_subset = dist + 1
  }
  if (subset_x) {
    rnet_x = rnet_subset(rnet_x, rnet_y, dist = dist_subset, ...)
  }
  rnet_x_buffer = geo_buffer(rnet_x, dist = dist, nQuadSegs = 2, endCapStyle = endCapStyle)
  if (segment_length > 0) {
    rnet_y = line_segment(rnet_y, segment_length = segment_length)
  }
  if (length_y) {
    rnet_y$length_y = as.numeric(sf::st_length(rnet_y))
  }
  # browser()
  if (contains) {
    rnetj = sf::st_join(rnet_x_buffer[key_column], rnet_y, join = sf::st_contains)
    # # For debugging:
    # library(tmap)
    # tmap_mode("view")
    # tm_shape(rnet_y) + tm_lines(lwd = 3) + qtm(rnetj) + qtm(rnet_x) +
    #   qtm(osm_net_example)
  } else {
    # If not using 'contains', find the centroids of 'rnet_y'
    rnet_y_centroids = sf::st_centroid(rnet_y)
    # Store the corresponding line geometries 
    rnet_y_centroids$corr_line_geometry = rnet_y$geometry
    rnetj = sf::st_join(rnet_x_buffer[key_column], rnet_y_centroids)
  }
  rnetj
}

#' Subset one route network based on overlaps with another
#'
#' @param rnet_x The route network to be subset
#' @param rnet_y The subsetting route network
#' @param dist The buffer width around y in meters. 1 m by default.
#' @param crop Crop `rnet_x`? `TRUE` is the default
#' @param min_length Segments shorter than this multiple of dist
#'   *and* which were longer
#'   before the cropping process will be removed. 3 by default.
#' @param rm_disconnected Remove ways that are
#' @export
#' @examples
#' rnet_x = osm_net_example[1]
#' rnet_y = route_network_small["flow"]
#' plot(rnet_x$geometry, lwd = 5)
#' plot(rnet_y$geometry, add = TRUE, col = "red", lwd = 3)
#' rnet_x_subset = rnet_subset(rnet_x, rnet_y)
#' plot(rnet_x_subset, add = TRUE, col = "blue")
rnet_subset = function(rnet_x, rnet_y, dist = 10, crop = TRUE, min_length = 20, rm_disconnected = TRUE) {
  # browser()
  rnet_x_original = data.frame(
    id = rnet_x[[1]],
    length_original = as.numeric(sf::st_length(rnet_x))
    )
  names(rnet_x_original)[1] = names(rnet_x)[1]
  rnet_y_union = sf::st_union(rnet_y)
  rnet_y_buffer = stplanr::geo_buffer(rnet_y_union, dist = dist, nQuadSegs = 2)
  if(crop) {
    rnet_x = sf::st_intersection(rnet_x, rnet_y_buffer)
    rnet_x = line_cast(rnet_x)
  } else {
    rnet_x = rnet_x[rnet_y_buffer, , op = sf::st_within]
  }
  if(min_length > 0) {
    rnet_x$length_new = as.numeric(sf::st_length(rnet_x))
    rnet_x_joined = dplyr::left_join(rnet_x, rnet_x_original)
    sel_short_remove = rnet_x_joined$length_new < min_length
    sel_changed_remove = rnet_x_joined$length_new < rnet_x_joined$length_original
    sel_remove = sel_short_remove & sel_changed_remove

    # browser()
    # # Testing:
    # # ids_to_keep = rnet_x_joined[[1]][!sel_remove]
    # rnet_x_joined[sel_remove, ]
    # plot(rnet_x_joined$geometry[sel_remove])
    # plot(rnet_x_joined$geometry[!sel_remove])
    # rnet_x_original_full = rnet_x
    # rnet_x = rnet_x[rnet_x[[1]] %in% ids_to_keep, ]

    rnet_x = rnet_x_joined[!sel_remove, ]
  }
  if(rm_disconnected) {
    rnet_x = rnet_connected(rnet_x)
  }
  rnet_x
}

#' Convert multilinestring object into linestrings
#'
#' Without losing vertices
#'
#' @param x Linestring object
#' @export
line_cast = function(x) {
  sf::st_cast(sf::st_cast(x, "MULTILINESTRING"), "LINESTRING")
}

#' Merge route networks, keeping attributes with aggregating functions
#'
#' @inheritParams rnet_join
#' @param sum_flows Should flows be summed? `TRUE` by default.
#' @param funs A named list of functions to apply to named columns, e.g.:
#'   `list(flow = sum, length = mean)`. The default is to sum all numeric
#'   columns.
#' @param ... Additional arguments passed to `rnet_join`.
#' @export
#' @examples
#' # The source object:
#' rnet_y = route_network_small["flow"]
#' # The target object
#' rnet_x = rnet_subset(osm_net_example[1], rnet_y)
#' plot(rnet_x$geometry, lwd = 5)
#' plot(rnet_y$geometry, add = TRUE, col = "red", lwd = 2)
#' rnet_y$quietness = rnorm(nrow(rnet_y))
#' funs = list(flow = sum, quietness = mean)
#' rnet_merged = rnet_merge(rnet_x[1], rnet_y[c("flow", "quietness")],
#'                          dist = 9, segment_length = 20, funs = funs)
#' plot(rnet_y$geometry, lwd = 5, col = "lightgrey")
#' plot(rnet_merged["flow"], add = TRUE, lwd = 2)
#'
#' # # Larger example
#' # system("gh release list")
#' # system("gh release upload v1.0.2 rnet_*")
#' # List the files released in v1.0.2:
#' # system("gh release download v1.0.2")
#' # rnet_x = sf::read_sf("rnet_x_ed.geojson")
#' # rnet_y = sf::read_sf("rnet_y_ed.geojson")
#' # rnet_merged = rnet_merge(rnet_x, rnet_y, dist = 9, segment_length = 20, funs = funs)
#' @return An sf object with the same geometry as `rnet_x`
# A function to merge two route networks based on certain conditions
# A function to merge two route networks based on certain conditions
rnet_merge <- function(rnet_x, rnet_y, dist = 5, funs = NULL, sum_flows = TRUE, ...) {
  
  # If no aggregation functions are provided, default to summing numeric columns
  if (is.null(funs)) {
    funs = list()
    for (col in names(rnet_y)) {
      if (is.numeric(rnet_y[[col]])) {
        funs[[col]] = sum
      }
    }
  }
  
  # Identify which columns should be summed
  sum_cols = sapply(funs, function(f) identical(f, sum))
  sum_cols = names(funs)[which(sum_cols)]
  
  # Join the route networks based on the given distance
  rnetj = rnet_join(rnet_x, rnet_y, dist = dist)
  
  # Calculate the angle between the corresponding lines in the merged network
  rnetj$angle = sapply(1:nrow(rnetj), function(i) {
    calculate_angle(get_vector(rnetj$corr_line_geometry[[i]]), get_vector(rnetj$geometry[[i]]))
  })
  
  # Drop geometry data to work with a standard data frame
  rnetj_df = sf::st_drop_geometry(rnetj)
  
  # Loop through the provided aggregation functions and apply them to the merged network
  res_list = lapply(seq(length(funs)), function(i) {
    nm = names(funs[i])
    fn = funs[[i]]
    
    # Group by the first column and ensure each group has consistent geometry and angle data
    intermediate_df = rnetj_df %>%
      dplyr::group_by_at(1) %>%
      dplyr::mutate(corr_line_geometry = first(corr_line_geometry),
                    angle = first(angle)) %>%
      dplyr::ungroup()

    # If the function is sum and sum_flows is TRUE, weight the sum by the length of the line
    if (identical(fn, sum) && sum_flows) {
      res = intermediate_df %>%
        dplyr::group_by_at(1) %>%
        dplyr::summarise(dplyr::across(dplyr::matches(nm), function(x) sum(x * length_y)), .groups = 'drop')
    } else {
      res = intermediate_df %>%
        dplyr::group_by_at(1) %>%
        dplyr::summarise(dplyr::across(dplyr::matches(nm), fn), .groups = 'drop')
    }

    # Add back the 'corr_line_geometry' and 'angle' columns
    res = dplyr::left_join(res, unique(intermediate_df %>% dplyr::select(identifier, corr_line_geometry, angle)), by = "identifier")

    # Rename the summarised column
    names(res)[which(names(res) == nm)[1]] = nm

    # If this isn't the first loop iteration, drop the 'identifier' column to avoid duplication
    if(i > 1) {
      res = res %>% dplyr::select(-matches("^identifier"))
    }

    res
  })

  # Combine all the results into one data frame
  res_df = dplyr::bind_cols(res_list)
  res_df <- res_df %>%
    dplyr::select(identifier, value, Quietness, corr_line_geometry = corr_line_geometry...3, angle = angle...4)
  
  # Filter out lines that don't have an angle close to 90 degrees
  mask <- (res_df$angle < 89) | (res_df$angle > 91)
  filtered_res_df <- res_df[mask, ]
  
  # Join the filtered results back to the original route network
  res_sf = dplyr::left_join(rnet_x, filtered_res_df)

  # If sum_flows is TRUE, adjust the flow values based on the length of the lines
  if (sum_flows) {
    res_sf$length_x = as.numeric(sf::st_length(res_sf))
    for(i in sum_cols) {
      length_y = as.numeric(sf::st_length(rnet_y))
      res_sf[[i]] = res_sf[[i]] / res_sf$length_x
      over_estimate = sum(res_sf[[i]] * res_sf$length_x, na.rm = TRUE) /
        sum(rnet_y[[i]] * length_y, na.rm = TRUE)
      res_sf[[i]] = res_sf[[i]] / over_estimate
    }
  }
  
  res_sf
}

# A function to extract a vector from a LINESTRING geometry
get_vector <- function(line) {
  if (sf::st_is_empty(line)) {
    # warning("Encountered an empty LINESTRING. Returning NULL.")
    return(NULL)
  }

  coords <- sf::st_coordinates(line)

  # Check if coords is empty or has insufficient dimensions
  if (is.null(coords) || nrow(coords) < 2 || ncol(coords) < 2) {
    stop("Insufficient coordinate data")
  }

  start <- coords[1, 1:2]
  end <- coords[2, 1:2]

  return(c(end[1] - start[1], end[2] - start[2]))
}

# A function to calculate the angle between two vectors
calculate_angle <- function(vector1, vector2) {
  dot_product <- sum(vector1 * vector2)
  magnitude_product <- sqrt(sum(vector1^2)) * sqrt(sum(vector2^2))
  cos_angle <- dot_product / magnitude_product
  angle <- acos(cos_angle) * (180 / pi)
  return(angle)
}
