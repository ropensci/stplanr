# stplanr 0.4.1.9000

- `route_graphhopper()` should now work with a local graphhopper instance. See https://github.com/ropensci/stplanr/pull/369

## BUG FIXES

- `destination` now works as an argument in `line2route()` (#368)
- `overline()` now accepts `sf` objects regardless of the name of the geometry column

# stplanr 0.4.1

## NEW FEATURES

- Better error messages if `od2line()` fails due to non-matching ids
- Improved documentation of `od2line()` in the vignette: https://docs.ropensci.org/stplanr/articles/stplanr-od.html#non-matching-ids

# stplanr 0.4.0

## NEW FEATURES

- A family of new functions, including `route_split()`, `rnet_add_node()` and `sln_add_node()` for adding new nodes to routes, route networks and `sfNetwork` objects, closing [#342](https://github.com/ropensci/stplanr/issues/342)
- Updated vignette on route networks, solving [#237](https://github.com/ropensci/stplanr/issues/237), which can be found here: https://docs.ropensci.org/stplanr/articles/stplanr-route-nets.html
- [Fix](https://github.com/ropensci/stplanr/commit/592fba2a6d191135d036af73e7c902c3ef4f758c) in `line2points()`
- `line_to_points()` function removed in favour of `line2point()`.
- New function `sln_clean_graph()` removes unconnected elements of `sfNetwork` objects. Credit to Andrea Gilardi. See (#344).
- New functions `rnet_breakup_vertices()` and `line2vertices()` for breaking up linestrings representing route networks into smaller segments, in preparation for routing. See (#282) (which these functions address) and PR [347](https://github.com/ropensci/stplanr/pull/347) for details.

## BUG FIXES

- Bugs in `route_dodgr()` and associated examples fixed (#348)
- Annoying message printed on load removed (#355)

## OTHER

- Andrea Gilardi added as author.
- Deprecated functions related to road crash (STATS19) data removed

# stplanr 0.3.1

- stplanr now has a logo! See [#334](https://github.com/ropensci/stplanr/issues/334)
- `line_to_points()` depreciated in favour of `od2line()`, the latter function name being more consistent with the package's other functions
- `line2pointsn()` now works with `sf` objects
- Documentation fixes - see [#329](https://github.com/ropensci/stplanr/issues/329)

## OTHER

- Various improvements made to the `stplanr-od` vignette, thanks to Edward Leigh
- URLs updated to link to stplanr's new, official website: https://docs.ropensci.org/stplanr/

# stplanr 0.3.0

## NEW FEATURES

- New functions `od_to_odmatrix()` and `odmatrix_to_od()` to convert between matrix forms of origin-destination data
- New function `od_oneway()` replaces `onewayid()`, works better and is twice as fast
- New `od_id*()` functions provide a range of ways to convert origin-destination pair IDs into a single ID. See [Stackoverflow](https://stackoverflow.com/questions/57235601/how-to-identify-duplicated-ordered-pairs-efficiently/57236658#57236658) and the [issue tracker](https://github.com/ropensci/stplanr/issues/321)
- New vignette [`stplanr-od`](https://docs.ropensci.org/stplanr/articles/stplanr-od.html) provides detailed documentation on the package's OD data handling capabilities

# stplanr 0.2.10

- Fix in documentation. See [#311](https://github.com/ropensci/stplanr/issues/311)

# stplanr 0.2.9

## NEW FEATURES

- New functions `od_aggregate_from()` and `od_aggregate_to()` provide easy ways to aggregate origin-destination pairs. See [#303](https://github.com/ropensci/stplanr/pull/303).
- Updated `overline2()` is now faster and better documented, thanks to [#307](https://github.com/ropensci/stplanr/pull/307)
- Updates to `route_dodgr()` function, which provides an interface to the [dodgr](https://github.com/ATFutures/dodgr) package, accepts wider range of inputs
- Better website and updated function list. See https://ropensci.github.io/stplanr/index.html
- The `sf` method for `overline()` has been updated so it calls the much faster `overline2()` function
- Updated documentation for `route_local()`

## BUG FIXES

- Bug in `sum_network_routes()` fixed. See [#267](https://github.com/ropensci/stplanr/issues/267)

# stplanr 0.2.8

## NEW FEATURES

- The stplanr paper has been published! See it here: https://journal.r-project.org/archive/2018/RJ-2018-053/index.html
- STATS19 functions such as `dl_stats19()` are depreciated. They have been split-out into the new package [`stats19`](https://github.com/ropensci/stats19)
- `route_dodgr()` has now been implemented
- A new function `overline2()` has been added, thanks to Malcolm Morgan. This is faster than `overline()`.
- A substantial refactoring operation has begun. This has resulted in fewer lines of code in the `od` functions, a new `stplanr::od_coords2line()` function, and more support of `sf`
- `route_dodgr()` has been added
- A new example dataset, `osm_net_example`, has been added for local routing purposes.
- A citation to the package has been added. Try `citation("stplanr")`
- The package has a shiny new website thanks to `@maelle`: https://ropensci.github.io/stplanr/
- The package looses its Imports dependency on rgdal, which has been demoted to a Suggests

## BUG FIXES

- An issue with `route_graphhopper()` has been fixed, see https://github.com/ropensci/stplanr/pull/297

# stplanr 0.2.7

## NEW FEATURES

* Various changes to support `dplyr` [0.8.0](https://www.tidyverse.org/articles/2018/12/dplyr-0-8-0-release-candidate/): https://github.com/ropensci/stplanr/pull/275


## BUG FIXES

* Fixed [#272](https://github.com/ropensci/stplanr/issues/272) by removing byvars argument of overline in preparation for overdue overhaul of overline function.

## OTHER

* No longer suggests **tmap** to reduce install times: `install.packages()` installs suggested packages by default

# stplanr 0.2.6

## NEW FEATURES

* New function `route_local()`
* New argument in `line2route()`: `time_sleep` waits a period between each route request

## BUG FIXES

* Issue with `dl_stats19()`, see [#270](https://github.com/ropensci/stplanr/issues/270)
* Make style consistent, see [commit](https://github.com/ropensci/stplanr/commit/521598c329c15ff2dfeb159b0da4ba7b1507b060)
* Various small fixes to documentation and style

# stplanr 0.2.5

## NEW FEATURES

* New function `line_via()` for identifying intermediary points on a transport network

## BUG FIXES

* Bug associated with `SpatialLinesNetwork()` fixed (see [#249](https://github.com/ropensci/stplanr/issues/249))

# stplanr 0.2.4

## NEW FEATURES

* New function `geo_length()` returns numeric vector of line lengths from **sp** or **sf** objects.

## DOCUMENTATION

- `?route_graphhopper` no longer mentions the depreciated 'bike2' profile - see [#246](https://github.com/ropensci/stplanr/issues/246)
- `?route_osrm` mentions that the public API only routes for cars - see [#246](https://github.com/ropensci/stplanr/issues/246)
- Updated `introducing-stplanr` vignette to show new function and make more robust

# stplanr 0.2.3

## NEW FEATURES

* **stplanr** now imports **lwgeom**, needed for `sf::st_length()`, used in `SpatialLinesNetwork()`.
* Plotting behaviour updated for `sfNetwork` objects: now only plots the geometry by default.
* Improved documentation for `SpatialLinesNetwork()` and `plot()` for spatial networks.

## BUG FIXES

* Bug in `sum_network_routes()` fixed (see [#240](https://github.com/ropensci/stplanr/issues/240)).

# stplanr 0.2.2

## NEW FEATURES

* In this release **sp** is demoted from a Depends to an Imports, meaning that all its functions will not be attached to your namespace (it will not be loaded) when you run `library(stplanr)`, making it less tied to **sp**. This is a continuation of the work to support **sf** and will make it easier for the package to work with alternative representations of geographic data.

## BUG FIXES

* Bug in `geo_select_aeq.sf()` was fixed by Jakub Nowosad in pull [#238](https://github.com/ropensci/stplanr/pull/238)
* An issue with `od_aggregate.sf()` was fixed making it much faster

# stplanr 0.2.0

## NEW FEATURES

* This is the largest release since the package was created, with dozens of changes to support simple features - see https://github.com/ropensci/stplanr/pull/198 for details.
* Support for **sf**. The package now support the new spatial class system for most functions.
* New function `geo_bb()` supercedes `bb2poly()`. The new function can return polygons, points and matrix objects determined by the `output` argument. It also allows bounding boxes to be extended in metres, and scaled in x and y dimensions.
* `geo_code()` now uses nominatim by default to find locations on the maps.
* New function `od_coords()` takes a wide range of input data types to return a consistent output representing OD data as a data frame of origin and destination coordinates. This is used behind the scenes to make other functions more modular.

## WORK IN PROGRESS

Plans for the next release

* New generic `route()` function for routing. This is more flexible and user-friendly than the existing `line2route()` and `route_*()` functions it enhances.
* Updated function names to make using **stplanr** easier and more intuitive.


# stplanr 0.1.9

## NEW FEATURES

* Dependency cull: we have removed dependencies on foreach and doParallel
* `route_cyclestreet()` now also called (correctly) `route_cyclestreets()`
* New `geo_code()` function replaces dependency on RGoogleMaps

## BUG FIXES

* See issues closed after the last release with this search term: https://github.com/ropensci/stplanr/issues?utf8=%E2%9C%93&q=is%3Aissue%20closed%3A%3E2017-06-01%20
* Bug with `google_dist()` fixed
* Fixed fails due to breaking changes in dplyr

# stplanr 0.1.8

## NEW FEATURES

* New argument `combinations` added to `sum_network_routes()` so it runs quicker - see [pull/177](https://github.com/ropensci/stplanr/pull/177).
* New examples added to `sum_network_routes()`, `weightfield()` and `find_network_nodes()` - see e.g. `example(sum_network_routes)` for details.
* New dataset `l_poly` [added](https://github.com/ropensci/stplanr/commit/7641760fbd6718352ed74142e5c339f6216afea4).
* **stplanr** now has a website! See [ropensci.github.io/stplanr/](https://ropensci.github.io/stplanr/).

## BUG FIXES

* Serious bug with `SpatialLinesNetwork()` [fixed](https://github.com/ropensci/stplanr/pull/186).
* Depreciated `_each()` **dplyr** functions replaced with equivalent `_at` or `_all` functions. See [here](https://blog.rstudio.org/2016/06/27/dplyr-0-5-0/) for more.

# stplanr 0.1.7

## NEW FEATURES

* There is a new vignette! See [vignettes/stplanr-paper.Rmd](https://github.com/ropensci/stplanr/blob/master/vignettes/stplanr-paper.Rmd) and `vignette("stplanr-paper")` for details.
* The original [`introducing-stplanr`](https://github.com/ropensci/stplanr/blob/master/vignettes/stplanr.Rmd) vignette has been updated. It now provides a more basic introduction for people new to R for spatial and transport data.
* `line2route()` has been refactored to improve error detection and allow `n_processes` arguments. Thanks @nikolai-b. See [pull/151](https://github.com/ropensci/stplanr/pull/151) for details.
* `line_match()` function added, a wrapper around `rgeos::gDistance()`, to find similar routes.
* **RCurl** and **data.table** dependencies have been [removed](https://github.com/ropensci/stplanr/pull/169)
* **leaflet** has been demoted from an import to a suggest. This should reduce install times.
* New functions `od_aggregate()` and `sp_aggregate()` have been [added](https://github.com/ropensci/stplanr/pull/165), to enable OD data to be aggregated to new geographic levels.


## BUG FIXES

* `#141` fixed: `viaroute()` works again.
* [#153](https://github.com/ropensci/stplanr/issues/153) fixed: `bidirectional = TRUE` returns a different result in `line_bearing()` now.

## FUTURE PLANS

* A new branch that uses **sf** is being [tested](https://github.com/ropensci/stplanr/pull/164). We may eventually transition to using simple features classes instead of **sp** classes.

# stplanr 0.1.6

## NEW FEATURES

* `onewayid()` is now a generic function, meaning it can handle spatial and non-spatial data
* New arguments provided for `line2route()` allow you to specify variables to join-by - also has updated and more sensible defaults
* New function `od_id_order()` to put origin-destination ids in order, to identify 2 way duplicates (split out from `onewayid()`)

## BUG FIXES

* See the [issue tracker](https://github.com/ropensci/stplanr/issues?q=is%3Aissue+is%3Aclosed)
* Bug in `route_cyclestreet()` leading `change_elev` and `av_incline` being wrong now fixed
* Bug making variable names with spaces in the id columns failed - now fixed [#138](https://github.com/ropensci/stplanr/issues/138)

stplanr 0.1.5
----------------------------------------------------------------

NEW FEATURES

* New argument destinations added to `od2line()`. See `example(od2line)` for an example.
* New dataset `destinations` for showing how OD matrix with destinations can be converted to spatial data
* New argument `list_output` allows the route information to be saved as a list, allowing `save_raw = TRUE` (which does not return a `Spatial` object) to be passed to the `route_` function.
* tmap dependency removed for faster installs

BUG FIXES

* Bug with `line2route()` (#124) fixed
* Various improvements to documentation

stplanr 0.1.4
----------------------------------------------------------------

NEW FEATURES

* New function `reproject()` is a simple wrapper around `spTransform()` that uses
  `crs_select_aeq()` to convert a spatial object in geographic (lat/lon) coordinates
  into on with projected coordinates, with units of 1 m. This is useful for various
  spatial operations, such as finding the length and area of an object.

* Implement `gprojected()`, a function for performing GIS operations on a temporary, projected, version
  of spatial objects.

* Addition of `line_bearing()` to return the bearing of lines based on start and end points.

* Addition of `angle_diff()` for finding the angular difference between lines: are they roughly parallel or perpendicular?

BUG FIXES

* `line2df()` now works on lines with multiple vertices and is faster.

* Fixes in the examples used to illustrate how `od_dist()` works.

stplanr 0.1.3
----------------------------------------------------------------

NEW FEATURES

* Update to OSRM functions to support API v5.

* New parameter `byvars` in the `overline()` function, to allow disaggregation of results by a grouping variable (see `example(overline)`).

* Faster implementation of `od2line()`: `od2line2()`. Plan is to replace the original if no issues are found with new implementation.

* New function `od2odf()` which converts OD data into a dataframe of origins and destinations (feeds `od2line2()` but also useful as self-standing function).

* New argument `new_proj` in `buff_geo()` allows the results to be exported to any coordinate reference system (CRS).

* New function `gprojected()` generalises concept of `buff_geo()`, building on `crs_select_aeq()` to allow any GIS query to be conducted on a temporary projected version of spatial objects with geographical CRSs.

* New function `od_dist()` can quickly calculate Euclidean distances of OD pairs without converting to spatial objects.

BUG FIXES

* Bug fix in `onewayid()` so it captures all lines.

* Various improvements to documentation and code.

stplanr 0.1.2
----------------------------------------------------------------

NEW FEATURES

* Interface to the Google Distance Matrix `API with dist_google`.

* New transport planning API added, with `route_transportapi_public` (for testing).

* Update to `line2route`, allowing it to accept different routing funtions via the new argument `route_fun` (for testing - tested with `route_fun = route_cyclestreet`).

* New functions for creating origin-destination data frames (`point2odf`) and SpatialLinesDataFrames (`points2flow`).

* Addition of `n_vertices` and `is_linepoint` for identifying the number of vertices in spatial objects and whether the 'line' is really a point.

BUG FIXES

* `line2route` refactored, with 10 fold speed increases on large (1000+) batches of lines.

stplanr 0.1.0
----------------------------------------------------------------

NEW FEATURES

* Addition of new class definition `SpatialLinesNetwork`, methods for `plot`
  and `summary` and functions `calc_network_routes` and `find_network_nodes`
  allowing fast route calculations via igraph and other network analysis
  functions.

* Functions for removing beginning and end of lines: `toptail` and
  `toptailgs`. Helper functions `buff_geo`,
  `crs_select_aeq` and `line2points` added.

* Functionality for reading in the UK's stats19 data: `read_stats19_*`
  functions download, unzip and re-categorise the data.

* `read_table` functions added for reading Australian OD data.

* `decode_gl` added to decode Google polylines and other functions for
  querying and reading data from OSRM services.

* `gtfs2sldf` added to import GTFS routes as SpatialLinesDataFrames.

stplanr 0.0.2
----------------------------------------------------------------

* Published on CRAN
