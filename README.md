
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Build
Status](https://travis-ci.org/ropensci/stplanr.svg?branch=master)](https://travis-ci.org/ropensci/stplanr)
[![rstudio mirror
downloads](http://cranlogs.r-pkg.org/badges/stplanr)](https://github.com/metacran/cranlogs.app)
[![](https://cranlogs.r-pkg.org/badges/grand-total/stplanr)](https://cran.rstudio.com/web/packages/stplanr/index.html)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/stplanr)](https://cran.r-project.org/package=stplanr)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![](https://badges.ropensci.org/10_status.svg)](https://github.com/ropensci/onboarding/issues/10)

**stplanr** is a package for sustainable transport planning with R.

It provides functions for solving common problems in transport planning
and modelling, such as how to best get from point A to point B. The
overall aim is to provide a reproducible, transparent and accessible
toolkit to help people better understand transport systems and inform
policy.

The initial work on the project was funded by the Department of
Transport
([DfT](https://www.gov.uk/government/organisations/department-for-transport))
as part of the development of the Propensity to Cycle Tool
([PCT](http://pct.bike/)). The PCT uses origin-destination data as the
basis of spatial analysis and modelling work to identify where bicycle
paths are most needed. See the package vignette (e.g. via
`vignette("introducing-stplanr")`) or an [academic paper on the
Propensity to Cycle Tool (PCT)](http://dx.doi.org/10.5198/jtlu.2016.862)
for more information on how it can be used. This README gives some
basics.

**stplanr** should be useful to researchers everywhere. The function
`route_graphhopper()`, for example, works anywhere in the world using
the [graphhopper](https://graphhopper.com/) routing API and
`read_table_builder()` reads-in Australian data. We welcome
contributions that make transport research easier worldwide.

## Key functions

Data frames representing flows between origins and destinations must be
combined with geo-referenced zones or points to generate meaningful
analyses and visualisations of ‘flows’ or origin-destination (OD) data.
**stplanr** facilitates this with `od2line()`, which takes flow and
geographical data as inputs and outputs spatial data. Some example data
is provided in the package:

``` r
library(stplanr)
data(cents, flow)
```

Let’s take a look at this data:

``` r
flow[1:3, 1:3] # typical form of flow data
#>        Area.of.residence Area.of.workplace All
#> 920573         E02002361         E02002361 109
#> 920575         E02002361         E02002363  38
#> 920578         E02002361         E02002367  10
cents[1:3,] # points representing origins and destinations
#> class       : SpatialPointsDataFrame 
#> features    : 3 
#> extent      : -1.546463, -1.511861, 53.8041, 53.81161  (xmin, xmax, ymin, ymax)
#> coord. ref. : +init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
#> variables   : 4
#> names       :  geo_code,  MSOA11NM, percent_fem,  avslope 
#> min values  : E02002382, Leeds 053,    0.408759, 2.284782 
#> max values  : E02002393, Leeds 064,    0.458721, 2.856563
```

These datasets can be combined as follows:

``` r
travel_network <- od2line(flow = flow, zones = cents)
w <- flow$All / max(flow$All) *10
plot(travel_network, lwd = w)
```

![](vignettes/README-plot1-1.png)<!-- -->

The package can also allocate flows to the road network, e.g. with
[CycleStreets.net](https://www.cyclestreets.net/api/) and the
OpenStreetMap Routing Machine
([OSRM](https://github.com/Project-OSRM/osrm-backend)) API interfaces.
These are supported in `route_*()` functions such as
`route_cyclestreets` and `route_osrm()`:

Route functions take lat/lon inputs:

``` r
trip <-
  route_osrm(from = c(-1, 53), to = c(-1.1, 53))
```

and place names, found using the Google Map API:

We can replicate this call multiple times using
`line2route`.

``` r
intrazone <- travel_network$Area.of.residence == travel_network$Area.of.workplace
travel_network <- travel_network[!intrazone,]
t_routes <- line2route(travel_network, route_fun = route_osrm)
#> Warning in summarise_impl(.data, dots): hybrid evaluation forced for
#> `first`. Please use dplyr::first() or library(dplyr) to remove this
#> warning.

#> Warning in summarise_impl(.data, dots): hybrid evaluation forced for
#> `first`. Please use dplyr::first() or library(dplyr) to remove this
#> warning.
#> Warning in summarise_impl(.data, dots): hybrid evaluation forced for
#> `last`. Please use dplyr::last() or library(dplyr) to remove this warning.

#> Warning in summarise_impl(.data, dots): hybrid evaluation forced for
#> `last`. Please use dplyr::last() or library(dplyr) to remove this warning.
plot(t_routes)
```

![](vignettes/README-plot2-1.png)<!-- -->

Another way to visualise this is with the leaflet package:

``` r
library(leaflet)
leaflet() %>% addTiles() %>% addPolylines(data = t_routes)
```

For more examples, `example("line2route")`.

`overline` is a function which takes a series of route-allocated lines,
splits them into unique segments and aggregates the values of
overlapping lines. This can represent where there will be most traffic
on the transport system, as illustrated below.

``` r
t_routes$All <- travel_network$All
rnet <- overline(t_routes, attrib = "All", fun = sum)

lwd <- rnet$All / mean(rnet$All)
plot(rnet, lwd = lwd)
points(cents)
```

![](vignettes/README-rnet-1.png)<!-- -->

## Installation

To install the stable version, use:

``` r
install.packages("stplanr")
```

The development version can be installed using **devtools**:

``` r
# install.packages("devtools") # if not already installed
devtools::install_github("ropensci/stplanr")
library(stplanr)
```

stplanr depends on rgdal, which can be tricky to install.

### Installing stplanr on Linux and Mac

**splanr** depends on **rgdal** which can be installed on Ubuntu, for
example, with:

    sudo apt install r-cran-rgdal

To install `gdal` binaries on other distributions please see here:
<http://trac.osgeo.org/gdal/wiki/DownloadingGdalBinaries>

**stplanr** also depends on **sf**. Installation instructions for Mac,
Ubuntu and other Linux distros can be found here:
<https://github.com/r-spatial/sf#installing>

Instructions to install `gdal` and `Quartz` are provided at
<https://github.com/ropensci/geojsonio#install> and
<https://www.xquartz.org/> respectively (Quartz is required for R - as
described [here](https://cran.r-project.org/bin/macosx/)).

## Funtions, help and contributing

The current list of available functions can be seen with:

``` r
lsf.str("package:stplanr", all = TRUE)
```

To get internal help on a specific function, use the standard way.

``` r
?od2line
```

## Dependencies

**stplanr** imports many great packages that it depends on. Many thanks
to the developers of these tools:

``` r
desc = read.dcf("DESCRIPTION")
headings = dimnames(desc)[[2]]
fields = which(headings %in% c("Depends", "Imports", "Suggests"))
pkgs = paste(desc[fields], collapse = ", ")
pkgs = gsub("\n", " ", pkgs)
strsplit(pkgs, ",")[[1]]
#>  [1] "R (>= 3.0.2)"          " sp (>= 1.3.1)"       
#>  [3] " curl (>= 3.2)"        " readr (>= 1.1.1)"    
#>  [5] " dplyr (>= 0.7.6)"     " httr (>= 1.3.1)"     
#>  [7] " jsonlite (>= 1.5)"    " stringi (>= 1.2.4)"  
#>  [9] " stringr (>= 1.3.1)"   " lubridate (>= 1.7.4)"
#> [11] " maptools (>= 0.9.3)"  " raster (>= 2.6.7)"   
#> [13] " rgdal (>= 1.3.4)"     " rgeos (>= 0.3.28)"   
#> [15] " openxlsx (>= 4.1.0)"  " methods"             
#> [17] " R.utils (>= 2.7.0)"   " geosphere (>= 1.5.7)"
#> [19] " Rcpp (>= 0.12.1)"     " igraph (>= 1.2.2)"   
#> [21] " nabor (>= 0.5.0)"     " rlang (>= 0.2.2)"    
#> [23] " lwgeom (>= 0.1.4)"    " sf (>= 0.6.3)"       
#> [25] " testthat (>= 2.0.0)"  " knitr (>= 1.20)"     
#> [27] " rmarkdown (>= 1.10)"  " dodgr (>= 0.0.3)"
```

## Meta

  - Please report issues, feature requests and questions to the [github
    issue tracker](https://github.com/ropensci/stplanr/issues)
  - License: MIT
  - Get citation information for `stplanr` in R doing `citation(package
    = 'stplanr')`
  - This project is released with a [Contributor Code of
    Conduct](CONDUCT.md). By participating in this project you agree to
    abide by its
terms.

<!-- [![rofooter](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org) -->
