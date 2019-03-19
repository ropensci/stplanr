
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
policy, as outlined in a
[paper](https://journal.r-project.org/archive/2018/RJ-2018-053/index.html)
about the package, and the potential for open source software in
transport planning in general, published in the [R
Journal](http://journal.r-project.org/).

The initial work on the project was funded by the Department of
Transport
([DfT](https://www.gov.uk/government/organisations/department-for-transport))
as part of the development of the Propensity to Cycle Tool (PCT), a web
application to explore current travel patterns and cycling potential at
zone, desire line, route and route network levels (see
[www.pct.bike](http://www.pct.bike/) and click on a region to try it
out). The basis of the methods underlying the PCT is origin-destination
data, which are used to highlight where many short distance trips are
being made, and estimate how many could switch to cycling. The results
help identify where cycleways are most needed, an important component of
sustainable transport planning infrastructure engineering and policy
[design](https://www.icevirtuallibrary.com/doi/abs/10.1680/dfct.63495.001).
See the package vignette (e.g. via `vignette("introducing-stplanr")`) or
an [academic paper on the Propensity to Cycle Tool
(PCT)](http://dx.doi.org/10.5198/jtlu.2016.862) for more information on
how it can be used. This README provides some basics.

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
```

Let’s take a look at this data:

``` r
flow[1:3, 1:3] # typical form of flow data
#>        Area.of.residence Area.of.workplace All
#> 920573         E02002361         E02002361 109
#> 920575         E02002361         E02002363  38
#> 920578         E02002361         E02002367  10
cents_sf[1:3,] # points representing origins and destinations
#> Simple feature collection with 3 features and 4 fields
#> geometry type:  POINT
#> dimension:      XY
#> bbox:           xmin: -1.546463 ymin: 53.8041 xmax: -1.511861 ymax: 53.81161
#> epsg (SRID):    4326
#> proj4string:    +proj=longlat +datum=WGS84 +no_defs
#>       geo_code  MSOA11NM percent_fem  avslope                   geometry
#> 1708 E02002384 Leeds 055    0.458721 2.856563 POINT (-1.546463 53.80952)
#> 1712 E02002382 Leeds 053    0.438144 2.284782 POINT (-1.511861 53.81161)
#> 1805 E02002393 Leeds 064    0.408759 2.361707  POINT (-1.524205 53.8041)
```

These datasets can be combined as follows:

``` r
travel_network <- od2line(flow = flow, zones = cents_sf)
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

Route functions take lat/lon inputs (results not calculated):

``` r
trip <- route_osrm(from = c(-1, 53), to = c(-1.1, 53))
```

and place names, found using the Google Map API:

We can replicate this call multiple times using `line2route`, in this
case lines 2 to 5. First we’ll create a small subset of the lines:

``` r
desire_lines <- travel_network[2:5,]
```

Next, we’ll calculate the routes (not not evaluated):

``` r
routes <- line2route(desire_lines, route_fun = route_osrm)
```

The resulting routes will look something like this:

``` r
routes = routes_fast_sf[2:5, ]
plot(routes$geometry)
```

![](vignettes/README-unnamed-chunk-7-1.png)<!-- -->

Another way to visualise this is with the leaflet package:

``` r
library(leaflet)
leaflet() %>% addTiles() %>% addPolylines(data = routes)
```

![](vignettes/README-unnamed-chunk-8-1.png)<!-- -->

For more examples, `example("line2route")`.

`overline` is a function which takes a series of route-allocated lines,
splits them into unique segments and aggregates the values of
overlapping lines. This can represent where there will be most traffic
on the transport system, as illustrated below.

``` r
routes$All <- desire_lines$All
rnet <- overline2(routes, attrib = "All")
#> Loading required namespace: pbapply
#> 2019-03-19 13:15:54 constructing segments
#> 2019-03-19 13:15:54 transposing 'B to A' to 'A to B'
#> 2019-03-19 13:15:54 removing duplicates
#> 2019-03-19 13:15:54 restructuring attributes
#> 2019-03-19 13:15:55 building geometry
#> 2019-03-19 13:15:55 simplifying geometry
#> 2019-03-19 13:15:55 rejoining segments into linestrings

lwd <- rnet$All / mean(rnet$All)
plot(rnet, lwd = lwd, reset = FALSE)
plot(cents_sf, add = TRUE)
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
#> [25] " magrittr"             " testthat (>= 2.0.0)" 
#> [27] " knitr (>= 1.20)"      " rmarkdown (>= 1.10)" 
#> [29] " dodgr (>= 0.0.3)"     " stats19"             
#> [31] " cyclestreets"         " pbapply"
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

[![rofooter](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
