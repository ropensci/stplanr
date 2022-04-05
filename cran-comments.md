Update in preparation for big changes in v1.0.0 that will remove support for soon-to-be retired rgeos/rgdal/maptools packages.

The message on load will give people a chance to provide feedback.

## R CMD check results

0 errors | 0 warnings | 2 notes

* checking R code for possible problems ... [29s] NOTE
File 'stplanr/R/zzz.R':
  .onLoad calls:
    packageStartupMessage(paste0("Loading stplanr v0.9.0.\n", "Note: the next planned version, v1.0.0, will not support sp objects.\n",     "See the issue #332 and https://github.com/ropensci/stplanr/pull/481.",     "Any feedback on GitHub: welcome. Thanks (Robin Lovelace, April 2022)!"))

See section 'Good practice' in '?.onAttach'.

Found if() conditions comparing class() to string:
File 'stplanr/R/oneway.R': if (class(x) == "factor") ...
File 'stplanr/R/oneway.R': if (class(y) == "factor") ...
File 'stplanr/R/oneway.R': if (class(x) == "factor") ...
File 'stplanr/R/oneway.R': if (class(y) == "factor") ...
Use inherits() (or maybe is()) instead.
* checking Rd files ... [1s] OK

I don't think these URLs are incorrect:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Robin Lovelace <rob00x@gmail.com>'

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1177/2399808320942779
    From: man/overline.Rd
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.3141/2308-01
    From: inst/doc/stplanr-paper.html
    Status: 503
    Message: Service Unavailable

Found the following (possibly) invalid DOIs:
  DOI: 10.1177/2399808320942779
    From: DESCRIPTION
    Status: Service Unavailable
    Message: 503
