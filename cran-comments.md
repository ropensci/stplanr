Update in preparation for big changes in v1.0.0 that will remove support for soon-to-be retired rgeos/rgdal/maptools packages.

The message on load will give people a chance to provide feedback.

Also contains fixes based on emails from CRAN and best practice checks.

Results on WinBuilder: https://win-builder.r-project.org/lsiV27w76DU3/00check.log

## R CMD check results
checking R code for possible problems ... NOTE
  File ‘stplanr/R/zzz.R’:
    .onLoad calls:
      packageStartupMessage(paste0("Loading stplanr v0.9.0.\n", "Note: the next planned version, v1.0.0, will not support sp objects.\n",     "See the issue #332 and https://github.com/ropensci/stplanr/pull/481.",     "Any feedback on GitHub: welcome. Thanks (Robin Lovelace, April 2022)!"))
  
  See section ‘Good practice’ in '?.onAttach'.

0 errors ✔ | 0 warnings ✔ | 2 notes ✖

R CMD check succeeded

