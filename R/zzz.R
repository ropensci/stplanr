.onLoad <- function(libname, pkgname){
  packageStartupMessage(
    paste0(
      "Loading stplanr v0.9.0.\n",
      "Note: the next planned version, v1.0.0, will not support sp objects.\n",
      "See the issue #332 and https://github.com/ropensci/stplanr/pull/481.",
      "Any feedback on GitHub: welcome. Thanks (Robin Lovelace, April 2022)!"
    )
  )
}
