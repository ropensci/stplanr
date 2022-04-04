.onLoad <- function(libname, pkgname){
  packageStartupMessage(
    paste0(
      "Loading stplanr v1.0.0.\n",
      "Note: this version removes support for sp objects.\n",
      "See the issue #332 and https://github.com/ropensci/stplanr/pull/481.",
      "Any feedback on GitHub: welcome. Thanks (Robin Lovelace, April 2022)!"
    )
  )
}
