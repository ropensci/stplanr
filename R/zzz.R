.onLoad <- function(libname, pkgname){
  packageStartupMessage(
    paste0(
      "Loading stplanr v1.0.0.\n",
      "Note: this version removes support for sp objects.\n",
      "If you require support for sp, use an older version or shift to sf.\n",
      "See r-spatial.org/r/2022/04/12/evolution.html for motivation."
    )
  )
}
