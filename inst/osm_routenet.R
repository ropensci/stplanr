file.rename("R/osm_routenet.R", "inst/osm_routenet.R")

#' Identify transport network features from Open Street Map
#'
#'

#' @examples
data("cents")
x <- cents[5,]

get_osmtn <- function(x, type, radius){

}


#' From http://gis.stackexchange.com/questions/121489
#'
aeqd_buff <- function(p, r){
  stopifnot(length(p) == 1)
  aeqd <- sprintf("+proj=aeqd +lat_0=%s +lon_0=%s +x_0=0 +y_0=0",
    p@coords[[2]], p@coords[[1]])
  projected <- spTransform(p, CRS(aeqd))
  buffered <- gBuffer(projected, width=r, byid=TRUE)
  spTransform(buffered, p@proj4string)
}