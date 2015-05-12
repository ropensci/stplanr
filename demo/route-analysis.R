data("cents", package = "stplanr")
plot(cents)
data("flowlines")
lines(flowlines)
data("routes_fast")
lines(routes_fast, col = "red")
library(rgdal) # needed for spTransform
lgb <- sp::spTransform(flowlines, CRSobj = CRS("+init=epsg:27700"))
length1 <- rgeos::gLength(lgb, byid = T)
length2 <- routes_fast@data$length
plot(length1, length2, xlab = "Euclidean distance", ylab = "Route distance")
abline(a = 0, b = 1)