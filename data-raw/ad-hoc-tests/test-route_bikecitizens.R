remotes::install_github("ropensci/stplanr", "route_bikecitizens")
remotes::install_github("itsleeds/pct")
library(tidyverse)
l = pct::get_pct_lines(region = "west-yorkshire")
l_leeds = l %>%
  filter(lad_name1 == "Leeds") %>%
  filter(lad_name2 == "Leeds")
l_leeds_top_100 = l_leeds %>%
  top_n(n = 100, wt = all)
plot(l_leeds_top_100)
system.time({
  r = stplanr::route(l = l_leeds_top_100, route_fun = stplanr::route_bikecitizens)
})
plot(r)
system.time({
  rcs = stplanr::route(l = l_leeds_top_100, route_fun = cyclestreets::journey)
})
# pretty much the same speed, ideal for teaching!
