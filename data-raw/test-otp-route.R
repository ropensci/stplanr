# Aim: test route() with otp package (requires external routing engine)
library(tidyverse)
library(opentripplanner)
library(sf)
library(stplanr)
ip = "otp.saferactive.org" # an actual server
otpcon = otp_connect(hostname = ip,
                     port = 80,
                     router = "west-yorkshire")
u = "https://github.com/ITSLeeds/TDS/releases/download/22/NTEM_flow.geojson"
desire_lines = read_sf(u)
head(desire_lines)
desire_top = desire_lines %>%
  top_n(n = 3, wt = all)
plot(desire_top)
routes_top_car = route(
  l = desire_top,
  route_fun = otp_plan,
  mode = "CAR",
  otpcon = otpcon
)
