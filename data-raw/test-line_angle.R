# Aim: test outputs of line_angle

remotes::install_github("wangzhao0217/stplanr@add_angle_as_filter")
# ?angle_diff
l1 = stplanr::routes_fast_sf[2:3, ]
plot(l1$geometry)

line_bearings = line_bearing(l1)
v1 = stplanr::get_vector(l1$geometry[1])

angle_diff(l1, 90)
