context("Test overline function")

test_that(
  desc = "overline generates a SpatialLinesDataFrame",
  code = {
    data("routes_fast", package = "stplanr")
    rnet <- overline(sldf = routes_fast, attrib = "length")
    expect_is(object = rnet, class = "SpatialLinesDataFrame")
  })