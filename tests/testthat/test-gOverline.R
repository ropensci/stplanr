context("Test gOverline function")

test_that(
  desc = "gOverline generates a SpatialLinesDataFrame",
  code = {
    data("routes_fast", package = "stplanr")
    rnet <- gOverline(sldf = routes_fast, attrib = "length")
    expect_is(object = rnet, class = "SpatialLinesDataFrame")
  })