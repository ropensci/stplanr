context("Test the od2line function")

test_that(
  desc = "od2line generates SpatialLinesDataFrame output",
  code = {
    l <- od2line(flow = flow, zones = cents_sf)
    expect_true(is(l, "sf"))
  }
)
