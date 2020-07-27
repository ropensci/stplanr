context("Test the od2line function")

test_that(
  desc = "od2line generates SpatialLinesDataFrame output",
  code = {
    data("flow", package = "stplanr")
    data("cents", package = "stplanr")
    l <- od2line(flow = flow, zones = cents)
    data("flowlines")
    expect_true(class(l) == "SpatialLinesDataFrame")
  }
)
