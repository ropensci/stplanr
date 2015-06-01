context("Test the gFlow2Line function")

test_that(
  desc = "gFlow2Line generates SpatialLinesDataFrame output",
  code = {
    data("flow", package = "stplanr")
    data("cents", package = "stplanr")
    l <- gFlow2line(flow = flow, zones = cents)
    data("flowlines")
    expect_identical(object = l, expected = flowlines)
  })