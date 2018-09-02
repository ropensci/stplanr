context("Test route_cyclestreet function")

test_that(
  desc = "route_cyclestreet generates a SpatialLinesDataFrame output",
  code = {
    if (!Sys.getenv("CYCLESTREET") == "") { # only run test if user has set an api key
      route_f <- route_cyclestreet(c(-1.55, 53.80), c(-1.76, 53.80))
      expect_true(grepl(pattern = "SpatialLinesDataFrame|sf", class(route_f)))
    }
  }
)
