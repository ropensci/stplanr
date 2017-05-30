context("Test route_cyclestreet function")

test_that(
  desc = "route_cyclestreet generates a SpatialLinesDataFrame output",
  code = {
    if(!Sys.getenv("CYCLESTREET") == ""){ # only run test if user has set an api key
      rf <- route_cyclestreet("leeds, UK", "bradford, UK", "fastest")
      expect_is(rf, "SpatialLinesDataFrame")
    }
  })
