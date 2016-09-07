context("Test route_cyclestreet function")

test_that(
  desc = "route_graphhopper generates a SpatialLinesDataFrame output",
  code = {
    if(!Sys.getenv("GRAPHHOPPER") == ""){ # only run test if user has set an api key
      rf <- route_graphhopper("Leeds", "Bradford", vehicle = "bike")
      expect_is(rf, "SpatialLinesDataFrame")
    }
  })
