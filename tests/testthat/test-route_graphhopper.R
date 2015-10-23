context("Test route_cyclestreet function")

test_that(
  desc = "route_cyclestreet generates a SpatialLinesDataFrame output",
  code = {
    if(!Sys.getenv("GRAPHHOPPER") == ""){ # only run test if user has set an api key
      p1 <- c(-2, 52)
      p2 <- p1 + 1
      rf <- route_graphhopper(p1, p2, vehicle = "bike")
      expect_is(rf, "SpatialLinesDataFrame")
    }
  })