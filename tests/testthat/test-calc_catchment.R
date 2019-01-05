context("Test the calc_catchment function")

test_that(
  desc = "calc_catchment returns a SpatialPolygonsDataFrame",
  code = {
    data_dir <- system.file("extdata", package = "stplanr")
    unzip(file.path(data_dir, "smallsa1.zip"))
    unzip(file.path(data_dir, "testcycleway.zip"))
    sa1income <- readOGR(".", "smallsa1")
    testcycleway <- readOGR(".", "testcycleway")
    t1 <- calc_catchment(
      polygonlayer = sa1income,
      targetlayer = testcycleway,
      calccols = c("Total"),
      distance = 800,
      projection = "austalbers",
      dissolve = TRUE
    )
    expect_is(t1, "SpatialPolygonsDataFrame")
    files_to_remove <- list.files(pattern = "smallsa|testcycleway")
    file.remove(files_to_remove) # tidy up
  }
)
