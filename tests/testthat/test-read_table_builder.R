context("Test the read_table_builder function")

test_that(
  desc = "read_table_builder returns a data.frame",
  code = {
    data_dir <- system.file("extdata", package = "stplanr")
    t1 <- read_table_builder(file.path(data_dir, "SA1Population.csv"))
    expect_is(t1, "data.frame")
  }
)
