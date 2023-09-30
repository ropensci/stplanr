test_that("mats2line() constructs linestrings correctly", {
  m1 <- matrix(c(1, 2, 1, 2), ncol = 2)
  m2 <- matrix(c(9, 9, 9, 1), ncol = 2)
  l <- mats2line(m1, m2)

  # we expect an sfc_LINESTRING
  expect_s3_class(l, c("sfc", "sfc_LINESTRING"))

  # default CRS should be NA
  expect_equal(sf::st_crs(l), sf::st_crs(NA))

  # construct the same sfc_LINESTRING by hand
  l2 <- sf::st_sfc(
    sf::st_linestring(matrix(c(1, 9, 1, 9), ncol = 2)),
    sf::st_linestring(matrix(c(2, 9, 2, 1), ncol = 2))
  )

  # test that they are identical
  expect_identical(l, l2)
})
