# tests for ubio_classification

context("ubio_classification")

tt <- suppressMessages(ubio_classification(hierarchiesID = 2483153))


test_that("ncbi_get_taxon_summary returns correct class and result", {
  expect_is(tt, "list")
  expect_equal(length(tt), 5)
  expect_is(tt$data, "data.frame")
  expect_equal(ncol(tt$data), 8)
})
