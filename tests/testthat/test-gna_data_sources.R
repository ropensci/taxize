context("gna_data_sources")

test_that("gna_data_sources returns a table", {
  skip_on_cran()
  vcr::use_cassette("gna_data_sources", {
    tmp <- gna_data_sources()
  })
  expect_is(tmp, "data.frame")
  expect_equal(NCOL(tmp), 13)
})

