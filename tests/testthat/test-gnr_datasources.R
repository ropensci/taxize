context("gnr_datasources")

test_that("gnr_datasources returns the correct class", {
  skip_on_cran()
  vcr::use_cassette("gnr_datasources", {
    tmp <- gnr_datasources()
  })
  expect_is(tmp, "data.frame")
  expect_equal(NCOL(tmp), 12)
  expect_is(tmp$title, "character")
  expect_is(tmp$updated_at, "character")
  expect_type(tmp$id, "integer")
})

test_that("gnr_datasources fails well", {
  skip_on_cran()
  
  expect_error(gnr_datasources(todf = 5),
    "todf is defunct")
})
