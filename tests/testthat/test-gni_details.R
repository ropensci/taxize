context("gni_details")

test_that("gni_details returns the correct value", {
  skip_on_cran()
  vcr::use_cassette("gni_details", {
    x <- gni_details(id = 17802847)
  })

  expect_is(x, "data.frame")
  expect_match(as.character(x[,3]), "none")
})
