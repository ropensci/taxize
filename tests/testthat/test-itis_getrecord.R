# tests for itis_getrecord fxn in taxize
context("itis_getrecord")

test_that("itis_getrecord returns the correct class", {
  skip_on_cran()

  one <- itis_getrecord(202385, verbose=FALSE)
  two <- itis_getrecord(c(202385,70340), verbose=FALSE)
  three <- itis_getrecord("urn:lsid:itis.gov:itis_tsn:202385", "lsid",
                          verbose=FALSE)

  expect_that(one, is_a("list"))
  expect_that(two, is_a("list"))
  expect_that(three, is_a("list"))
})
