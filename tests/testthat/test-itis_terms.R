# tests for itis_terms fxn in taxize
context("itis_terms")


test_that("itis_terms returns the correct class", {
  skip_on_cran()

  one <- itis_terms(query='bear', verbose=FALSE)
  two <- itis_terms(query='tarweed', "common", verbose=FALSE)
  three <- itis_terms(query='Poa annua', "scientific", verbose=FALSE)

  expect_that(one, is_a("data.frame"))
  expect_that(two, is_a("data.frame"))
  expect_that(three, is_a("data.frame"))
})
