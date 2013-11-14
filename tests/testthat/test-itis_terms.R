# tests for itis_terms fxn in taxize
context("itis_terms")

one <- itis_terms(query='bear')
two <- itis_terms(query='tarweed', "common")
three <- itis_terms(query='Poa annua', "scientific")

test_that("itis_terms returns the correct class", {
  expect_that(one, is_a("data.frame"))
  expect_that(two, is_a("data.frame"))
  expect_that(three, is_a("data.frame"))
})