# tests for itis_getrecord fxn in taxize
context("itis_getrecord")

one <- itis_getrecord(202385)
two <- itis_getrecord(c(202385,70340))
three <- itis_getrecord("urn:lsid:itis.gov:itis_tsn:180543", "lsid")

test_that("itis_getrecord returns the correct class", {
  expect_that(one, is_a("list"))
  expect_that(two, is_a("list"))
  expect_that(three, is_a("list"))
})

test_that("itis_getrecord correctly suppresses a message", {
  expect_message(itis_getrecord(202385, verbose=TRUE))
  expect_that(itis_getrecord(202385, verbose=FALSE), not(shows_message()))
})