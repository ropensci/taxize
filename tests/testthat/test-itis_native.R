# tests for itis_native fxn in taxize
context("itis_native")

one <- itis_native(what="values")
two <- itis_native(what="originvalues")
three <- itis_native(tsn=180543)
four <- itis_native(tsn=c(180543,41074,36616))

test_that("itis_native returns the correct class", {
  expect_that(one, is_a("data.frame"))
  expect_that(two, is_a("data.frame"))
  expect_that(three, is_a("data.frame"))
  expect_that(four, is_a("list"))
  expect_that(four[[1]], is_a("data.frame"))
})

test_that("itis_native correctly suppresses a message", {
  expect_message(itis_native(180543, verbose=TRUE))
  expect_that(itis_native(180543, verbose=FALSE), not(shows_message()))
})