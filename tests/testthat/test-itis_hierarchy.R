# tests for itis_hierarchy fxn in taxize
context("itis_hierarchy")

one <- itis_hierarchy(tsn=180543, verbose=FALSE)
two <- itis_hierarchy(tsn=180543, "up", verbose=FALSE)
three <- itis_hierarchy(tsn=180543, "down", verbose=FALSE)
four <-itis_hierarchy(tsn=c(180543,41074,36616), verbose=FALSE)

test_that("itis_hierarchy returns the correct class", {
  expect_that(one, is_a("data.frame"))
  expect_that(two, is_a("data.frame"))
  expect_that(three, is_a("data.frame"))
  expect_that(four, is_a("list"))
  expect_that(four[[1]], is_a("data.frame"))
})
