# tests for gni_details fxn in taxize
context("gni_details")

test_that("gni_details returns the correct value", {
  skip_on_cran()

  expect_match(as.character(gni_details(id = 17802847)[,3]), "none")
})

test_that("gni_details returns the correct class", {
  skip_on_cran()

	expect_that(gni_details(id = 17802847), is_a("data.frame"))
})
