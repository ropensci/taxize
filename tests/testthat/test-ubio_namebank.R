# tests for ubio_namebank fxn in taxize
context("ubio_namebank")

out <- ubio_search(searchName = 'elephant', sci = 1, vern = 0)

test_that("ubio_namebank returns the correct value", {
	expect_that(out$scientific[ , 1], matches("6938660"))
})

test_that("ubio_namebank returns the correct class", {
  expect_that(out, is_a("list"))
	expect_that(out$scientific, is_a("data.frame"))
	expect_that(ncol(out$scientific), equals(8))
})
