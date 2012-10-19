# tests for itis_downstream fxn in taxize
context("itis_downstream")

dat_ <- itis_downstream(tsns = 180541, "Species")

test_that("itis_downstream returns the correct value", {
	expect_that(dat_[1,2], matches("Ursus"))
})

test_that("itis_downstream returns the correct class", {
	expect_that(dat_, is_a("data.frame"))
})
