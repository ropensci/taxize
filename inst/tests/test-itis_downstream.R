# tests for itis_downstream fxn in taxize
context("itis_downstream")

data(rank_ref)
dat_ <- itis_downstream(tsns = 183264, "Species")

test_that("itis_downstream returns the correct value", {
	expect_that(dat_[1,2], matches("Ginkgo"))
})

test_that("itis_downstream returns the correct class", {
	expect_that(dat_, is_a("data.frame"))
})
