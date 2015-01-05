# tests for itis_downstream fxn in taxize
context("itis_downstream")

data(rank_ref, package = "taxize")
dat_ <- itis_downstream(tsns=183264, "Species", verbose=FALSE)
dat2 <- itis_downstream(tsns=846509, downto="Genus", verbose=FALSE)

test_that("itis_downstream returns the correct value", {
	expect_that(as.character(dat_[1,"rankname"]), matches("Species"))
	expect_that(as.character(dat2[1,"rankname"]), matches("Genus"))
})

test_that("itis_downstream returns the correct class", {
	expect_that(dat_, is_a("data.frame"))
	expect_that(dat2, is_a("data.frame"))
})

test_that("itis_downstream returns the correct dimensions", {
  expect_that(dim(dat_)[2], equals(6))
  expect_that(dim(dat2)[2], equals(6))
})
