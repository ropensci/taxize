# tests for itis_downstream fxn in taxize
context("itis_downstream")


test_that("itis_downstream returns the correct value", {
  skip_on_cran()

  data(rank_ref, package = "taxize")
  dat_ <- itis_downstream(tsns=183264, "Species", verbose=FALSE)

	expect_that(as.character(dat_[1,"rankname"]), matches("species"))

	expect_that(dat_, is_a("data.frame"))

  expect_that(dim(dat_)[2], equals(6))
})
