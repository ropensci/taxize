# tests for get_tpsid fxn in taxize
context("get_tpsid")

test_that("get_tpsid returns the correct value", {
  expect_equal(get_tpsid(sciname='Helianthus excubitor')[[1]], 50230899)
  expect_that(is.na(get_tpsid(sciname='adsf asdf asdf', verbose=FALSE)[[1]]), 
              is_true())
})

test_that("get_tpsid returns the correct class", {
  expect_that(get_tpsid(c("Helianthus excubitor", "adsf asdf asdf"), verbose=FALSE), 
              is_a("tpsid"))
})

test_that("get_tpsid accepts ask-argument", {
  expect_that(is.na(get_tpsid(sciname='adsf asdf asdf', ask=FALSE, verbose=FALSE)[[1]]), 
              is_true())
})
