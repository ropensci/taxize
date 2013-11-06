# tests for get_uid fxn in taxize
context("get_uid")

test_that("get_uid returns the correct value", {
	expect_that(is.na(get_uid(c("Chironomus riparius", "aaa"), verbose=FALSE)[2]), 
              is_true())
})

test_that("get_uid returns the correct class", {
	expect_that(get_uid(c("Chironomus riparius", "Chaetopteryx"), verbose=FALSE), 
              is_a("uid"))
})

test_that("get_uid accepts ask-argument", {
  expect_that(is.na(get_uid('Dugesia', ask = FALSE, verbose=FALSE)), 
              is_true())
})
