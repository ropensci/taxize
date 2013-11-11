# tests for get_tsn fxn in taxize
context("get_tsn")

test_that("get_tsn returns the correct value", {
	expect_that(is.na(get_tsn("asdfasdf", verbose=FALSE)[[1]]), is_true())
})

test_that("get_tsn returns the correct class", {
	expect_that(get_tsn(c("Chironomus riparius", "Chaetopteryx"), verbose=FALSE), is_a("tsn"))
})

test_that("get_tsn accepts ask and verbose arguments", {
  expect_message(get_tsn('Dugesia', verbose=TRUE))
  expect_that(get_tsn('Dugesia', verbose=FALSE), not(shows_message()))
  
  expect_that(is.na(get_tsn('bear', searchtype="comname", ask=FALSE, verbose=FALSE)), is_true())
})
