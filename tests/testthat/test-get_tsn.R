# tests for get_tsn fxn in taxize
context("get_tsn")

test_that("get_tsn returns the correct value", {
  skip_on_cran()

	expect_that(is.na(get_tsn("asdfasdf", verbose=FALSE)[[1]]), is_true())
})

test_that("get_tsn returns the correct class", {
  skip_on_cran()

	expect_that(get_tsn(c("Chironomus riparius", "Chaetopteryx"), verbose=FALSE), is_a("tsn"))
})

test_that("get_tsn accepts ask and verbose arguments", {
  skip_on_cran()

  expect_message(get_tsn('Dugesia', verbose=TRUE))
  expect_message(get_tsn('Dugesia', verbose=FALSE), NA)

  expect_that(all(is.na(get_tsn('black bear', searchtype="common", ask=FALSE, verbose=FALSE))), is_true())
})
