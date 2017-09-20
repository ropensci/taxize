# tests for get_tsn fxn in taxize
context("get_tsn")

test_that("get_tsn returns the correct value", {
  skip_on_cran()

	expect_that(is.na(get_tsn("asdfasdf", verbose=FALSE)[[1]]), is_true())
})

test_that("get_tsn returns the correct class", {
  skip_on_cran()

	expect_that(get_tsn(c("Chironomus riparius", "Chaetopteryx"),
	                    verbose=FALSE), is_a("tsn"))
})

test_that("get_tsn accepts ask and verbose arguments", {
  skip_on_cran()

  expect_message(get_tsn('Dugesia', verbose=TRUE))
  expect_message(get_tsn('Dugesia', verbose=FALSE), NA)

  expect_that(all(is.na(get_tsn('black bear', searchtype="common",
                                ask=FALSE, verbose=FALSE))), is_true())
})

test_that("get_tsn fails as expected", {
  skip_on_cran()

  expect_error(get_tsn(), "argument \"searchterm\" is missing")
  expect_error(get_tsn("Arni", ask = 4, verbose = FALSE),
               "ask must be of class logical")
  expect_error(
    get_tsn(searchterm="black bear", searchtype=5,
            verbose = FALSE),
    "searchtype must be of class character")
  expect_error(
    get_tsn("Arni", accepted = 34,
            verbose = FALSE),
    "accepted must be of class logical")

  # searchtype values
  expect_error(
    get_tsn(searchterm="black bear", searchtype="asdfadf",
            verbose = FALSE),
    "'arg' should be one of")

  # rows param
  expect_error(get_tsn("Achlya", rows = "foobar", verbose = FALSE),
               "'rows' must be numeric or NA")
  expect_error(get_tsn("Achlya", rows = 0, verbose = FALSE),
               "'rows' value must be an integer 1 or greater")
})
