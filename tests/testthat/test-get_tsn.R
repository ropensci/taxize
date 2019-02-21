context("get_tsn")

test_that("get_tsn returns the correct value", {
  vcr::use_cassette("get_tsn", {
    x <- get_tsn("asdfasdf", verbose=FALSE)[[1]]
    z <- get_tsn("Chironomus riparius", verbose=FALSE)
  })

	expect_true(is.na(x))
	expect_is(z, "tsn")
})

test_that("get_tsn accepts ask and verbose arguments", {
  vcr::use_cassette("get_tsn_ask_verbose_args", {
    expect_message(sw(get_tsn('Dugesia', messages=TRUE, ask = FALSE)))
    expect_true(all(is.na(sw(get_tsn('black bear', searchtype="common",
      ask=FALSE, verbose=FALSE)))))
  })
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
               "rows must be of class numeric, integer")
  expect_error(get_tsn("Achlya", rows = 0, verbose = FALSE),
               "rows > 0 is not TRUE")
})
