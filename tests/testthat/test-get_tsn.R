context("get_tsn")

test_that("get_tsn returns the correct value", {
  skip_on_cran()
  vcr::use_cassette("get_tsn", {
    x <- get_tsn("asdfasdf", messages=FALSE)[[1]]
    z <- get_tsn("Chironomus riparius", messages=FALSE)
  })

	expect_true(is.na(x))
	expect_is(z, "tsn")
})

test_that("get_tsn accepts ask and verbose arguments", {
  skip_on_cran()
  vcr::use_cassette("get_tsn_ask_verbose_args", {
    # expect_message(sw(get_tsn('Dugesia', messages=TRUE, ask = FALSE)))
    expect_true(all(is.na(sw(get_tsn('black bear', searchtype="common",
      ask=FALSE, messages=FALSE)))))
  })
})

test_that("get_tsn fails as expected", {
  skip_on_cran()

  expect_error(get_tsn(), "argument \"sci_com\" is missing")
  expect_error(get_tsn("Arni", ask = 4, messages = FALSE),
               "ask must be of class logical")
  expect_error(
    get_tsn(sci_com="black bear", searchtype=5,
            messages = FALSE),
    "searchtype must be of class character")
  expect_error(
    get_tsn("Arni", accepted = 34,
            messages = FALSE),
    "accepted must be of class logical")

  # searchtype values
  expect_error(
    get_tsn(sci_com="black bear", searchtype="asdfadf",
            messages = FALSE),
    "'arg' should be one of")

  # rows param
  expect_error(get_tsn("Achlya", rows = "foobar", messages = FALSE),
               "rows must be of class numeric, integer")
  expect_error(get_tsn("Achlya", rows = 0, messages = FALSE),
               "all\\(rows > 0\\) is not TRUE")
})
