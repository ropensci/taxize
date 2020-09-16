context("get_itis")

test_that("get_itis returns the correct value", {
  skip_on_cran()
  vcr::use_cassette("get_itis", {
    x <- get_itis("asdfasdf", messages=FALSE)
    z <- get_itis("Chironomus riparius", messages=FALSE)
  })

	expect_true(is.na(x))
	expect_is(z, "itis")
})

test_that("get_itis accepts ask and verbose arguments", {
  skip_on_cran()
  vcr::use_cassette("get_itis_ask_verbose_args", {
    expect_message(sw(get_itis('Dugesia', messages=TRUE, ask = FALSE)))
    expect_true(all(is.na(sw(get_itis('black bear', searchtype="common",
      ask=FALSE, messages=FALSE)))))
  })
})

test_that("get_itis fails as expected", {
  skip_on_cran()

  expect_error(get_itis(), "argument \"sci_com\" is missing")
  expect_error(get_itis("Arni", ask = 4, messages = FALSE),
               "ask must be of class logical")
  expect_error(
    get_itis(sci_com="black bear", searchtype=5,
            messages = FALSE),
    "searchtype must be of class character")
  expect_error(
    get_itis("Arni", accepted = 34,
            messages = FALSE),
    "accepted must be of class logical")

  # searchtype values
  expect_error(
    get_itis(sci_com="black bear", searchtype="asdfadf",
            messages = FALSE),
    "'arg' should be one of")

  # rows param
  expect_error(get_itis("Achlya", rows = "foobar", messages = FALSE),
               "rows must be of class numeric, integer")
  expect_error(get_itis("Achlya", rows = 0, messages = FALSE),
               "all\\(rows > 0\\) is not TRUE")
})
