context("get_natserv")

test_that("get_natserv returns the correct value", {
  skip_on_cran()

  vcr::use_cassette("get_natserv", {
    x <- get_natserv(c('Pomatomus saltatrix', "howdy"), messages=FALSE)
    w <- get_natserv(c("Helianthus annuus", 'Pomatomus saltatrix'),
        messages=FALSE)
  })
    
  expect_true(is.na(x[2]))
  expect_is(w, "natserv")
})

test_that("get_natserv accepts ask-argument", {
  skip_on_cran()
  
  vcr::use_cassette("get_natserv_ask_arg", {
    x <- get_natserv('howdy', ask = FALSE, messages=FALSE)
  })

  expect_true(is.na(x))
})

test_that("get_natserv fails well", {
  skip_on_cran()

  expect_true(is.na(get_natserv("asdfadsf", messages = FALSE)))

  expect_error(get_natserv(), "argument \"sci_com\" is missing")
  expect_error(get_natserv("clam", 5),
               "searchtype must be of class character")
  expect_error(get_natserv("clam", "stuff", messages = FALSE),
               "'searchtype' must be one of")
  expect_error(get_natserv("clam", ask = 4),
               "ask must be of class logical")

  # rows param
  expect_error(get_natserv('Ruby*', 'common', rows = "foobar", messages = FALSE),
               "rows must be of class numeric, integer")
  expect_error(get_natserv('Ruby*', 'common', rows = 0, messages = FALSE),
               "all\\(rows > 0\\) is not TRUE")
})
