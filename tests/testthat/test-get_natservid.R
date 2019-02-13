context("get_natservid")

test_that("get_natservid returns the correct value", {
  vcr::use_cassette("get_natservid", {
    x <- get_natservid(c('Gadus morhua', "howdy"), verbose=FALSE)[2]
    w <- get_natservid(c("Helianthus annuus", 'Gadus morhua'), 
        verbose=FALSE)
  })
    
  expect_true(is.na(x))
  expect_is(w, "natservid")
})

test_that("get_natservid accepts ask-argument", {
  vcr::use_cassette("get_natservid_ask_arg", {
    x <- get_natservid('asdasf', ask = FALSE, verbose=FALSE)
  })

  expect_true(is.na(x))
})

test_that("get_natservid fails well", {
  skip_on_cran()

  expect_true(is.na(get_natservid("asdfadsf", verbose = FALSE)))

  expect_error(get_natservid(), "argument \"query\" is missing")
  expect_error(get_natservid("clam", 5),
               "searchtype must be of class character")
  expect_error(get_natservid("clam", "stuff", verbose = FALSE),
               "'searchtype' must be one of")
  expect_error(get_natservid("clam", ask = 4),
               "ask must be of class logical")

  # rows param
  expect_error(get_natservid('Ruby*', 'common', rows = "foobar", verbose = FALSE),
               "rows must be of class numeric, integer")
  expect_error(get_natservid('Ruby*', 'common', rows = 0, verbose = FALSE),
               "rows > 0 is not TRUE")
})
