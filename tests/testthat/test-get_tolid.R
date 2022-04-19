context("get_tolid")

test_that("get_tolid returns the correct value", {
  skip_on_cran()
  
  vcr::use_cassette("get_tolid", {
    x <- get_tolid("Quercus douglasii", messages = FALSE)
    z <- get_tolid(c("Chironomus", "Chaetopteryx"),
      messages = FALSE)
  })

  expect_is(x, "tolid")
  expect_is(x[1], "character")
  expect_match(attr(x, "uri"), "opentreeoflife")
  expect_equal(length(x), 1)

  expect_is(z, "tolid")
  expect_is(z[1], "character")
  expect_match(attr(z, "uri"), "opentreeoflife")
  expect_equal(length(z), 2)
})

test_that("get_tolid accepts ask-argument", {
  skip_on_cran()
  
  vcr::use_cassette("get_tolid_ask_arg", {
    x <- sw(get_tolid("Dugesia", ask = FALSE, messages = FALSE))
  })
  expect_true(is.na(x))
})

test_that("get_tolid fails as expected", {
  skip_on_cran()

  expect_error(get_tolid(), "argument \"sci\" is missing")
  expect_error(get_tolid("Satyrium", ask = 4, messages = FALSE),
               "ask must be of class logical")

  # rows param
  expect_error(get_tolid("Achlya", rows = "foobar", messages = FALSE),
               "rows must be of class numeric, integer")
  expect_error(get_tolid("Achlya", rows = 0, messages = FALSE),
               "rows > 0 is not TRUE")
})
