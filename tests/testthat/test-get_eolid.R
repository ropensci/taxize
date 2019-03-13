context("get_eolid")

test_that("get_eolid works", {
  skip_on_cran()

  a <- get_eolid(sciname = "Puma concolor", message = FALSE, rows = 1)

  expect_is(a, "eolid")
  expect_is(a[[1]], "character")
  expect_is(as.numeric(a[[1]]), "numeric")
  expect_is(attr(a, "pageid"), "character")
  expect_is(attr(a, "provider"), "character")
  expect_is(attr(a, "match"), "character")
  expect_true(attr(a, "multiple_matches"))
  expect_true(attr(a, "pattern_match"))
  expect_is(attr(a, "uri"), "character")

  Sys.sleep(3)
})

test_that("get_eolid returns the correct value", {
  skip_on_cran()

  expect_true(is.na(get_eolid(sciname = 'adsf asdf asdf',
    messages = FALSE)[[1]]))
  Sys.sleep(3)
})

test_that("get_eolid returns the correct class", {
  skip_on_cran()

  expect_that(get_eolid("adsf asdf asdf", messages=FALSE),
              is_a("eolid"))
  Sys.sleep(1)
})

Sys.sleep(1)

test_that("get_eolid accepts ask-argument", {
  skip_on_cran()

  expect_that(is.na(get_eolid(sciname = 'adsf asdf asdf', ask = FALSE,
    messages = FALSE)[[1]]),
              is_true())
})

test_that("get_eolid fails as expected", {
  skip_on_cran()

  expect_error(get_eolid(), "argument \"sciname\" is missing")
  expect_error(get_eolid("Poa annua", ask = 4, messages = FALSE),
               "ask must be of class logical")

  Sys.sleep(1)
  # rows param
  expect_error(get_eolid("Poa annua", rows = "foobar", messages = FALSE),
               "rows must be of class numeric, integer")
  expect_error(get_eolid("Poa annua", rows = 0, messages = FALSE),
               "all\\(rows > 0\\) is not TRUE")
})
