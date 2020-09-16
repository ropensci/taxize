context("get_worms")

test_that("get_worms returns the correct value", {
  skip_on_cran()
  vcr::use_cassette("get_worms", {
    x <- sw(get_worms("Gadus morhua", rows = 1, messages = FALSE))
    z <- sw(get_worms(c("Gadus morhua", "Lichenopora neapolitana"),
      rows = 1, messages = FALSE))
  })

  expect_is(x, "txid")
  expect_is(x, "worms")
  expect_is(z, "txid")
  expect_is(z, "worms")
  expect_equal(length(x), 1)
  expect_equal(length(z), 2)
  expect_equal(sort(txz_mm(z)), c(FALSE, TRUE))
  expect_false(all(txz_pm(z)))
  expect_match(txz_uri(z), "marinespecies.org")
})

test_that("get_worms accepts ask-argument", {
  skip_on_cran()
  vcr::use_cassette("get_worms_ask_arg", {
    x <- sw(get_worms("Platanista gangetica", ask = FALSE, messages = FALSE))
    z <- get_worms("asdasf", ask = FALSE, messages = FALSE)
  })

  expect_is(x, "worms")
  expect_is(z, "worms")
  expect_true(is.na(z))
})

test_that("get_worms searchtype param works", {
  skip_on_cran()
  vcr::use_cassette("get_worms_query_modifiers", {
    mod2 <- sw(get_worms("asiatic clam", "common", messages = FALSE))
  })

  expect_is(mod2, "worms")
  expect_equal(txidac(mod2), "181580")
})

test_that("get_worms marine_only param works", {
  skip_on_cran()
  vcr::use_cassette("get_worms_marine_only", {
    a <- sw(get_worms("Apedinella", marine_only = TRUE, accepted = TRUE, messages = FALSE))
    b <- sw(get_worms("Apedinella", marine_only = FALSE, messages = FALSE))
  })

  expect_is(a, "worms")
  expect_is(b, "worms")
  expect_equal(txidac(a), "248097")
  expect_equal(txidac(b), "248096")
})

test_that("get_worms fuzzy param works", {
  skip_on_cran()
  vcr::use_cassette("get_worms_fuzzy", {
    a <- sw(get_worms("Platypleu", fuzzy = TRUE, accepted = TRUE, messages = FALSE))
    b <- sw(get_worms("Platypleu", fuzzy = FALSE, messages = FALSE))
  })

  expect_is(a, "worms")
  expect_is(b, "worms")
  expect_equal(txidac(a), "105519")
  expect_true(is.na(b))
})

test_that("get_worms fails well", {
  skip_on_cran()
  vcr::use_cassette("get_worms_fail_with_rows_unknown", {
    expect_error(sw(get_worms("howdy", rows = 1, messages = FALSE)))
  })

  skip_on_cran()
  expect_error(get_worms(), "argument \"sci_com\" is missing")
  expect_error(get_worms("clam", 5, messages = FALSE),
               "searchtype must be of class character")
  expect_error(get_worms("clam", "stuff", messages = FALSE),
               "'searchtype' must be one of")
  expect_error(get_worms("clam", accepted = 4, messages = FALSE),
               "accepted must be of class logical")
  expect_error(get_worms("clam", ask = 4, messages = FALSE),
               "ask must be of class logical")
  expect_error(get_worms("clam", marine_only = 4, messages = FALSE),
               "marine_only must be of class logical")
  expect_error(get_worms("clam", fuzzy = 4, messages = FALSE),
               "fuzzy must be of class logical")
})

test_that("get_worms exact match with more than 1 exact match found", {
  skip_on_cran()
  vcr::use_cassette("get_worms_multiple_exact_matches_found", {
    x <- get_worms("Gadus", messages = FALSE, rows = 1:2)
    1
  })

  expect_is(x, "worms")
  expect_equal(length(x), 1)
  expect_true(txz_mm(x))
  expect_false(txz_pm(x))
})
