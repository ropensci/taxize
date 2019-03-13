context("get_wormsid")

test_that("get_wormsid returns the correct value", {
  vcr::use_cassette("get_wormsid", {
    x <- sw(get_wormsid("Gadus morhua", rows = 1, messages = FALSE))
    z <- sw(get_wormsid(c("Platanista gangetica", "Lichenopora neapolitana"),
      rows = 1, messages = FALSE))
  })

  expect_is(x, "wormsid")
  expect_is(x[1], "character")
  expect_is(z, "wormsid")
  expect_equal(length(z), 2)
  expect_equal(sort(attr(z, "multiple_matches")), c(FALSE, TRUE))
  expect_false(all(attr(z, "pattern_match")))
  expect_match(attr(z, "uri"), "marinespecies.org")
})

test_that("get_wormsid accepts ask-argument", {
  vcr::use_cassette("get_wormsid_ask_arg", {
    x <- sw(get_wormsid("Platanista gangetica", ask = FALSE, messages = FALSE))
    z <- get_wormsid("asdasf", ask = FALSE, messages = FALSE)
  })

  expect_is(x, "wormsid")
  expect_true(is.na(z))
})

test_that("get_wormsid query modifiers work", {
  vcr::use_cassette("get_wormsid_query_modifiers", {
    mod2 <- sw(get_wormsid("asiatic clam", "common", messages = FALSE))
  })

  expect_is(mod2, "wormsid")
  expect_equal(mod2[1], "181580")
})

test_that("get_wormsid fails well", {
  vcr::use_cassette("get_wormsid_fail_with_rows_unknown", {
    expect_error(sw(get_wormsid("howdy", rows = 1, messages = FALSE)))
  })

  skip_on_cran()
  expect_error(get_wormsid(), "argument \"query\" is missing")
  expect_error(get_wormsid("clam", 5),
               "searchtype must be of class character")
  expect_error(get_wormsid("clam", "stuff", messages = FALSE),
               "'searchtype' must be one of")
  expect_error(get_wormsid("clam", accepted = 4),
               "accepted must be of class logical")
  expect_error(get_wormsid("clam", ask = 4),
               "ask must be of class logical")
})

test_that("get_wormsid exact match with more than 1 exact match found", {
  vcr::use_cassette("get_wormsid_multiple_exact_matches_found", {
    x <- get_wormsid("Gadus", messages = FALSE, rows = 1:2)
    1
  })

  expect_is(x, "wormsid")
  expect_is(x[1], "character")
  expect_equal(length(x), 1)
  expect_true(attr(x, "multiple_matches"))
  expect_false(attr(x, "pattern_match"))
})
