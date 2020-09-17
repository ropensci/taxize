context("get_uid")

test_that("get_uid returns the correct value", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("get_uid", {
    x <- get_uid(c("Chironomus riparius", "aaa"), messages = FALSE)
    z <- get_uid(c("Chironomus riparius", "Chaetopteryx"),
      messages = FALSE)
  })
  expect_true(is.na(x[2]))
  expect_is(z, "uid")
})

test_that("get_uid accepts ask-argument", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("get_uid_ask_arg", {
    x <- sw(get_uid("Dugesia", ask = FALSE, messages = FALSE))
  })
  expect_true(is.na(x))
})

test_that("get_uid query modifiers work", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("get_uid_name_modifiers", {
    mod1 <- get_uid(sci_com = "Aratinga", messages = FALSE)
    mod2 <- get_uid(sci_com = "Aratinga", modifier = "Organism",
      rows = 1, messages = FALSE)
  })

  expect_is(mod1, "uid")
  expect_is(mod2, "uid")
  expect_equal(mod1[[1]], "12945")
  expect_equal(mod2[[1]], "1230190")

  vcr::use_cassette("get_uid_rank_modifiers", {
    rq1 <- get_uid(sci_com = "Bombus", rank_query = "genus", messages = FALSE)
    rq2 <- get_uid(sci_com = "Bombus", rank_query = "subgenus", messages = FALSE)
  })

  expect_is(rq1, "uid")
  expect_is(rq2, "uid")
  expect_equal(rq1[[1]], "28641")
  expect_equal(rq2[[1]], "144708")
})

test_that("get_uid filtering works", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("get_uid_filtering_division", {
    df1 <- get_uid(sci_com = "Echinacea", division_filter = "eudicots",
      messages = FALSE)
    df2 <- get_uid(sci_com = "Echinacea", division_filter = "sea urchins",
      messages = FALSE)
  })

  expect_is(df1, "uid")
  expect_is(df2, "uid")
  expect_equal(df1[[1]], "53747")
  expect_equal(df2[[1]], "7674")

  vcr::use_cassette("get_uid_filtering_rank", {
    rf1 <- get_uid(sci_com = "Bombus", rank_filter = "genus", rows = 2,
      messages = FALSE)
    rf2 <- get_uid(sci_com = "Bombus", rank_filter = "subgenus",
      messages = FALSE)
  })

  expect_is(rf1, "uid")
  expect_is(rf2, "uid")
  expect_equal(rf1[[1]], "28641")
  expect_equal(rf2[[1]], "144708")
})

test_that("get_uid fails as expected", {
  skip_on_cran()

  expect_error(get_uid(), "argument \"sci_com\" is missing")
  expect_error(get_uid("Satyrium", ask = 4, messages = FALSE),
               "ask must be of class logical")
  expect_error(
    get_uid(sci_com = "Aratinga acuticauda", modifier = 5,
            messages = FALSE),
               "modifier must be of class character")
  expect_error(
    get_uid(sci_com = "Pinus", rank_query = TRUE,
            messages = FALSE),
    "rank_query must be of class character")
  expect_error(
    get_uid(sci_com = "Echinacea", division_filter = 4,
            messages = FALSE),
    "division_filter must be of class character")
  expect_error(
    get_uid(sci_com = "Pinus", rank_filter = 34,
            messages = FALSE),
    "rank_filter must be of class character")

  # rows param
  expect_error(get_uid("Achlya", rows = "foobar", messages = FALSE),
               "rows must be of class numeric, integer")
  expect_error(get_uid("Achlya", rows = 0, messages = FALSE),
               "all\\(rows > 0\\) is not TRUE")
})
