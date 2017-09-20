# tests for get_uid fxn in taxize
context("get_uid")

test_that("get_uid returns the correct value", {
  skip_on_cran()

	expect_that(is.na(get_uid(c("Chironomus riparius", "aaa"), verbose=FALSE)[2]),
              is_true())
})

test_that("get_uid returns the correct class", {
  skip_on_cran()

	expect_that(get_uid(c("Chironomus riparius", "Chaetopteryx"), verbose=FALSE),
              is_a("uid"))
})

test_that("get_uid accepts ask-argument", {
  skip_on_cran()

  expect_that(is.na(
    suppressWarnings(get_uid('Dugesia', ask = FALSE, verbose=FALSE))),
              is_true())
})

test_that("get_uid query modifiers work", {
  skip_on_cran()

  ### w/ modifiers to the name
  mod1 <- get_uid(sciname = "Aratinga", verbose=FALSE)
  mod2 <- get_uid(sciname = "Aratinga", modifier = "Organism", rows = 1, verbose=FALSE)

  expect_is(mod1, "uid")
  expect_is(mod2, "uid")
  expect_equal(mod1[[1]], "12945")
  expect_equal(mod2[[1]], "1230190")

  ### w/ rank query
  rq1 <- get_uid(sciname = "Pinus", rank_query = "genus", verbose=FALSE)
  rq2 <- get_uid(sciname = "Pinus", rank_query = "subgenus", verbose=FALSE)

  expect_is(rq1, "uid")
  expect_is(rq2, "uid")
  expect_equal(rq1[[1]], "3337")
  expect_equal(rq2[[1]], "139271")
})

test_that("get_uid filtering works", {
  skip_on_cran()

  ### w/ division
  df1 <- get_uid(sciname = "Echinacea", division_filter = "eudicots", verbose=FALSE)
  df2 <- get_uid(sciname = "Echinacea", division_filter = "sea urchins", verbose=FALSE)

  expect_is(df1, "uid")
  expect_is(df2, "uid")
  expect_equal(df1[[1]], "53747")
  expect_equal(df2[[1]], "7674")

  ## Rank example
  rf1 <- get_uid(sciname = "Pinus", rank_filter = "genus", rows = 2, verbose=FALSE)
  rf2 <- get_uid(sciname = "Pinus", rank_filter = "subgenus", verbose=FALSE)

  expect_is(rf1, "uid")
  expect_is(rf2, "uid")
  expect_equal(rf1[[1]], "3337")
  expect_equal(rf2[[1]], "139271")
})

test_that("get_uid fails as expected", {
  skip_on_cran()

  expect_error(get_uid(), "argument \"sciname\" is missing")
  expect_error(get_uid("Satyrium", ask = 4, verbose = FALSE),
               "ask must be of class logical")
  expect_error(
    get_uid(sciname = "Aratinga acuticauda", modifier = 5,
            verbose = FALSE),
               "modifier must be of class character")
  expect_error(
    get_uid(sciname = "Pinus", rank_query = TRUE,
            verbose = FALSE),
    "rank_query must be of class character")
  expect_error(
    get_uid(sciname = "Echinacea", division_filter = 4,
            verbose = FALSE),
    "division_filter must be of class character")
  expect_error(
    get_uid(sciname = "Pinus", rank_filter = 34,
            verbose = FALSE),
    "rank_filter must be of class character")

  # rows param
  expect_error(get_uid("Achlya", rows = "foobar", verbose = FALSE),
               "'rows' must be numeric or NA")
  expect_error(get_uid("Achlya", rows = 0, verbose = FALSE),
               "'rows' value must be an integer 1 or greater")
})
