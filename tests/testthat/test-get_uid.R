# tests for get_uid fxn in taxize
context("get_uid")

test_that("get_uid returns the correct value", {
  skip_on_cran()

	expect_that(is.na(get_uid(c("Chironomus riparius", "aaa"), messages=FALSE)[2]),
              is_true())
})

test_that("get_uid returns the correct class", {
  skip_on_cran()

	expect_that(get_uid(c("Chironomus riparius", "Chaetopteryx"), messages=FALSE),
              is_a("uid"))
})

test_that("get_uid accepts ask-argument", {
  skip_on_cran()

  expect_that(is.na(
    suppressWarnings(get_uid('Dugesia', ask = FALSE, messages=FALSE))),
              is_true())
})

test_that("get_uid query modifiers work", {
  skip_on_cran()

  ### w/ modifiers to the name
  mod1 <- get_uid(sciname = "Aratinga", messages=FALSE)
  mod2 <- get_uid(sciname = "Aratinga", modifier = "Organism", rows = 1, messages=FALSE)

  expect_is(mod1, "uid")
  expect_is(mod2, "uid")
  expect_equal(mod1[[1]], "12945")
  expect_equal(mod2[[1]], "1230190")

  ### w/ rank query
  rq1 <- get_uid(sciname = "Pinus", rank_query = "genus", messages=FALSE)
  rq2 <- get_uid(sciname = "Pinus", rank_query = "subgenus", messages=FALSE)

  expect_is(rq1, "uid")
  expect_is(rq2, "uid")
  expect_equal(rq1[[1]], "3337")
  expect_equal(rq2[[1]], "139271")
})

test_that("get_uid filtering works", {
  skip_on_cran()

  ### w/ division
  df1 <- get_uid(sciname = "Echinacea", division_filter = "eudicots", messages=FALSE)
  df2 <- get_uid(sciname = "Echinacea", division_filter = "sea urchins", messages=FALSE)

  expect_is(df1, "uid")
  expect_is(df2, "uid")
  expect_equal(df1[[1]], "53747")
  expect_equal(df2[[1]], "7674")

  ## Rank example
  rf1 <- get_uid(sciname = "Pinus", rank_filter = "genus", rows = 2, messages=FALSE)
  rf2 <- get_uid(sciname = "Pinus", rank_filter = "subgenus", messages=FALSE)

  expect_is(rf1, "uid")
  expect_is(rf2, "uid")
  expect_equal(rf1[[1]], "3337")
  expect_equal(rf2[[1]], "139271")
})

test_that("get_uid fails as expected", {
  skip_on_cran()

  expect_error(get_uid(), "argument \"sciname\" is missing")
  expect_error(get_uid("Satyrium", ask = 4, messages = FALSE),
               "ask must be of class logical")
  expect_error(
    get_uid(sciname = "Aratinga acuticauda", modifier = 5,
            messages = FALSE),
               "modifier must be of class character")
  expect_error(
    get_uid(sciname = "Pinus", rank_query = TRUE,
            messages = FALSE),
    "rank_query must be of class character")
  expect_error(
    get_uid(sciname = "Echinacea", division_filter = 4,
            messages = FALSE),
    "division_filter must be of class character")
  expect_error(
    get_uid(sciname = "Pinus", rank_filter = 34,
            messages = FALSE),
    "rank_filter must be of class character")

  # rows param
  expect_error(get_uid("Achlya", rows = "foobar", messages = FALSE),
               "'rows' must be numeric or NA")
  expect_error(get_uid("Achlya", rows = 0, messages = FALSE),
               "'rows' value must be an integer 1 or greater")
})
