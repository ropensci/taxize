context("get_ncbi")

test_that("get_ncbi returns the correct value", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("get_ncbi", {
    x <- get_ncbi(c("Chironomus riparius", "aaa"), messages = FALSE)
    z <- get_ncbi(c("Chironomus riparius", "Chaetopteryx"),
      messages = FALSE)
  })
  expect_true(is.na(x[2]))
  expect_is(z, "ncbi")
})

test_that("get_ncbi accepts ask-argument", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("get_ncbi_ask_arg", {
    x <- sw(get_ncbi("Dugesia", ask = FALSE, messages = FALSE))
  })
  expect_true(is.na(x))
})

test_that("get_ncbi query modifiers work", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("get_ncbi_name_modifiers", {
    mod1 <- get_ncbi(sci_com = "Aratinga", messages = FALSE)
    mod2 <- get_ncbi(sci_com = "Aratinga", modifier = "Organism",
      rows = 1, messages = FALSE)
  })

  expect_is(mod1, "ncbi")
  expect_is(mod2, "ncbi")
  expect_equal(txidac(mod1), "12945")
  expect_equal(txidac(mod2), "1230190")

  vcr::use_cassette("get_ncbi_rank_modifiers", {
    rq1 <- get_ncbi(sci_com = "Bombus", rank_query = "genus", messages = FALSE)
    rq2 <- get_ncbi(sci_com = "Bombus", rank_query = "subgenus", messages = FALSE)
  })

  expect_is(rq1, "ncbi")
  expect_is(rq2, "ncbi")
  expect_equal(txidac(rq1), "28641")
  expect_equal(txidac(rq2), "144708")
})

test_that("get_ncbi filtering works", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("get_ncbi_filtering_division", {
    df1 <- get_ncbi(sci_com = "Echinacea", division_filter = "eudicots",
      messages = FALSE)
    df2 <- get_ncbi(sci_com = "Echinacea", division_filter = "sea urchins",
      messages = FALSE)
  })

  expect_is(df1, "ncbi")
  expect_is(df2, "ncbi")
  expect_equal(txidac(df1), "53747")
  expect_equal(txidac(df2), "7674")

  vcr::use_cassette("get_ncbi_filtering_rank", {
    rf1 <- get_ncbi(sci_com = "Bombus", rank_filter = "genus", rows = 2,
      messages = FALSE)
    rf2 <- get_ncbi(sci_com = "Bombus", rank_filter = "subgenus",
      messages = FALSE)
  })

  expect_is(rf1, "ncbi")
  expect_is(rf2, "ncbi")
  expect_equal(txidac(rf1), "28641")
  expect_equal(txidac(rf2), "144708")
})

test_that("get_ncbi fails as expected", {
  skip_on_cran()

  expect_error(get_ncbi(), "argument \"sci_com\" is missing")
  expect_error(get_ncbi("Satyrium", ask = 4, messages = FALSE),
               "ask must be of class logical")
  expect_error(
    get_ncbi(sci_com = "Aratinga acuticauda", modifier = 5,
            messages = FALSE),
               "modifier must be of class character")
  expect_error(
    get_ncbi(sci_com = "Pinus", rank_query = TRUE,
            messages = FALSE),
    "rank_query must be of class character")
  expect_error(
    get_ncbi(sci_com = "Echinacea", division_filter = 4,
            messages = FALSE),
    "division_filter must be of class character")
  expect_error(
    get_ncbi(sci_com = "Pinus", rank_filter = 34,
            messages = FALSE),
    "rank_filter must be of class character")

  # rows param
  expect_error(get_ncbi("Achlya", rows = "foobar", messages = FALSE),
               "rows must be of class numeric, integer")
  expect_error(get_ncbi("Achlya", rows = 0, messages = FALSE),
               "all\\(rows > 0\\) is not TRUE")
})
