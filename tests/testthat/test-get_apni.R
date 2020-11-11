context("get_apni")

test_that("get_apni returns the correct value", {
  skip_on_cran()
  vcr::use_cassette("get_apni", {
    x <- get_apni("Acacia adfdadaasdf", messages = FALSE)
    z <- get_apni(c("Acacia", "Pinus"), messages = FALSE)
  })
  expect_is(x, "apni")
  expect_true(is.na(x))
  expect_is(z, "apni")
  expect_length(z, 2)
  expect_true(grepl("id.biodiversity.org", attr(z, "uri")[1]))
})

test_that("get_apni accepts ask-argument", {
  skip_on_cran()
  vcr::use_cassette("get_apni_ask_arg", {
    x <- sw(get_apni("Acacia a", ask = FALSE, messages = FALSE))
  })
  expect_true(is.na(x))
})

test_that("get_apni filtering works", {
  skip_on_cran()

  vcr::use_cassette("get_apni_filtering_rank", {
    rf1 <- get_apni(sci = "Acacia a", rank_filter = "species", rows = 2,
      messages = FALSE)
    rf2 <- get_apni(sci = "Acacia a", rank_filter = "subspecies", rows = 2,
      messages = FALSE)
  })

  expect_is(rf1, "apni")
  expect_is(rf2, "apni")
  expect_equal(rf1[[1]], "57932")
  expect_equal(rf2[[1]], "153411")
})

test_that("get_apni fails as expected", {
  skip_on_cran()

  expect_error(get_apni(), "argument \"sci\" is missing")
  expect_error(get_apni("Satyrium", ask = 4, messages = FALSE),
               "ask must be of class logical")
  expect_error(
    get_apni(sci = "Pinus", rank_filter = 34,
            messages = FALSE),
    "rank_filter must be of class character")

  # rows param
  expect_error(get_apni("Achlya", rows = "foobar", messages = FALSE),
               "rows must be of class numeric, integer")
  expect_error(get_apni("Achlya", rows = 0, messages = FALSE),
               "all\\(rows > 0\\) is not TRUE")
})
