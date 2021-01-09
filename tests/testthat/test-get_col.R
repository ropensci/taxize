context("get_col")

test_that("get_col returns the correct value", {
  skip_on_cran()
  vcr::use_cassette("get_col", {
    z <- get_col(c("Chironomus riparius", "aaasdfadsfasdf"), 
      messages = FALSE)
  }, preserve_exact_body_bytes = TRUE)

  expect_is(z, "col")
  expect_false(is.na(z[1]))
  expect_true(is.na(z[2]))
})

test_that("get_col accepts ask-argument", {
  skip_on_cran()
  vcr::use_cassette("get_col_ask_arg", {
    a <- sw(get_col('Dugesia', ask = FALSE, messages = FALSE))
  })
  expect_true(is.na(a))
})

test_that("get_col fails as expected", {
  skip_on_cran()

  expect_error(get_col(), "argument \"sci\" is missing")
  expect_error(get_col('Poa annua', ask = 4, messages = FALSE),
               "ask must be of class logical")
  
  expect_error(
    get_col("Satyrium", minRank = 5, messages = FALSE),
    "minRank must be of class character")

  # rows param
  expect_error(get_col("Satyrium", rows = "foobar", messages = FALSE),
               "rows must be of class numeric, integer")
  expect_error(get_col("Satyrium", rows = 0, messages = FALSE),
               "all\\(rows > 0\\) is not TRUE")
})

test_that("get_col works with state input", {
  skip_on_cran()
  
  taxon_clear()
  
  # species list
  # spp <- names_list("species", size = 3)
  spp <- c("Ilex zygophylla", "Astronia shungolensis", "Uromorus anthopophagorum")
  xx <- taxon_last()
  # make sure last taxon is null
  expect_null(xx)

  vcr::use_cassette("get_col_state", {
    res <- get_col(spp, messages = FALSE)
  }, preserve_exact_body_bytes = TRUE)

  # now it's not NULL
  expect_is(taxon_last(), "taxon_state")

  # & get_col output is all good
  expect_is(res, "col")

  # passing taxon_last to get_col: already done, so should be identical
  expect_identical(res, get_col(taxon_last()))
})
