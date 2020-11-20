test_that("get_nbn", {
  skip_on_cran()

  vcr::use_cassette("get_nbn", {
    x <- get_nbn(sci_com='Poa annua', messages=FALSE)
  })
    
  expect_is(x, c("txid", "taxa_taxon", "nbn"))
  expect_null(names(x))
  expect_is(taxa2::tax_db(x), "taxa_taxon_db")
  expect_equal(as.character(taxa2::tax_db(x)), "nbn")
})

test_that("get_nbn accepts ask-argument", {
  skip_on_cran()
  
  vcr::use_cassette("get_nbn_ask_arg", {
    x <- get_nbn('howdy', ask = FALSE, messages=FALSE)
  })

  expect_true(is.na(x))
})

test_that("get_nbn fails well", {
  skip_on_cran()

  expect_error(get_nbn(), "argument \"sci_com\" is missing")
  expect_error(get_nbn("clam", ask = 4),
               "ask must be of class logical")
  expect_error(get_nbn("clam", rank=5),
               "rank must be of class character")
  expect_error(get_nbn("clam", rows="stuff"),
               "rows must be of")
  expect_error(get_nbn("clam", rec_only="stuff"),
               "rec_only must be of")
})
