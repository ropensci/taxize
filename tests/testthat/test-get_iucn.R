test_that("get_iucn", {
  skip_on_cran()

  vcr::use_cassette("get_iucn", {
    x <- get_iucn(sci='Poa annua', messages=FALSE)
  })
    
  expect_is(x, c("txid", "taxa_taxon", "iucn"))
  expect_null(names(x))
  expect_is(taxa2::tax_db(x), "taxa_taxon_db")
  expect_equal(as.character(taxa2::tax_db(x)), "iucn")
})

test_that("get_iucn accepts ask-argument", {
  skip_on_cran()
  
  vcr::use_cassette("get_iucn_not_found", {
    x <- get_iucn('howdy', messages=FALSE)
  })

  expect_true(is.na(x))
})

test_that("get_iucn fails well", {
  skip_on_cran()

  expect_error(get_iucn(), "argument \"sci\" is missing")
})
