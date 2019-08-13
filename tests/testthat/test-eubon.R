context("eubon_search")

test_that("eubon_search works", {
  vcr::use_cassette("eubon_search", {
    aa <- eubon_search("Prionus")
    bb <- eubon_search("Salmo", providers = 'worms')
    cc <- eubon_search("Salmo", providers = c('pesi', 'worms'))
  })

  expect_is(aa, "data.frame")
  expect_is(bb, "data.frame")
  expect_is(cc, "data.frame")

  expect_is(aa$otherNames, "list")
  expect_match(aa$taxon.taxonName.scientificName, "Prionus")
  expect_equal(NROW(aa), 1)

  expect_is(bb$checklistId, "character")
  expect_match(bb$taxon.taxonName.scientificName, "Salmo")
  expect_equal(NROW(bb), 1)

  expect_is(cc$taxon.accordingTo, "character")
  expect_match(cc$taxon.taxonName.scientificName, "Salmo")
  expect_equal(NROW(cc), 2)
})

test_that("eubon_search fails well", {
  expect_error(eubon_search("Prionus", limit = "asdf"), 
    "limit must be of class numeric, integer", class = "error")
  expect_error(eubon_search("Prionus", page = "asdf"), 
    "page must be of class numeric, integer", class = "error")
  expect_error(eubon_search("Prionus", timeout = "adf"), 
    "timeout must be of class numeric", class = "error")
  expect_error(eubon_search("Prionus", addParentTaxon = 5), 
    "addParentTaxon must be of class logical", class = "error")
})

# vcr::use_cassette("eubon_search_fails", {
#   test_that("eubon_search fails well", {
#     expect_error(eubon_search("Salmo", 'asdfdf'),
#                  "Error 400 invalid value for request parameter 'providers'")

#     expect_error(eubon_search("Salmo", searchMode = "adfdf"),
#                  "Error 400 Bad Request")
#   })
# }, preserve_exact_body_bytes = TRUE)

test_that("eubon and eubon_search are no longer aliases - eubon is defunct", {
  skip_on_cran()

  expect_error(eubon(), "use eubon_search", class = "error")
})

