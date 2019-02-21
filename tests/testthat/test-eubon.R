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

# vcr::use_cassette("eubon_search_fails", {
#   test_that("eubon_search fails well", {
#     expect_error(eubon_search("Salmo", 'asdfdf'),
#                  "Error 400 invalid value for request parameter 'providers'")

#     expect_error(eubon_search("Salmo", searchMode = "adfdf"),
#                  "Error 400 Bad Request")
#   })
# }, preserve_exact_body_bytes = TRUE)

test_that("eubon and eubon_search are aliases of each other", {
  skip_on_cran()

  expect_identical(eubon, eubon_search)
})

