# tests for fungorum fxns in taxize
context("fungorum")

test_that("fungorum - fg_name_search", {
  skip_on_cran()

  aa <- fg_name_search(q = "Gymnopus", limit = 2)
  bb <- fg_name_search(q = "Gymnopus")

  expect_is(aa, "data.frame")
  expect_is(bb, "data.frame")

  expect_true(any(grepl("authors", names(aa))))
  expect_equal(NROW(aa), 2)
  expect_equal(unique(aa$infraspecific_epithet), "Gymnopus")

  expect_true(any(grepl("authors", names(bb))))
  expect_equal(NROW(bb), 10)
  expect_equal(unique(bb$infraspecific_epithet)[1], "Gymnopus")
})

test_that("fungorum - fg_epithet_search", {
  skip_on_cran()

  aa <- fg_epithet_search(q = "phalloides", limit = 2)

  expect_is(aa, "data.frame")
  expect_true(any(grepl("authors", names(aa))))
  expect_equal(NROW(aa), 2)
  expect_equal(unique(aa$infraspecific_rank), "sp.")
})

test_that("fungorum - fg_name_by_key", {
  skip_on_cran()

  aa <- fg_name_by_key(17703)

  expect_is(aa, "data.frame")
  expect_true(any(grepl("pubplaceofpublication", names(aa))))
  expect_true(any(grepl("uuid", names(aa))))
  expect_equal(NROW(aa), 1)
})

test_that("fungorum - fg_name_full_by_lsid", {
  skip_on_cran()

  aa <- fg_name_full_by_lsid("urn:lsid:indexfungorum.org:names:81085")

  expect_is(aa, "character")
  expect_equal(length(aa), 1)
  expect_true(grepl("Omphalotaceae", aa))
  expect_true(grepl("1985", aa))
})

test_that("fungorum - fg_all_updated_names", {
  skip_on_cran()

  date <- as.numeric(gsub("-", "", as.character(Sys.Date()))) - 2
  aa <- fg_all_updated_names(date = date)

  expect_is(aa, "data.frame")
  expect_more_than(NROW(aa), 1)
  expect_match(aa[1,], "indexfungorum")

  date <- as.numeric(gsub("-", "", as.character(Sys.Date() + 1)))
  expect_equal(NROW(fg_all_updated_names(date = date)), 0)
})

test_that("fungorum - fg_deprecated_names", {
  skip_on_cran()

  date <- as.numeric(gsub("-", "", as.character(Sys.Date() - 30)))
  aa <- fg_deprecated_names(date = date)

  expect_is(aa, "data.frame")
  expect_more_than(NROW(aa), 1)
  expect_match(aa[1,1], "indexfungorum")

  date <- as.numeric(gsub("-", "", as.character(Sys.Date() + 1)))
  expect_equal(NROW(fg_deprecated_names(date = date)), 0)
})

test_that("fungorum - fg_author_search", {
  skip_on_cran()

  aa <- fg_author_search(q = "Fayod", limit = 2)

  expect_is(aa, "data.frame")
  expect_equal(NROW(aa), 2)
  expect_match(aa$authors, "Fayod")
})

