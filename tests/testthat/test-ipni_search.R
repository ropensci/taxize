context("ipni_search")

test_that("ipni_search works", {
  aa <- ipni_search(genus = 'Brintonia', isapnirecord = TRUE,
                    isgcirecord = TRUE, isikrecord = TRUE)
  bb <- ipni_search(genus = 'Ceanothus')
  cc <- ipni_search(genus = 'Pinus', species = 'contorta')

  expect_is(aa, "data.frame")
  expect_is(bb, "data.frame")
  expect_is(cc, "data.frame")
  expect_is(aa$id, "character")
  expect_is(aa$family, "character")

  expect_named(aa, c('id','version','family','full_name_without_family_and_authors','authors'))
  expect_is(aa$family, "character")
})

test_that("ipni_search works with different output formats", {
  aa <- ipni_search(genus = 'Ceanothus')
  bb <- ipni_search(genus = 'Ceanothus', output = 'short')
  cc <- ipni_search(genus = 'Ceanothus', output = 'classic')
  dd <- ipni_search(genus = 'Ceanothus', output = 'extended')

  expect_is(aa, "data.frame")
  expect_is(bb, "data.frame")
  expect_is(cc, "data.frame")
  expect_is(dd, "data.frame")

  expect_less_than(NCOL(aa), NCOL(bb))
  expect_less_than(NCOL(bb), NCOL(cc))
  expect_less_than(NCOL(aa), NCOL(cc))
  expect_less_than(NCOL(aa), NCOL(dd))
  expect_less_than(NCOL(cc), NCOL(dd))
})

test_that("ipni_search fails correctly", {
  expect_error(ipni_search(), "No results")
  expect_error(ipni_search(family = 5), "No results")
  expect_warning(ipni_search(genus = "adfasdfasffd"), "No data found")
})
