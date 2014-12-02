# tests for vascan_search fxn in taxize
context("vascan_search")

aa <- vascan_search(q = "Helianthus annuus")
bb <- vascan_search(q = c("Helianthus annuus", "Crataegus dodgei"), raw=TRUE)
splist <- names_list(rank='species', size=50)
cc <- vascan_search(q = splist)

test_that("vascan_search returns the correct class", {
  expect_is(aa, "list")
  expect_is(bb, "character")
  expect_is(cc, "list")
})

test_that("vascan_search returns the correct value", {
  expect_equal(aa[[1]]$searchedterm, "Helianthus annuus")
  expect_equal(cc[[1]]$nummatches, 0)
})
