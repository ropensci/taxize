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
  expect_is(aa[[1]]$matches[[1]]$taxonomicassertions, "data.frame")
})

test_that("vascan_search returns the correct dimensions", {
  expect_equal(NCOL(aa[[1]]$matches[[1]]$taxonomicassertions), 7)
})
