# tests for vascan_search fxn in taxize
context("vascan_search")

aa <- vascan_search(q = "Helianthus annuus")
bb <- vascan_search(q = c("Helianthus annuus", "Crataegus dodgei"), raw=TRUE)
splist <- names_list(rank='species', size=50)
cc <- vascan_search(q = splist)

test_that("vascan_search returns the correct class", {
  expect_that(aa, is_a("list"))
  expect_that(bb, is_a("character"))
  expect_that(cc, is_a("list"))
})

test_that("vascan_search returns the correct value", {
  expect_equal(aa$apiVersion, "0.1")  
  expect_equal(cc$results[[1]]$numMatches, 0)
})