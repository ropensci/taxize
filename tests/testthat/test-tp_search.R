# tests for tp_search fxn in taxize
context("tp_search")

ttt <- tp_search(name = 'Poa annua')
uuu <- tp_search(name = 'stuff things')

test_that("tp_search returns the correct class", {
	expect_that(ttt, is_a("data.frame"))
})


test_that("tp_search returns a data.frame with one raw starting none found", {
  expect_that(uuu, is_a("data.frame"))
  expect_that(names(uuu), equals("error"))
  expect_that(as.character(uuu[1,1]), equals("No names were found"))
})
