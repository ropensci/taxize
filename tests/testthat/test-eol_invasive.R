# tests for eol_invasive fxn in taxize
context("eol_invasive")

gisd <- eol_invasive(name='Brassica oleracea', dataset='gisd', verbose=FALSE)
mineps <- eol_invasive(name='Ciona intestinalis', dataset='mineps', verbose=FALSE)

test_that("eol_invasive returns the correct value", {
	expect_that(gisd[1,1], equals('Brassica oleracea'))
	expect_that(gisd[1,"db"], equals("gisd"))
	expect_that(mineps[1,1], equals('Ciona intestinalis'))
	expect_that(mineps[1,"db"], equals("mineps"))
})

test_that("eol_invasive returns the correct dimensions", {
  expect_that(dim(gisd), equals(c(1,4)))
  expect_that(dim(mineps), equals(c(1,4)))
})

test_that("eol_invasive returns the correct class", {
	expect_that(gisd, is_a("data.frame"))
	expect_that(mineps, is_a("data.frame"))
})
