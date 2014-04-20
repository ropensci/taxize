# tests for eol_dataobjects fxn in taxize
context("eol_dataobjects")

temp <- eol_dataobjects(id="d72801627bf4adf1a38d9c5f10cc767f", verbose=FALSE)

test_that("eol_dataobjects returns the correct dimensions", {
	expect_equal(length(temp), 8)
	expect_equal(length(temp$dataObjects[[1]]), 22)
})

test_that("eol_dataobjects returns the correct class", {
	expect_that(temp, is_a("list"))
	expect_that(temp$scientificName, is_a("character"))
	expect_that(temp$identifier, is_a("numeric"))
})
