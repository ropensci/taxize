# tests for eol_dataobjects fxn in taxize
context("eol_dataobjects")

temp <- eol_dataobjects(id="d72801627bf4adf1a38d9c5f10cc767f", verbose=FALSE)

test_that("eol_dataobjects returns the correct class", {
	expect_is(temp, "list")
	expect_is(temp$scientificName, "character")
	expect_is(temp$identifier, "integer")
})
