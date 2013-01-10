# tests for eol_dataobjects fxn in taxize
context("eol_dataobjects")

temp <- eol_dataobjects(id="d72801627bf4adf1a38d9c5f10cc767f")

test_that("eol_dataobjects returns the correct value", {
	expect_that(temp[[1]], equals(1045608))
})

test_that("eol_dataobjects returns the correct class", {
	expect_that(temp, is_a("list"))
})
