# tests for eol_dataobjects fxn in taxize
context("eol_dataobjects")

test_that("eol_dataobjects with taxonomy TRUE", {
  skip_on_cran()

  temp <- suppressMessages(eol_dataobjects(id = "d72801627bf4adf1a38d9c5f10cc767f", verbose = FALSE))

  expect_is(temp, "list")
	expect_is(temp$scientificname, "character")
	expect_is(temp$identifier, "integer")
	expect_is(temp$taxonconcepts, "data.frame")

})

test_that("eol_dataobjects with taxonomy FALSE", {
	# taxonomy=FALSE - gives no taxonconcepts data.frame
	temp2 <- suppressMessages(eol_dataobjects(id = "d72801627bf4adf1a38d9c5f10cc767f",
	                                         taxonomy = FALSE, verbose = FALSE))

	expect_is(temp2, "list")
	expect_is(temp2$scientificname, "character")
	expect_is(temp2$identifier, "integer")
	expect_null(temp2$taxonconcepts)
})
