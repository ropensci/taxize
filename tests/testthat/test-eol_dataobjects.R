context("eol_dataobjects")

test_that("eol_dataobjects with taxonomy TRUE", {
  skip_on_cran()

  temp <- sm(eol_dataobjects(id = "7561533", verbose = FALSE))

  expect_is(temp, "list")
	expect_is(temp$taxonconcepts$scientificname, "character")
	expect_is(temp$taxonconcepts$identifier, "integer")
	expect_is(temp$taxonconcepts, "data.frame")
})

test_that("eol_dataobjects with taxonomy FALSE", {
  skip_on_cran()

	temp2 <- sm(eol_dataobjects(id = "7561533", 
    taxonomy = FALSE, verbose = FALSE))

	expect_is(temp2, "list")
	expect_null(temp2$taxonconcepts$scientificname)
	expect_null(temp2$taxonconcepts$identifier)
})

test_that("eol_dataobjects fails well", {
  skip_on_cran()

  expect_error(eol_dataobjects(), "argument \"id\" is missing")
})
