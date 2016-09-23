# tests for synonyms fxn in taxize
context("synonyms")


test_that("synonyms returns the correct value", {
  skip_on_cran()

  tt <- synonyms("Poa annua", db = "itis")

	expect_match(names(tt), "Poa annua")
	expect_match(tt[[1]][1, "syn_name"], "Poa annua var. aquatica")

	expect_is(tt, "list")
	expect_is(tt[[1]], "data.frame")

	expect_equal(dim(tt[[1]]), c(11, 5))
})
