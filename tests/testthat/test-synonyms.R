# tests for synonyms fxn in taxize
context("synonyms")

tt <- synonyms("Poa annua", db="itis", verbose=FALSE)

test_that("synonyms returns the correct value", {
	expect_that(names(tt), matches("Poa annua"))
	expect_that(tt[[1]][1, "syn_name"], matches("Poa annua var. aquatica"))
})

test_that("synonyms returns the correct class", {
	expect_that(tt, is_a("list"))
	expect_that(tt[[1]], is_a("data.frame"))

	expect_that(dim(tt[[1]]), equals(c(11,4)))
})
