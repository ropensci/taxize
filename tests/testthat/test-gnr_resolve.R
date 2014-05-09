# tests for gnr_resolve fxn in taxize
context("gnr_resolve")

tmp <- gnr_resolve(names = c("Helianthus annuus", "Homo sapiens"))

test_that("gnr_resolve returns the correct value", {
	expect_that(ncol(tmp[[1]]), equals(4))
})

test_that("gnr_resolve returns the correct class", {
	expect_is(tmp, "list")
	expect_is(tmp[[1]], "data.frame")
	expect_is(tmp[[1]]$matched_name, "character")
	expect_null(tmp[[2]])
})
