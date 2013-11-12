# tests for ubio_classification_search fxn in taxize
context("ubio_classification_search")

out <- ubio_classification_search(namebankID = 3070378)

test_that("ubio_classification_search returns the correct value", {
	expect_that(as.numeric(as.character(out[ , 1])), matches("6938660"))
})

test_that("ubio_classification_search returns the correct class", {
	expect_that(out, is_a("data.frame"))
	expect_that(ncol(out), equals(8))
})
