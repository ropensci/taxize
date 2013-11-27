# tests for tp_accnames fxn in taxize
context("tp_accnames")

test_that("tp_accnames returns the correct class", {
	expect_that(tp_accnames(id = 25503923, output = 'raw'), is_a("list"))
})
