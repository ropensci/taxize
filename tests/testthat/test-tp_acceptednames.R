# tests for tp_acceptednames fxn in taxize
context("tp_acceptednames")

test_that("tp_acceptednames returns the correct class", {
	expect_that(tp_acceptednames(id = 25503923, output = 'raw'), is_a("list"))
})
