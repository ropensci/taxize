# tests for tp_refs fxn in taxize
context("tp_refs")

ttt <- tp_refs(id = 25509881, verbose=FALSE)

test_that("tp_refs returns the correct class", {
	expect_that(ttt, is_a("data.frame"))
})
