# tests for tp_namereferences fxn in taxize
context("tp_namereferences")

ttt <- tp_namereferences(id = 25509881, verbose=FALSE)

test_that("tp_namereferences returns the correct class", {
	expect_that(ttt, is_a("data.frame"))
})
