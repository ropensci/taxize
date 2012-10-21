# tests for tp_namereferences fxn in taxize
context("tp_namereferences")

tt <- tp_namereferences(id = 25509881, output = 'raw')
ttt <- tp_namereferences(id = 25509881)

test_that("tp_namereferences returns the correct class", {
	expect_that(tt, is_a("list"))
	expect_that(ttt, is_a("data.frame"))
})
