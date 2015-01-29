# tests for tp_dist fxn in taxize
context("tp_dist")

tt <- tp_dist(id = 25509881)

test_that("tp_dist returns the correct class", {
	expect_that(tt, is_a("list"))
	expect_that(tt[['location']], is_a("data.frame"))
	expect_that(tt[['reference']], is_a("data.frame"))
	expect_true(any(grepl("abbreviatedtitle", names(tt[['reference']]))))
})
