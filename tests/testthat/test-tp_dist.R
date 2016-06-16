# tests for tp_dist fxn in taxize
context("tp_dist")


test_that("tp_dist returns the correct class", {
  skip_on_cran()

  tt <- suppressMessages(tp_dist(id = 25509881))

	expect_that(tt, is_a("list"))
	expect_that(tt[['location']], is_a("data.frame"))
	expect_that(tt[['reference']], is_a("data.frame"))
	expect_true(any(grepl("abbreviatedtitle", names(tt[['reference']]))))
})
