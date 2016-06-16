# tests for tp_summary fxn in taxize
context("tp_summary")


test_that("tp_summary returns the correct value", {
  skip_on_cran()

  dat <- suppressMessages(tp_summary(id = 25509881))

	expect_that(names(dat)[[1]], matches(".id"))

	expect_is(dat, "data.frame")
	expect_equal(NCOL(dat), 22)
})
