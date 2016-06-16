# tests for tp_refs fxn in taxize
context("tp_refs")


test_that("tp_refs returns the correct class", {
  skip_on_cran()

  ttt <- suppressMessages(tp_refs(id = 25509881))

	expect_that(ttt, is_a("data.frame"))
})
