# tests for itis fxn in taxize
context("itis")

test_that("getacceptednamesfromtsn works", {
  aa <- getacceptednamesfromtsn(208527)

  expect_is(aa, "list")
	expect_equal(aa$submittedtsn, 208527)
	expect_true(is.na(aa$acceptedname))
})

test_that("itis returns the correct class", {
  bb <- gettaxonomicusagefromtsn(526852)

  expect_equal(NCOL(bb), 2)
	expect_is(bb, "data.frame")
})
