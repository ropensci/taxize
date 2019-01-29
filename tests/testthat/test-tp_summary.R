context("tp_summary")

test_that("tp_summary returns the correct value", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("tp_summary", {
    dat <- suppressMessages(tp_summary(id = 25509881))
  })

  if ("error" %in% names(dat)) skip("error in tp_summary call - skipping")

  expect_match(names(dat)[[1]], ".id")
	expect_is(dat, "data.frame")
	expect_gt(NCOL(dat), 10)
})
