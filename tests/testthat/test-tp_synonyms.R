context("tp_synonyms")

test_that("tp_synonyms returns the correct value", {
  skip_on_cran()

  dat <- suppressMessages(tp_synonyms(id = 25509881))

  expect_match(names(dat)[[1]], "accepted")

	expect_that(dat, is_a("list"))
	expect_that(dat[[1]], is_a("data.frame"))
	expect_that(dat[[2]], is_a("data.frame"))
	expect_that(ncol(dat[[2]]), equals(4))
})
