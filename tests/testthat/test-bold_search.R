test_that("bold_search returns the correct value, dimensions, and classes", {
  skip_on_cran()
  vcr::use_cassette("bold_search", {
    a <- bold_search(sci = "Apis")
    b <- bold_search(sci = "Aga", fuzzy = TRUE)
    c <- bold_search(sci = c("Apis", "Puma concolor"))
    d <- bold_search(id = 88899)
  })

	expect_equal(names(a)[2], "taxid")
	expect_that(a$taxon, equals("Apis"))

  expect_equal(NROW(a), 1)
  expect_gt(NROW(b), 10)
  expect_gt(NROW(c), 1)
  expect_equal(NROW(d), 1)

	expect_is(a, "data.frame")
	expect_is(b, "data.frame")

	expect_that(a$tax_rank, is_a("character"))
	expect_that(d$parentname, is_a("character"))
})

test_that("bold_search is robust to user error", {
  skip_on_cran()

  expect_is(bold_search(sci = "asdfsdf"), "data.frame")
  expect_is(bold_search(sci = ""), "data.frame")
  expect_is(bold_search(id = "asdfsdf"), "data.frame")
  expect_error(bold_search())
})
