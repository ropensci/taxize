# tests for bold_search fxn in taxize
context("bold_search")

test_that("col_search returns the correct value, dimensions, and classes", {
  skip_on_cran()

  a <- bold_search(name="Apis")
  b <- bold_search(name="Aga", fuzzy=TRUE)
  c <- bold_search(name=c("Apis","Puma concolor"))
  d <- bold_search(id=88899)

	expect_equal(names(a)[1], "taxid")
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

  expect_is(bold_search(name = "asdfsdf"), "data.frame")
  expect_is(bold_search(name = ""), "data.frame")
  expect_is(bold_search(id = "asdfsdf"), "data.frame")
  expect_error(bold_search())
})
