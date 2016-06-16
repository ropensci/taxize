# tests for col_search fxn in taxize
context("col_search")


test_that("col_search returns the correct values, dimensions, and classes", {
  skip_on_cran()

  temp <- col_search(name = "Apis")
  two <- col_search(name = c("Apis","Puma concolor"))
  byid <- col_search(id = 11935941)

	expect_equal(names(temp), "Apis")
	expect_equal(temp[[1]]$name[1], "Apis")

	expect_equal(names(temp), "Apis")
	expect_equal(temp[[1]]$name[1], "Apis")

  expect_that(is.null(dim(temp)), is_true())
  expect_equal(NCOL(temp[[1]]), 10)

  expect_that(length(two), equals(2))
  expect_equal(NCOL(two[[1]]), 10)
  expect_equal(NCOL(do.call(rbind, lapply(two, data.frame))), 10)
  expect_that(dim(byid[[1]]), equals(c(0,0)))

	expect_that(temp, is_a("list"))
	expect_that(temp[[1]], is_a("data.frame"))

	expect_that(two, is_a("list"))
	expect_that(two[[1]], is_a("data.frame"))
})

test_that("col_search is robust to user error", {
  expect_is(col_search(name = "asdfsdf")[[1]], "data.frame")
  expect_is(col_search(name = "")[[1]], "data.frame")
  expect_is(col_search(id = "asdfsdf")[[1]], "data.frame")
  expect_is(col_search(), "list")
  expect_equal(length(col_search()), 0)
})
