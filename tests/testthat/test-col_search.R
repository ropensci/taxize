# tests for col_search fxn in taxize
context("col_search")

temp <- col_search(name="Apis")
two <- col_search(name=c("Apis","Puma concolor"))
byid <- col_search(id=11935941)

test_that("col_search returns the correct value", {
	expect_that(names(temp), equals("Apis"))
	expect_that(as.character(temp[[1]]$name), equals("Apis"))
  
	expect_that(names(temp), equals("Apis"))
	expect_that(as.character(temp[[1]]$name), equals("Apis"))
})

test_that("col_search returns the correct dimensions", {
  expect_that(is.null(dim(temp)), is_true())
  expect_that(dim(temp[[1]]), equals(c(1,10)))
  
  expect_that(length(two), equals(2))
  expect_that(dim(two[[1]]), equals(c(1,10)))
  expect_that(dim(do.call(rbind, lapply(two,data.frame))), equals(c(2,10)))
  expect_that(dim(byid[[1]]), equals(c(0,0)))
})

test_that("col_search returns the correct class", {
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