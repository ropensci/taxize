# tests for col_children fxn in taxize
context("col_children")

temp <- col_children(name="Apis")

test_that("col_children returns the correct value", {
	expect_that(temp[[1]][1,3], matches("Species"))
})

test_that("col_children returns the correct class", {
	expect_that(temp, is_a("list"))
	expect_that(temp[[1]], is_a("data.frame"))
})
