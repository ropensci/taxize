# tests for col_downstream fxn in taxize
context("col_downstream")

temp <- col_downstream(name="Apis", downto="Species")

test_that("col_downstream returns the correct value", {
	expect_that(temp[[1]][1,2], matches("Apis andreniformis"))
})

test_that("col_downstream returns the correct class", {
	expect_that(temp, is_a("list"))
})
