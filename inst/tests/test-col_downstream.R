# tests for col_downstream fxn in taxize
context("col_downstream")

data(rank_ref)
temp <- col_downstream(name="Apis", downto="Species")

test_that("col_downstream returns the correct value", {
	expect_that(as.character(temp[[1]][1,2]), equals("Apis andreniformis"))
})

test_that("col_downstream returns the correct class", {
	expect_that(temp, is_a("list"))
})
