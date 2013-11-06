# tests for itis fxn in taxize
context("tnrs")

mynames <- c("Panthera tigris", "Eutamias minimus")
out <- tnrs(query = mynames)

test_that("tnrs returns the correct value", {
	expect_that(ncol(out), equals(7))
})

test_that("tnrs returns the correct class", {
	expect_that(out, is_a("data.frame"))
})
