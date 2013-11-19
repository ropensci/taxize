# tests for gisd_isinvasive fxn in taxize
context("gisd_isinvasive")

sp <- c("Carpobrotus edulis", "Rosmarinus officinalis")
out <- gisd_isinvasive(sp, simplify = TRUE, verbose=FALSE)

test_that("gisd_isinvasive returns the correct value", {
	expect_that(dim(out), equals(c(2,2)))
})

test_that("gisd_isinvasive returns the correct class", {
	expect_that(out, is_a("data.frame"))
})


