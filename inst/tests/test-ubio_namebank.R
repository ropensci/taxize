# tests for ubio_namebank fxn in taxize
context("ubio_namebank")

test_that("ubio_namebank returns the correct value", {
	expect_that(ubio_namebank(searchName = 'elephant', sci = 1, vern = 0)[,1], 
							matches("6938660"))
})

test_that("ubio_namebank returns the correct class", {
	expect_that(ubio_namebank(searchName = 'elephant', sci = 1, vern = 0), 
							is_a("data.frame"))
	expect_that(ncol(ubio_namebank(searchName = 'Helianthus annuus', sci = 1, vern = 0)), 
							equals(8))
})
