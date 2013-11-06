# tests for tax_name fxn in taxize
context("tax_name")

tmp_itis <- tax_name(query = "Baetis", get = c("family", "order"), db = "itis")
tmp_ncbi  <- tax_name(query = "Baetis", get = c("family", "order"), db = "ncbi")
tmp_ncbi2 <- tax_name(query = c("Helianthus annuus", 'Baetis rhodani'), get = c("genus", "kingdom"), db = "ncbi")
tmp_na2 <- tax_name(query=c("Helianthus annuus", 'xxxx'), get=c("family", "order"), db="ncbi")
tmp_na3 <- tax_name(query=c("Helianthus annuus", 'xxxx'), get=c("family", "order"), db="itis")
tmp_ncbi_both <- tax_name(query = c("Helianthus annuus", 'Baetis rhodani'), get = c("genus", "kingdom"), db = "both")

test_that("tax_name returns the correct class", {
	expect_that(tmp_itis, is_a("data.frame"))
	expect_that(tmp_ncbi, is_a("data.frame"))
	expect_that(tmp_na2, is_a("data.frame"))
	expect_that(tmp_na3, is_a("data.frame"))
	expect_that(tmp_ncbi_both, is_a("data.frame"))
	expect_that(ncol(tmp_itis), equals(2))
	expect_that(ncol(tmp_ncbi), equals(2))
	expect_that(ncol(tmp_na2), equals(2))
	expect_that(ncol(tmp_na3), equals(2))
	expect_that(ncol(tmp_ncbi_both), equals(2))
})

test_that("tax_name returns the correct value", {
	expect_equal(tmp_ncbi$family, "Baetidae")
	expect_equal(tmp_ncbi$order, "Ephemeroptera")
	expect_true(all(is.na(tmp_na2[2, ])))
	expect_true(all(is.na(tmp_na3[2, ])))
})

test_that("tax_name works with vectors", {
  expect_that(nrow(tmp_ncbi2), equals(2))
})

test_that("tax_name accepts ask-argument", {
  expect_that(is.na(tax_name(query = "Dugesia", get = "family", db = "ncbi", ask = FALSE)), is_true())
})


