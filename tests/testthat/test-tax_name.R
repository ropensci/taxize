# tests for tax_name fxn in taxize
context("tax_name")

tmp_itis <- tax_name(query = "Baetis", get = c("family", "order"),
                     db = "itis", verbose=FALSE)
tmp_ncbi  <- tax_name(query = "Baetis", get = c("family", "order"),
                      db = "ncbi", verbose=FALSE)
tmp_ncbi2 <- tax_name(query = c("Helianthus annuus", 'Baetis rhodani'),
                      get = c("genus", "kingdom"), db = "ncbi", verbose=FALSE)
tmp_na2 <- tax_name(query=c("Helianthus annuus", 'xxxx'),
                    get=c("family", "order"), db="ncbi", verbose=FALSE)
tmp_na3 <- tax_name(query=c("Helianthus annuus", 'xxxx'),
                    get=c("family", "order"), db="itis",verbose=FALSE)
tmp_ncbi_both <- tax_name(query = c("Helianthus annuus", 'Baetis rhodani'),
                          get = c("genus", "kingdom"), db = "both", verbose=FALSE)

test_that("tax_name returns the correct class", {
  expect_is(tmp_itis, "data.frame")
	expect_is(tmp_ncbi, "data.frame")
	expect_is(tmp_na2, "data.frame")
	expect_is(tmp_na3, "data.frame")
  expect_is(tmp_ncbi_both, "data.frame")
	expect_equal(ncol(tmp_itis), 4)
  expect_equal(ncol(tmp_ncbi), 4)
  expect_equal(ncol(tmp_na2), 4)
  expect_equal(ncol(tmp_na3), 4)
  expect_equal(ncol(tmp_ncbi_both), 4)
})

test_that("tax_name returns the correct value", {
	expect_equal(tmp_ncbi$family, "Baetidae")
	expect_equal(tmp_ncbi$order, "Ephemeroptera")
	expect_true(any(is.na(tmp_na2[2, ])))
	expect_true(any(is.na(tmp_na3[2, ])))
})

test_that("tax_name works with vectors", {
  expect_that(nrow(tmp_ncbi2), equals(2))
})

test_that("tax_name accepts ask-argument", {
  expect_that(is.na(tax_name(query = "Dugesia", get = "family", db = "ncbi",
                             ask = FALSE, verbose = FALSE)$family), is_true())
})


