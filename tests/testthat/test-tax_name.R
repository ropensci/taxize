# tests for tax_name fxn in taxize
context("tax_name")

test_that("tax_name returns the correct class", {
  skip_on_cran()

  tmp_ncbi  <- tax_name(query = "Baetis", get = c("family", "order"),
                        db = "ncbi", verbose=FALSE)
  tmp_ncbi2 <- tax_name(query = c("Helianthus annuus", 'Baetis rhodani'),
                        get = c("genus", "kingdom"), db = "ncbi", verbose=FALSE)
  tmp_na2 <- tax_name(query=c("Helianthus annuus", 'xxxx'),
                      get=c("family", "order"), db="ncbi", verbose=FALSE)
  tmp_na3 <- sw(tax_name(query = c("Helianthus annuus", 'xxxx'), rows = 1,
                      get = c("family", "order"), db="itis", verbose = FALSE))

	expect_is(tmp_ncbi, "data.frame")
	expect_is(tmp_na2, "data.frame")
	expect_is(tmp_na3, "data.frame")
  expect_equal(ncol(tmp_ncbi), 4)
  expect_equal(ncol(tmp_na2), 4)
  expect_equal(ncol(tmp_na3), 4)

	expect_equal(tmp_ncbi$family, "Baetidae")
	expect_equal(tmp_ncbi$order, "Ephemeroptera")
	expect_true(any(is.na(tmp_na2[2, ])))
	expect_true(any(is.na(tmp_na3[2, ])))

  expect_that(nrow(tmp_ncbi2), equals(2))
})

test_that("tax_name works with ncbi and rows arg", {
  skip_on_cran()
  
  aa <- tax_name('Bacillus', c("family", "order"), db="ncbi", rows=1)
  expect_is(aa, "data.frame")
  expect_equal(NCOL(aa), 4)
  expect_equal(NROW(aa), 1)
  expect_named(aa, c("db", "query", "family", "order"))
})

test_that("tax_name accepts ask-argument", {
  skip_on_cran()

  expect_that(is.na(sw(tax_name(query = "Dugesia", get = "family", db = "ncbi",
                             ask = FALSE, verbose = FALSE))$family), is_true())
})

test_that("taxon with no data returned from classification() works", {
  skip_on_cran()

  aa <- sw(tax_name("foo bar", get = "species",
                                  rows = 1, messages = FALSE))
  expect_is(aa, "data.frame")
  expect_true(is.na(aa$species))

  bb <- sw(tax_name("Asterias helianthus", get = "genus", messages = FALSE))
  expect_is(bb, "data.frame")
  expect_true(is.na(bb$genus))
  expect_warning(tax_name("Asterias helianthus", get = "genus", messages = FALSE),
                 "rank requested")

  cc <- sw(tax_name("Stellonia helianthus", get = "genus", messages = FALSE))
  expect_is(cc, "data.frame")
  expect_true(is.na(cc$genus))
  expect_warning(tax_name("Stellonia helianthus", get = "genus", messages = FALSE),
                 "rank requested")
})
