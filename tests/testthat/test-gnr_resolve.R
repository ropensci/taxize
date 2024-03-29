context("gnr_resolve")

test_that("gnr_resolve returns the correct value", {
  skip_on_cran()

  tmp <- gnr_resolve(c("Helianthus annuus", "Homo sapiens"))

  expect_equal(NCOL(tmp), 5)

	expect_is(tmp, "data.frame")
	expect_is(tmp$matched_name, "character")
})

test_that("best_match_only works correctly", {
  skip_on_cran()

  x <- 'Aconitum degeni subsp. paniculatum'
  a <- gnr_resolve(x, best_match_only = TRUE)
  b <- gnr_resolve(x, best_match_only = FALSE)

  expect_is(a, "data.frame")
  expect_is(b, "data.frame")
  expect_equal(NROW(a), 0)
  expect_equal(attributes(a)$not_known, x)
  expect_true(all(c("names", "row.names", "class", "not_known") %in% names(attributes(a))))
  expect_is(b$data_source_title, "character")

  ## same order as user supplied
  cc <- gnr_resolve(c("Homo sapiens", "Helianthus annuus"), best_match_only = TRUE)
  expect_identical(cc$user_supplied_name, c("Homo sapiens", "Helianthus annuus"))
})

test_that("canonical works correctly", {
  skip_on_cran()

  # x = a canse where no canonical names is found
  x <- gnr_resolve("Metzgeria", data_source_ids = c(12), canonical = TRUE)
  y <- "Helianthus annuus"
  w <- gnr_resolve(y, canonical = TRUE)
  z <- gnr_resolve(y, canonical = FALSE)

  expect_is(w, "data.frame")
  expect_is(z, "data.frame")
  expect_named(w, c("user_supplied_name", "submitted_name", "data_source_title", "score", "matched_name2"))
  expect_named(z, c("user_supplied_name", "submitted_name", "matched_name", "data_source_title", "score"))
  expect_equal(NROW(x), 1)
  expect_true(is.na(x$matched_name2[2]))
})

test_that("fields parameter works correctly", {
  skip_on_cran()

  tmp1 <- gnr_resolve(c("Asteraceae", "Plantae"), fields = 'all')
  tmp2 <- gnr_resolve(c("Asteraceae", "Plantae"), fields = 'minimal')

  expect_is(tmp1, "data.frame")
  expect_is(tmp1$matched_name, "character")
  expect_true(any(grepl("Asteraceae", tmp1$matched_name)))
  expect_is(tmp2, "data.frame")
  expect_is(tmp2$matched_name, "character")
  expect_true(any(grepl("Asteraceae", tmp2$matched_name)))
  expect_false(identical(tmp1$matched_name, tmp2$matched_name))

  expect_lt(NCOL(tmp2), NCOL(tmp1))
})

test_that("works correctly when no data found for preferred data source", {
  skip_on_cran()

  aa <- gnr_resolve("Scabiosa triandra", preferred_data_sources = c(21,24),
    best_match_only = TRUE)

  expect_is(aa, "data.frame")
  expect_equal(NROW(aa), 0)
  expect_equal(length(attributes(aa)$not_known), 1)
})
