context("tax_rank")

test_that("tax_rank returns the correct class", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("tax_rank", {
    A <- suppressMessages(tax_rank(c("Helianthus annuus", "Baetis"),
      db = "ncbi", messages = FALSE))
    B <- suppressMessages(sw(tax_rank("Helianthus", db = "itis",
      messages = FALSE, rows = 1)))
    C <- suppressMessages(tax_rank(c("Helianthus annuus", "xxxxxx"),
      db = "ncbi", messages = FALSE))
  })

  expect_is(A, "list")
  expect_is(B, "list")
  expect_is(C, "list")
  expect_equal(names(C), c("Helianthus annuus", "xxxxxx"))

  expect_equal(A$`Helianthus annuus`, "species")
  expect_true(is.na(C$xxxxxx))

  expect_equal(length(A), 2)
  expect_equal(length(C), 2)
})

test_that("works with get_*() input", {
  skip_on_cran()
  vcr::use_cassette("tax_rank_get_star_input", {
    aa <- suppressMessages(tax_rank(get_boldid("Helianthus annuus")))
  })

  expect_is(aa, "list")
  expect_equal(names(aa), "421377")
  expect_equal(aa$`421377`, "genus")
})

test_that("tax_rank fails well", {
  skip_on_cran()

  expect_error(tax_rank(), "argument \"x\" is missing")
  expect_error(tax_rank("aadfd"), "Must specify db!")
  expect_error(tax_rank("Asdfadsf", db = "asdfd"),
               "the provided db value was not recognised")
  expect_error(tax_rank(NA, db = "itis"),
               "'db' not recognized")
})
