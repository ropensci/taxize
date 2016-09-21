context("tol_resolve")

test_that("tol_resolve basic usage works", {
  skip_on_cran()

  xx <- c("echinodermata", "xenacoelomorpha", "chordata", "hemichordata")
  tmp <- tol_resolve(names = xx)

  expect_is(tmp, "data.frame")
  expect_is(tmp$search_string, "character")
  expect_is(tmp$number_matches, "character")
  expect_equal(NROW(tmp), 4)

  # query gives back expected output
  expect_equal(
    xx,
    tolower(tmp$unique_name)
  )
})

test_that("context_name works correctly", {
  skip_on_cran()

  aa <- tol_resolve(c("Hyla", "Salmo", "Diadema", "Nautilus"),
              context_name = "Animals")
  expect_is(aa, "data.frame")
  expect_is(aa$search_string, "character")
  expect_is(aa$number_matches, "character")

  # errors when not in accepted set
  expect_error(
    tol_resolve(c("Hyla", "Salmo", "Diadema", "Nautilus"),
                context_name = "stuff"),
    "is not valid. Check possible values using tnrs_contexts"
  )
})

test_that("do_approximate_matching works correctly", {
  skip_on_cran()

  aa <- tol_resolve("Nautilas", do_approximate_matching = TRUE)
  expect_is(aa, "data.frame")
  expect_equal(aa$unique_name, "Nautilus")

  expect_error(
    tol_resolve("Nautilas", do_approximate_matching = FALSE),
    "No matches for any of the provided taxa"
  )
})

test_that("fails well", {
  expect_error(tol_resolve(), "You must supply")
  expect_error(tol_resolve(ids = 5), "You must supply a ‘names’ argument")
  expect_error(tol_resolve(context_name = "stuff"), "is not valid")
  expect_error(tol_resolve(names = 5), "Argument ‘names’ must be of class")
})

test_that("fails well - HTTP needed", {
  skip_on_cran()

  expect_error(tol_resolve(c("Hyla", "Salmo", "Diadema", 5)),
               "HTTP failure")
})
