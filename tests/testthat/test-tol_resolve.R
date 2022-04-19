context("tol_resolve")

test_that("tol_resolve basic usage works", {
  skip_on_cran()
  
  vcr::use_cassette("tol_resolve", {v
    xx <- c("echinodermata", "xenacoelomorpha", "chordata", "hemichordata")
    tmp <- tol_resolve(names = xx)
  })

  expect_is(tmp, "data.frame")
  expect_is(tmp$search_string, "character")
  expect_type(tmp$number_matches, "integer")
  expect_equal(NROW(tmp), 4)

  # query gives back expected output
  expect_equal(
    xx,
    tolower(tmp$unique_name)
  )
})

test_that("context_name works correctly", {
  skip_on_cran()

  vcr::use_cassette("tol_resolve_context_name", {
    aa <- tol_resolve(c("Hyla", "Salmo", "Diadema", "Nautilus"),
                context_name = "Animals")
  })

  expect_is(aa, "data.frame")
  expect_is(aa$search_string, "character")
  expect_type(aa$number_matches, "integer")

  # errors when not in accepted set
  expect_error(
    tol_resolve(c("Hyla", "Salmo", "Diadema", "Nautilus"),
                context_name = "stuff"),
    "is not valid. Check possible values using tnrs_contexts"
  )
})

test_that("do_approximate_matching works correctly", {
  skip_on_cran()

  vcr::use_cassette("tol_resolve_do_approximate_matching", {
    aa <- tol_resolve("Nautilas", do_approximate_matching = TRUE)
  })

  expect_is(aa, "data.frame")
  expect_equal(aa$unique_name, "Nautilus")

  # FIXME: bring back when new rotl ver on CRAN
  vcr::use_cassette("tol_resolve_do_approximate_matching_false", {
    expect_warning(
      tol_resolve("Nautilas", do_approximate_matching = FALSE),
      "not matched"
    )
  })
})

test_that("fails well", {
  skip_on_cran()
  
  expect_error(tol_resolve(), "You must supply")
  expect_error(tol_resolve(ids = 5), "You must supply a")
  expect_error(tol_resolve(context_name = "stuff"), "is not valid")
  expect_error(tol_resolve(names = 5), "must be of class")
})

